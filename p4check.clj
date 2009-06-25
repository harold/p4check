(import '(java.io File)
        '(java.lang ProcessBuilder)
        '(java.util.concurrent CountDownLatch))

;; Timer
(defn get-time [] (System/currentTimeMillis))
(def start-time (get-time))

;; Reporting
(def considered-file-count (ref 0))
(def touched-paths (ref []))

(defn p4Edit [path]
  (let [process (new ProcessBuilder ["p4" "edit" path])]
    (dosync (alter touched-paths conj path))
    (print ".")
    (flush)
    (.start process)))

(defn watchFilter [path]
  (let [extension (.substring path (+ 1 (.lastIndexOf path ".")))]
    (some #{extension} ["cpp" "cxx" "h" "xml" "inl" "c" "vcproj"
                        "sln" "js" "build" "hxx" "include" "hpp"])))

(defn maybe-p4Edit [file]
  (let [path (.getAbsolutePath file)]
    (dosync (alter considered-file-count inc))
    (when (.canWrite file) ; File is *not* read only.
      (when (watchFilter path)
        (p4Edit path)))))

(defn create-lazy-dir-sequence [base-java-file]
  (tree-seq (memfn isDirectory) #(seq (.listFiles %)) base-java-file))

(defn create-lazy-file-sequence [base-java-file]
  (filter #(.isFile %) (create-lazy-dir-sequence base-java-file)))

(def base-file (new File (System/getProperty "user.dir")))
(def items (ref (create-lazy-file-sequence base-file)))

(defn get-next-item []
  (dosync
   (let [retval (first @items)]
     (alter items #(rest %))
     retval)))

(defn call-and-resend [function item agent agent-loop]
  (function item)
  (send-off agent agent-loop))

(defn agent-perform [latch]
  (let [my-item (get-next-item)]
    (if my-item 
      (call-and-resend maybe-p4Edit my-item *agent* agent-perform)
      (.countDown latch)))
  latch)

(def agent-count 4)

(def countdown-latch (new CountDownLatch agent-count))

(dotimes [_ agent-count] (send-off (agent countdown-latch) agent-perform))

(.await countdown-latch)
(shutdown-agents)

(println "Done!")
(def elapsed-time-seconds (/ (- (get-time) start-time) 1000.0))
(println "Considered" @considered-file-count "files in" elapsed-time-seconds "seconds." )
(doall (map #(println %) @touched-paths))