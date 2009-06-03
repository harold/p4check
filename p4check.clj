(import '(java.io File)
				'(java.lang ProcessBuilder)
				'(java.util.concurrent CountDownLatch))

(def files [])

(defn p4Edit [path]
  (let [process (new ProcessBuilder ["p4" "edit" path])]
		(println path)
		(. process start)))

(defn watchFilter [path]
  (let [extension (.substring path (+ 1 (.lastIndexOf path ".")))]
		(some #{extension} ["cpp" "cxx" "h"
												"xml" "inl"	"c"
												"vcproj" "sln"
												"js" "build"
												"include"])))

(defn maybe-p4Edit [file]
	(let [path (. file getAbsolutePath)]
		(when (. file canWrite) ; File is *not* read only.
			(when (watchFilter path)
				(p4Edit path)))))
	
(defn build-list [file]
	(if (. file isFile)
		(def files (conj files file))
	  (dorun (map #(build-list %1) (. file listFiles)))))

(def base (new File (System/getProperty "user.dir")))

(dorun (map #(build-list %1) (. base listFiles)))

(println (count files) "files to process..")

(def items (ref files))

(defn get-next-item []
	(dosync
	 (let [retval (first @items)]
		 (alter items #(rest %1))
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