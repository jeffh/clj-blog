(ns hui.blog.reloader
  "Handles automatic reloading of clojure files for rapid iteration of code."
  (:import java.io.File))

(def *print-on-reload* true)

(defn last-modified
  "Gets the last modified time as long."
  [filepath]
  (.lastModified (File. filepath)))
(defn directory?
  "Returns true if the given path is a folder."
  [filepath]
  (.isDirectory (File. filepath)))
(defn file?
  "Returns true if the given path is a file."
  [filepath]
  (.isFile (File. filepath)))
(defn get-files
  "Lists all files and directories in the given path."
  [filepath]
  (map #(str (if (.endsWith filepath "/") filepath (str filepath "/")) %)
       (seq (.list (File. filepath)))))
(defn get-files-recursively
  "Lists all files in a directory tree."
  [dirpath]
  (let [files (get-files dirpath)]
    (into (filter file? files)
	  (map get-files (filter directory? files)))))

(defn reload-old
  "Watches a set of files and reloads them automatically if changed."
  [recorded-times file-list]
  (if (nil? recorded-times)
    (apply hash-map
	   (interleave file-list
		       (map last-modified file-list)))
    (let [new-times (reload-old nil file-list)]
      (doseq [[filename old-time] (seq recorded-times)]
	(if (not= (- (new-times filename) old-time) 0)
	  (do
	    (if *print-on-reload* (println (str "Reload: " filename)))
	    (try (load-file filename) (catch Exception _)))))
      new-times)))

(defn watch-files
  "Performs on an agent. Recursively checks the file-list for modifications and
  reloads them with reload-all when the modification changed. If the agent is 
  set to :stop, then the constant watching is stopped."
  [agt file-list & [delay]]
  (if (not= agt :stop)
    (do
      (send-off *agent* #'watch-files file-list)
      (let [r (reload-old agt file-list)]
	(Thread/sleep (or delay 1000))
	r))
    agt))

(defn start-watching-files
  "Resets the agent for watching if needed."
  [agt file-list & [delay]]
  (send *agent* (fn [& _] nil))
  (send-off *agent* watch-files file-list delay))
  

(defn stop-watching-files
  "Performs on an agent. Simply sets the agent to :stop, which stops the recuring
  polling of file modifications done be watch-files."
  [agt] :stop)

(comment
;; example usage
(def watcher (agent nil))
;; start watching- will automatically reload now
(send-off watcher start-watching-files ["main.clj" "other.clj"])
;; if we want to stop it
(send-off watcher stop-watching-files)
)
