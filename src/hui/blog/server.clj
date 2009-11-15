(ns hui.blog.server
  "Provides server control functions. No core part of the blog app should
  load this code unless it is a higher-level than this (eg - server
  management"
  (:use compojure
	[compojure.server.common :only (get-host-and-path)]
	[hui.blog.routes :only (root-routes)])
  (:import org.mortbay.jetty.servlet.Context
	   org.mortbay.jetty.servlet.FilterHolder
	   org.mortbay.jetty.Handler
	   org.mortbay.servlet.GzipFilter
	   java.io.File))

;(run-server {:port 8080} "/*" (servlet webservice))

;; async implementation
;; use (start-server) and (stop-server)
(defonce *server* (agent nil))
;; initializes an async server with a set of filters
;; keys represent the filter to use while the value is
;; the uri for the filter to apply to
(def default-filters 
     {GzipFilter "/*"})

(def file-watch-list
     (map #(str "hui/blog/" %)
	  ["views.clj" "db.clj" "routes.clj" "openid.clj"]))
(def *file-watcher* (agent nil))

(defn last-modified
  "Gets the last modified time as long."
  [filepath]
  (.lastModified (File. filepath)))

(defn reload-old
  "Watches a set of files and reloads them automatically if changed."
  [recorded-times file-list]
  (if (nil? recorded-times)
    (apply hash-map
	   (interleave file-list
		       (map last-modified file-list)))
    (let [new-times (watch nil file-list)]
      (doseq [[filename old-time] (seq recorded-times)]
	(if (not= (- (new-times filename) old-time) 0)
	  (do (println (str "Reloaded: " filename)) (load-file filename))))
      new-times)))
(defn watch-files
  [old-file-times file-list]
  (if (not= old-file-times :stop)
    (do
      (send-off *agent* #'watch-files file-list)
      (let [r (reload-old old-file-times file-list)]
	(Thread/sleep 2000)
	r))
    old-file-times))

(declare add-filter!)
(defn- init-server
  "Returns an instanciated server object."
  [_ & [options [routes]]]
  (let [r (if routes routes root-routes)
	o (if options options {:port 8080})
	s (doto (jetty-server o "/*" (servlet r)) (start))]
    (map #(add-filter! s (second %) (first %)) default-filters) s))

(defn- add-routes
  [server routes root-path]
  (add-servlet! server (if root-path root-path "/*") (servlet routes))
  server)

(defn add-filter!
  "Adds a servlet to a Jetty server."
  [server url-or-path filter & [dispatches]]
  (prn (class filter))
  (let [[host path] (get-host-and-path url-or-path)
	context     (get-context server host)
	holder      (if (instance? FilterHolder filter)
		      filter
		      (FilterHolder. filter))]
    (.addFilter context holder path
		(if dispatches dispatches Handler/REQUEST))
    server))

(defn start-server
  "Starts the Jetty Server asynchonously."
  [& [options [routes]]]
  (clear-agent-errors *server*)
  (send-off *server* init-server options routes)
  (send-off *file-watcher* watch-files file-watch-list))


(defn stop-server
  "Stops the Jetty Server asynchonously."
  []
  (if @*server*
    (send-off *server* #(do (stop %) (println "Server terminated.\n") nil))
    (throw (IllegalArgumentException. "No server initialized!")))
  (send-off *file-watcher* (fn [& _] :stop)))

(defn append-routes
  "Updates the Jetty Server with new routes."
  [routes & [root-path]]
  (if @*server*
    (send-off *server* add-routes routes root-path)
    (throw (IllegalArgumentException. "No server initialized!"))))

(defn append-filter
  "Updates the Jetty Server with new filters."
  [filter & [root-path]]
  (if @*server*
    (send-off *server* add-filter! (if root-path root-path "/*") filter)))

(defn restart-server
  [] (clear-agent-errors *server*) (stop-server) (start-server))
