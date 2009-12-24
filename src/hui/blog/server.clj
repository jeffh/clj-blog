(ns hui.blog.server
  "Provides server control functions. No core part of the blog app should
  load this code unless it is a higher-level than this (eg - server
  management)."
  (:use compojure
	[compojure.server.common :only (get-host-and-path)]
	[hui.blog.routes :only (root-routes)]
	hui.blog.reloader)
  (:import org.mortbay.jetty.servlet.Context
	   org.mortbay.jetty.servlet.FilterHolder
	   org.mortbay.jetty.Handler
	   org.mortbay.servlet.GzipFilter))

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
     (filter #(.endsWith % ".clj") (get-files "hui/blog")))
(def *file-watcher* (agent nil))

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
  (send-off *file-watcher* start-watching-files file-watch-list)
  nil)


(defn stop-server
  "Stops the Jetty Server asynchonously."
  []
  (if @*server*
    (send-off *server* #(do (stop %) (println "Server terminated.\n") nil))
    (throw (IllegalArgumentException. "No server initialized!")))
  (send-off *file-watcher* stop-watching-files)
  nil)

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

(defn -main
  []
  (println "HTTP Server on port 8080 ...")
  (start-server))