(ns hui.blog.server
  "Provides server control functions. No core part of the blog app should
  load this code unless it is a higher-level than this (eg - server
  management"
  (:use
   [hui.blog.routes :only (root-routes)]
   compojure))

;(run-server {:port 8080} "/*" (servlet webservice))

;; async implementation
;; use (start-server) and (stop-server)
(defonce *server* (agent nil))

(defn- init-server
  "Returns an instanciated server object."
  ([_ & [options [routes]]]
     (let [r (if routes routes root-routes)
	   o (if options options {:port 8080})]
       (doto (jetty-server o "/*" (servlet r)) (start)))))

(defn- add-routes
  [server routes root-path]
  (add-servlet! server (if root-path root-path "/*") (servlet routes))
  server)

(defn start-server
  "Starts the Jetty Server asynchonously."
  [& [options [routes]]]
  (clear-agent-errors *server*)
  (send-off *server* init-server options routes))

(defn stop-server
  "Stops the Jetty Server asynchonously."
  [] (if @*server*
       (send-off *server* #(do (stop %) (println "Server terminated.\n") nil))
       (throw (IllegalArgumentException. "No server initialized!"))))

(defn extend-routes
  "Updates the Jetty Server with new routes."
  [routes & [root-path]]
  (if @*server*
    (send-off *server* add-routes routes root-path)
    (throw (IllegalArgumentException. "No server initialized!"))))

(defn restart-server
  [] (stop-server) (start-server))
