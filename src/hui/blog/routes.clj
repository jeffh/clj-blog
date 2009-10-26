(ns hui.blog.routes
  "Maps URIs to specific functions for the server."
  (:require [hui.blog.views :as views])
  (:use compojure))

;; provides abstraction between the url and internal links
(defmulti uri
  "Provides abstraction from uri to blog resources.
  Accepts a keyword type and parameters to build a uri."
  (fn [type & _] type))
(defmethod uri :posts [_] "/")
(defmethod uri :year-posts [_ year] (str "/" year))
(defmethod uri :post [_ year id & [slug]]
  (str "/" year "/" id "/" slug))
(defmethod uri :save-comment [_ year id & _]
  (str "/" year "/" id "/comment"))

(defmacro refreshable-routes!
  "Defines a route-name ref and sets routes using ref-set to allow
  the routes to be reloaded when this file is reloaded (via :reload-all)."
  [routes-name & routes-body]
  `(do
     (defonce ~routes-name (ref nil))
     (dosync
      (ref-set ~routes-name (routes ~@routes-body)))
     ~routes-name))

;; core routes that the server directly loads
;; use net.blog.utils/extend-routes to add more routes
;; and update URIs. Routes are applied below to allow
;; the routes to be updated on :reload-all
(refreshable-routes!
 root-routes
 (GET "/" (views/all-posts request))
 (GET #"/([0-9]{4})" (views/all-posts request :year (params 0)))
 (GET #"/([0-9]{4})/([0-9]+)/:slug"
      (views/post request
		  :year (params 0)
		  :id (params 1)
		  :slug (params :slug)))
 (POST #"/([0-9]{4})/([0-9]+)/:slug"
       (views/add-comment request
			  :year (params 0)
			  :id (params 1)
			  :slug (params :slug))))
