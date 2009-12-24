(ns hui.blog.routes
  "Maps URIs to specific functions for the server."
  (:require [hui.blog.views :as views]
	    [hui.blog.openid :as openid]
	    [hui.blog.utils :as utils])
  (:use compojure))

;; provides abstraction between the url and internal links
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
 ;; REMEMBER to append the slash at the end of the route where
 ;; appropriate. This is utilized for
 (GET "/favicon.ico" (page-not-found))
; (ANY "/*" (openid/openid-routes request))
 (GET "/" (views/all-posts request))
 (ANY "/openid/redirect/" (openid/begin-openid request
					      :redirect-to "/openid/auth"))
 (GET "/openid/auth/" (openid/end-openid request :redirect-to "/"))
 (GET "/public/*" (or (serve-file
		       "/home/jeff/Projects/clj-blog/public/" (params :*))
		      :next))
 (POST #"/new/" (views/new-post request))
 (GET "/:slug/" (views/post request :slug (-> request :route-params :slug)))
 (GET "/*" (views/auto-append-slash request))
 (GET "/*" (page-not-found)))


(dosync
 (ref-set root-routes (with-session @root-routes
			{:type :memory, :expires 600})))