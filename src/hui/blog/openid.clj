(ns hui.blog.openid
  "Provides an openid authentication layer. Provides routes via
  'openid-routes for inclusion to a server."
  (:use compojure
	compojure.openid
	[hui.blog.db :as db]
	[hui.blog.utils :as utils]))

;; when using URLs, its recommended to prefix with /openid or something
;; alike that.
(def openid-urls {:begin "/openid/start"
		  :verify "/openid/validate"
		  :end "/openid/end"})

;; Provides a set of urls to support the openid consumer protocol
(def openid-routes
     (openid-auth (openid-urls :begin)
		  {:return-to (openid-urls :end)
		   :success-uri (openid-urls :validate)}))

(defn- get-info
  ([session] (get session :identity))
  ([session key] ((session :openid {}) key)))

(defroutes openid-validate
  "Simply looks for the openid in the database and creates if needed."
  (GET (openid-urls :verify)
       (if (get-info session)
	 (db/with-db
	   (let [usr (first (db/user (get-info session)))]
	     (if (not usr)
	       (db/user {:openid (get-info session),
			 :email (get-info session :email)}))
	     (redirect-to "/")))
	 (redirect-to "/")))

(utils/enable-session-for openid-routes openid-validate)