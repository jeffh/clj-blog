(ns
    #^{:author "Jeff Hui"
       :doc "Provides an openid authentication layer. Requires JOID 1.1+"}
    hui.blog.openid
    (:use compojure)
    (:import [org.verisign.joid Store OpenId]))

(defn 

(defn openid-routes
  "Provides a set of urls to support the open-id consumer protocol."
  [login-page-fn]
  (routes*
   (GET "/" login-page-fn)
   (ANY "/begin_openid_login" ) ; ?openid_url= (both GET & POST)
   (GET "/complete_openid_login" )) ; lib handles params
