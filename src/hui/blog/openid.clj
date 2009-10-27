(ns hui.blog.openid
  "Provides openid authentication handlers."
  (:use compojure
	[clojure.contrib.java-utils :only (as-str)]
	[hui.blog.utils :as utils])
  (:import org.verisign.joid.consumer.JoidConsumer
	   java.util.Map))

(def consumer (JoidConsumer.))

(defn- auth-url
  "Returns the openid provider url for the given openid url."
  [openid-url return-to trust-url]
  (.getAuthUrl consumer openid-url return-to trust-url))
(defn- simplify-auth-response
  [auth-result]
  {:success? (.isSuccessful auth-result)
   :identity (.getIdentity auth-result)})
(defn- auth
  "Attempts a clojure map or java.utils/Map for the return-to url to
  authenticate the response with the server."
  [parameters]
  (simplify-auth-response
   (.authenticate consumer
		  (if (isa? parameters Map)
		  parameters
		  (apply hash-map
			 (interleave
			  (map as-str (keys parameters))
			  (map as-str (vals parameters))))))))

(defn begin-openid
  "Handles the initilization of the openid authentication."
  [request & options]
  (let [kwargs (apply hash-map options)
	param-key (kwargs :param-key :url)
	uri (absolute-url request (kwargs :redirect-to))
	trust-root (kwargs :trust-uri (utils/absolute-url request))]
    (redirect-to (auth-url ((request :params) param-key) uri trust-root))))

(defn end-openid
  "Handles the authorization confirmation by verifying the data with the
  openid provider. Associates :openid to the openid identity in the
  session."
  [request & options]
  (let [kwargs (apply hash-map options)
	uri (kwargs :redirect-to)
	oid (auth (request :params))
	red (redirect-to uri)]
    (if (oid :success?)
      [(session-assoc :openid (oid :identity)) red]
      red)))
