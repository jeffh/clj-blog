(ns compojure.openid
  "Compojure OpenID wrapper around jopenid library. Originally from weavejester's gist
  which can be found at http://gist.github.com/127459. Modified by Jeff (jeffh)"
  (:use compojure.control)
  (:use compojure.encodings)
  (:use compojure.http)
  (:use compojure.http.session)
  (:import org.expressme.openid.OpenIdManager)
  (:import javax.servlet.http.HttpServletRequest))
 
(defn- make-manager
  [options]
  (doto (OpenIdManager.)
    (.setReturnTo (options :return-to))
    (.setRealm    (options :realm nil))
    (.setTimeOut  (options :timeout 10000))))
 
(defn- openid-lookup
  [manager openid-url]
  (let [endpoint    (.lookupEndpoint manager openid-url)
        association (.lookupAssociation manager endpoint)
        auth-url    (.getAuthenticationUrl manager endpoint association)]
    [(session-assoc :openid-mac (.getMacKey association))
     (redirect-to auth-url)]))
 
(defn- request-proxy
  [params]
  (proxy [HttpServletRequest] []
    (getParameter [key]
      (params (keyword key)))))
 
(defn- auth->map
  [auth]
  {:identity (.getIdentity auth)
   :email    (.getEmail auth)})
 
(defn- set-session-auth
  [manager session params]
  (let [mac-key (base64-decode-bytes (session :openid-mac))
        request (request-proxy params)
        auth    (.getAuthentication manager request mac-key)]
    (session-assoc :openid (auth->map auth))))
 
(defn openid-auth
  "Returns routes for openid urls. After the openid authentication process, session
  will store a key->map of :openid->{:identity, :email}
  
  path: the url that accepts :openid parameter which represents the user's openid url.
  options: a map of additional settiongs:
      { :return-to - the server url which validates the response from the openid provider.
        :success-uri - the url to go to after the user logged in via openid
        :realm (optional) - the (sub)domain which this openid authentication
                            request is accessing.
        :timeout (optional) - the timeout in milliseconds
      }"
  [path options]
  (let [manager (make-manager options)]
    (routes
      (POST path
        (openid-lookup manager (params :openid)))
      (GET (options :return-to)
        [(set-session-auth manager session params)
         (redirect-to (options :success-uri))]))))
