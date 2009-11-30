(ns hui.blog.views
  "Provides core uri functions for the blog."
  (:use compojure
	[hui.blog.utils :only (absolute-url)]))

(defn auto-append-slash
  [request]
  (with-request-bindings request
    (if (re-matches #"^/(.+[^/])$" (:uri request))
      (redirect-to
       (if (empty? (dissoc params :*))
	 (absolute-url request (:uri request))
	 (url-params (absolute-url request (:uri request))
		     (dissoc params :*)))))))

(defn all-posts
  [request & [year-filter]]
  (with-request-bindings request
    (str "HI " (request :server-name))))
;    (if (session :openid)
;      (str "Hello, " (session :openid) "!")
;      (str request))))

(defn post
  [request & args]
  "GET_POST")

(defn add-comment
  [request & args]
  "ADDING_COMMENT")

;(enable-session-for all-posts post add-comment)