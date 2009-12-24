(ns hui.blog.views
  "Provides core uri functions for the blog."
  (:use compojure
	hui.blog.db
	[hui.blog.utils :only (absolute-url)]
	hui.blog.html)
  (:import java.util.Date))

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
  [request & [slug]]
  (with-request-bindings request
    (index-page (get-site (:server-name request)) nil (get-posts :limit 10))))
;    (if (session :openid)
;      (str "Hello, " (session :openid) "!")
;      (str request))))

(defn new-post
  [request]
  (let [site (get-site (:server-name request))]
    (if site
      (with-request-bindings request
	(with-validated-params params post-validate
	  (if (validation-errors?)
	    (index-page site nil (get-posts :limit 10))
	    (do (save-post {:site-ids [(params :target)]
			    :title (params :title)
			    :body (params :body)})
	    (redirect-to "/")))))
      (redirect-to "/new/site/"))))

(defn new-site
  [request])

(defn post
  [request & kwargs]
  (let [kwargs (apply hash-map kwargs)
	site (get-site (:server-name request))]
    (with-request-bindings request
      (index-page site nil (vector (get-post (:_id site) (:slug kwargs)))))))

(defn add-comment
  [request & args]
  "ADDING_COMMENT")

;(enable-session-for all-posts post add-comment)