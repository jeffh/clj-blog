(ns hui.blog.views
  "Provides core uri functions for the blog."
  (:use compojure
	[hui.blog.utils :only (absolute-url enable-session-for)]))

(defn all-posts
  [request & [year-filter]]
  (absolute-url request (request :uri) "openid" "/start"))

(defn post
  [request & args]
  "GET_POST")

(defn add-comment
  [request & args]
  "ADDING_COMMENT")

(enable-session-for all-posts post add-comment)