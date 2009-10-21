(ns #^{:author "Jeff Hui"
       :doc "Provides the uri functions for the server."}
  hui.blog.views
  (:use compojure))

(defn all-posts
  [request & [year-filter]]
  (if year-filter
    "FILTER_BY_YEAR"
    "Hello World!"))

(defn post
  [request & args]
  "HI")

(defn add-comment
  [request & args]
  "ADDING_COMMENT")