(ns hui.blog.templates
  "Provides HTML output for given data."
  (:refer-clojure :exclude [empty complement])
  (:use net.cgrand.enlive-html
	clojure.contrib.str-utils :only (chop)
	[hui.blog.db :as db]))

;; template structure
(def template-files
     {:posts "posts.html" ;; all posts
      :post "post.html"
      :page "page.html"})

(defn static
  "Points to a specific static web file."
  [& extra]
  (apply str "/public/" extra)) ; where static content resides

(defn template
  "Points to a specific template file."
  [& extra]
  (chop (apply str "templates/" (interleave extra (repeatedly "/")))))
