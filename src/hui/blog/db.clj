(ns hui.blog.db
  "Provides database functionality for the blog."
  (:use clojure.contrib.except
	[clojure.contrib.str-utils :only (re-gsub)]
	hui.blog.mongodb)
  (:import java.util.Date))

(def db-settings {:host "localhost", :port 27017, :db "blog"})
(defn db-sites [] (get-coll "sites")) ; sites collection name
(defn db-posts [] (get-coll "posts")) ; posts collection name
(defn db-authors [] (get-coll "authors")) ; authors collection name

(defmacro with-db
  "Initializes a mongo db connection for code in its body. Uses db-settings."
  [& body]
  `(with-mongo [(db-settings :host) (db-settings :port)]
       (db-settings :db) ~@body))

(defmacro with-coll
  "Wraps a db connection if needed and allows its body's operation to function
  on a specific collection. Accepts a 0-arty function that returns the
  collection to operate on. Use with-posts, with-authors, or with-sites for
  high-level operations."
  [db-coll-fn & body]
  `(if (current-connection?)
     (with-mongo-coll (~db-coll-fn) ~@body)
     (with-db (with-mongo-coll (~db-coll-fn) ~@body))))

(defmacro with-posts
  "Operates the given body using the posts collection."
  [& body] `(with-coll db-posts ~@body))
(defmacro with-sites
  "Operates the given body using the sites collection."
  [& body] `(with-coll db-sites ~@body))
(defmacro with-authors
  "Operates the given body using the authors collection."
  [& body] `(with-coll db-authors ~@body))

;; It's better to explain the current schema - even though the actual database
;; is schema-less.
;;
;; Theses are the current collections:
;;   sites: Provides information about each site
;;   posts: Provides all the blog posts for all sites
;;   authors: Provides author information
;;
;; Fields prefixed with ! are indexed in ascending (1) and postfixed with
;; a ! are indexed in descending (-1).
;;
;; Sites Schema:
;;   title: Public name of the site
;;   !base: The base URL the site resites on which is accessible to the public.
;;          This should make the base url to type to access the blog. The string
;;          is case-insensitive. UNIQUE per site.
;;   disqus: A map of settings for the disqus commenting system
;;     key: The disqus API forum key. REQUIRED.
;;     mode: A string of either 'embed' or 'request'. Embed simply embeds
;;           javascript into the comments page. Until request is implemented,
;;           this is the recommended method. Request will utilizes the disqus
;;           API 1.1: http://groups.google.com/group/disqus-dev/web/api-1-1.
;;           Defaults to 'embed' until request is implemented.

;; Authors Schema:
;;   name: Public name of the user. Nil if new author that didn't login yet.
;;   !openid: The openid login url. UNIQUE per author.
;;   admin-for: A list of site _ids that the author has admin access to.
;;
;; Posts Schema:
;;   site-ids: A list of site ids this post belongs to.
;;   title: Name of the particular post.
;;   !slug: URL-friendly string to reference to the post from the web.
;;          UNIQUE with site_ids.
;;   body: The html body of the post.
;;   published: The datetime the post will be published. Nil is a draft.
;;   tags: A list of strings for data tags
;;

(defn ensure-indexes-for
  [type]
  (cond (= type :posts) (ensure-index {:slug 1} :unique true)
	(= type :authors) (ensure-index {:openid 1})
	(= type :sites) (ensure-index {:base 1})))

;;;; HELPERS
(defn valid-slug?
  "Returns true if the given string is a unique slug that doesn't conflict
  with existing reserved keywords."
  [v]
  (and
   (not (#{"new" "openid" "public" "maintenance"}
	 (.toLowerCase (str v))))
   (= 0 (with-posts (count-docs :all :where {:slug v})))))

(defn slugify
  "Creates a lowercased alpha-numerical, hyphen, underscored only string.
  Spaces are converted to hyphens."
  [s]
  (let [s (->> (re-gsub #" " "-" s)
	       (re-gsub #"[^A-Za-z0-9_-]" "")
	       (re-gsub #"-{2,}" "-")
	       (.toLowerCase))]
    (loop [curr s, i 1]
      (if (valid-slug? curr)
	curr
	(recur (str s "-" i) (inc i))))))

(defn process-post
  "Ensures valid values for certain fields for a post map."
  [m]
  (throw-if-not (:site-ids m) "Site ids must be specified.")
  (assoc m
    :title (or (:title m) "")
    :slug (or (:slug m) (slugify (:title m)))
    :body (or (:body m) "")
    :published (or (:published m) (new Date))
    :tags (or (:tags m) [])))

(defn process-site
  "Ensures valid values for certain fields for a site map."
  [m]
  (let [d (:disqus m)]
    (throw-if-not (:key d) "No disqus forum API key provided.")
    (assoc m
      :title (or (:title m) "My Blog")
      :base (.toLowerCase (m :base))
      :disqus (assoc d :mode (or (:mode d) "embed")))))

(defn process-author
  "Ensures valid values for certain fields for an author map."
  [m]
  (throw-if-not (:openid m) "No openid for author!")
  (throw-if-not (:admin-for m) "Not admin. What's the point then?")
  (assoc m :name (or (:name m) nil)))

;;;; END HELPERS


(defn get-posts
  "Gets all posts by published date. Optionally accepts kwargs for (get-docs)."
  [& kwargs]
  (with-posts
    (ensure-indexes-for :posts)
    (apply get-docs :all, :sort {:published -1}, kwargs)))

(defn get-post
  "Gets a particular post by slug & site id or returns nil if not found."
  [site-id slug]
  (with-posts
    (ensure-indexes-for :posts)
    (get-docs :first :where {:slug slug :site-ids {:$in [site-id]}})))

(defn save-post
  "Creates and saves a map to the post collection."
  [m] (with-posts (save-docs (process-post m))))

(defn get-site
  "Gets a site by base url or nil on failure."
  [base]
  (with-sites
    (ensure-indexes-for :sites)
    (get-docs :first :where {:base base})))

(defn save-site
  "Saves the site based on a given map. Processes the map to be valid before
  saving to the database."
  [m] (with-sites (save-docs (process-site m))))

(defn get-author
  "Gets the author map of the given openid."
  [openid]
  (with-authors
    (ensure-indexes-for :authors)
    (get-docs :first :where {:openid openid})))

(defn save-author
  "Saves an author based on a given map. Processes the map to be valid before
  saving to the database."
  [m] (with-authors (save-docs (process-author m))))

(defn is-admin?
  "Returns true if the given openid is an admin for a given site-id."
  [openid siteid]
  (with-authors
    (ensure-indexes-for :authors)
    (= 1 (count-docs :first :where {:site-ids {:$in (obj-id siteid)}}))))