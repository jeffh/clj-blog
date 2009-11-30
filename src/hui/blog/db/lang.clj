(ns hui.blog.db.lang
  "Handles specific table logic."
  (:use hui.blog.db.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level definitions: Functions pertaining to the schemas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: these functions MAY BREAK if the schema changes! This is
;;          especially true since the schema is not near-final yet. ~Jeff

(def *site* nil)

(declare find-site *sites*)
(defmacro with-site
  "Runs the given high-level db functions assuming a given site id.
  If a string of a domain is passed, then a lookup of the site id is done to
  fetch the id before proceeding. If site id is a map, it is passed directly
  to as a site record.

  Uses with-db macro in body."
  {:test
   (fn []
     (binding [*site* nil
	       *sites* (ref {1 :hi})]
       (assert (nil? *site*))
       (with-site 1 (assert (= *site* :hi)))))}
  [siteid & body]
  `(let [sid# ~siteid]
     (binding [*site* (cond
		       (string? sid#) (find-site sid#)
		       (map? sid#) sid#
		       true (@*sites* sid#))]
       (with-db ~@body))))

(defn- site-clause
  "Generates a site.id=id where clause. Relies on *state* global ref."
  [] (str "sites.id=" (:id *site*)))

(defn add-site-clause
  "A shortcut for adding a site-clause if needed. The original query should
  never contain a sites where clause.

  A given query is modified to include the 'sites' table in the from
  clause and 'sites.id=n' in the where clause where n=(*site* :id). If
  *site* is logically false, then the given query is returned unchanged."
  {:test
   (fn []
     (let [base "select t.* from t"]
       (assert (= base (add-site-clause base)))
       (binding [*site* {:id 2}]
	 (assert (= "select t.* from sites , t where sites.id=2"
		    (add-site-clause base))))))}
  [query]
  (if *site*
    (insert-clause
     (insert-clause query :where (site-clause))
     :from "sites" ",")
    query))

(defn site-query
  "Automatically adds 'site.id=*site*' where clause if *site* is set. If
  not the given query is executed, unchanged. For consistancy, the given
  queries should be as explicit as possible: like using table.* instead
  of just *.

  For example, if the query given is:
    'select t.* from t'
  site-query produces one of the two:
    ;; if *site* is logically false (unchanged query)
    'select t.* from t'
    ;; if the *site* is logically true, filters the query to the
    ;; specific site
    ;; where n is the (*site* :id)
    'select t.* from sites , t where sites.id=n'

  This query generation method is performed by add-site-clause, which
  utilizes insert-clause to figure out where to insert the proper sql
  partials."
  [query & substitutions]
  (apply sql-query (add-site-clause query) substitutions))

(defn posts
  "Gets all published posts or force fetching all posts. Accepts a
  query-parser which can be used modify the base query provided by this
  function. The queries vary based on the keyword:

  {:all 'select posts.* from posts'
   :published 'select posts.* from posts where posts.published
               <= current_timestamp'
   :drafts 'select posts.* from posts where posts.published > '+
            'current_timestamp'}

  It is recommended to use insert-clause to assist in modifying the queries.
  Like most high-level functions, posts defaults to executing the query
  via site-query - meaning the proper site.id is automatically added if
  need be.

  Any additional arguments are passed to site-query as ? substitutions."
  ([] (posts :published))
  ([type]
     (if (fn? type)
       (posts :all type)
       (posts type identity)))
  ([type query-parser & substitutions]
     (if (= type :count)
       (count-rows :posts query-parser)
       (apply
	(comp relate-to-one site-query)
	(cond
	 (= type :all)
	 (query-parser "select posts.* from posts")

	 (= type :published)
	 (query-parser
	  (str
	   "select posts.* from posts where posts.published <= current_timestamp"
	   " and posts.published IS NOT NULL"))

	 (= type :pending)
	 (query-parser
	  "select posts.* from posts where posts.published > current_timestamp")

	 (= type :drafts)
	 (query-parser
	  "select posts.* from posts where posts.published IS NULL"))
	substitutions))))

(defn post
  "Gets/Set a post by id. Inserts a new row if id is a map."
  [id]
  (cond
   (map? id) (insert-records :posts id)
   true (posts :published #(insert-clause % :where "posts.id=?") id)))

(defn users
  "Gets all users which can be filtered by access level"
  ([] (users :all))
  ([type]
     (site-query
      (cond
       (= type :all) "select users.* from users"
       true (str "select users.* from users where access=" (access type))))))

(defn user
  "Gets/Sets a user by openid. Inserts a new row if the id happens to
  be a map."
  {:arglists '([id] [user-record access-level? siteid?])}
  ([id] (site-query "select users.* from users where users.openid=?" id))
  ([user-record [access-level & [site-id]]]
     (let [site-id (or site-id (:id *site*)
		    (throw-arg (str "site must be given or this function"
				    " must be in a with-site macro.")))
	   access-level (access access-level (access :reader))
	   uid (do (insert-records :users user-record)
		   (user (:openid user-record)))]
       (insert-records :memberships
		       {:user_id uid
			:site_id site-id
			:access access-level}))))

(defn membership
  "Gets a membership relationship by openid and siteid. Siteid is optional if
  with-site is used. This is the recommended method to check if the user is
  registered with a particular site."
  [openid & [siteid]]
  (let [siteid (or siteid (:id *site*) (throw-arg "siteid not set"))]
    ((comp relate-to-one first sql-query)
	    "select * from memberships where user_id=? and site_id=?"
	    openid siteid)))

(defn sites
  "Gets all sites available in the database."
  ([] (sql-query "select * from sites"))
  ([record]
     (if-not (map? record)
       (throw-arg "expected a map"))
     (insert-records :sites record)))
(defn sites-map [] (map-by :id (sites)))
;; cache by the app server for performance reasons
;; reload-sites! is used to force reload
(def *sites* (ref (if (valid-connection?)
		    (with-db (if (table-exists? :sites) (sites-map))))))

(defn reload-sites!
  "Reloads the sites information from the database."
  [] (dosync (with-db (ref-set *sites* (sites-map)))))

(defn find-site
  "Takes a given request server and fetches the url."
  [url] (first (filter #(= (:domain %) url) (vals @*sites*))))