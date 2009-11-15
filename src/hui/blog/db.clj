(ns hui.blog.db
  "Provides database functionality for the blog."
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join chop re-split re-gsub)]
	clojure.contrib.except
	compojure))

;; imports all vars as local vars to this namespace,
;; including other files that use or require this file.
;; This allows access to func & vars in other libraries
;; through this file.
;;
;; For documentation of functions provided by clojure.contrib.sql:
;;   http://richhickey.github.com/clojure-contrib/sql-api.html
(immigrate 'clojure.contrib.sql)

;; database configuration settings use by with-connection-config
;; TODO: we should really support MYSQL / Other databases
;;       which basically means fixing schemas definition
(def db-config {:classname "org.postgresql.Driver"
		:subprotocol "postgresql"
		:subname "blog"
		:user "jeff"
		:password "xuqa+Hu&aK@w&s+!+#phawadrestesec"})

(def *site* nil) ;; currently active site, managed by bindings
(def *print-queries* true) ;; if true, prints executed queries to *out*

;; represents the tables we want in the database.
;; these should be in create-table parameter syntax such that:
;;   (apply create-table :key (latest-schemas :key))
;; would work.
(def schema-version 1) ;; the latest schema version integers only
(def first-schema ;; the very first schema spec. (all changes go to migrations)
     {:sites [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:domain "varchar(50)" "UNIQUE" "NOT NULL"]
	      [:title "varchar(50)" "NOT NULL"]
	      [:theme "varchar(50)" "DEFAULT 'default'" "NOT NULL"]
	      [:comment_embed "text" "DEFAULT ''" "NOT NULL"]]
      ;; many-to-many relationship between sites and users
      :memberships [[:id "serial" "PRIMARY KEY" "NOT NULL"]
		    [:user_id "integer" "NOT NULL"]
		    [:site_id "integer" "NOT NULL"]
		    [:access "integer" "DEFAULT 0" "NOT NULL"]
		    ["UNIQUE (user_id, site_id)"]]
      :categories [[:id "serial" "PRIMARY KEY" "NOT NULL"]
		   ;[:site_id "integer" "NOT NULL"] ;; sharing is caring
		   [:name "varchar(50)" "NOT NULL"]
		   [:slug "varchar(50)" "NOT NULL"]]
      :posts [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:site_id "integer" "NOT NULL"]
	      [:category_id "integer" "NULL"]
	      [:title "varchar(100)" "NOT NULL"]
	      [:slug "varchar(100)" "NOT NULL"]
	      [:body "text" "NOT NULL"]
	      [:created "timestamp with time zone"
	       "DEFAULT current_timestamp" "NOT NULL"]
	      [:published "timestamp with time zone" "NULL"]]
      ;; in reality, the only real users are admins
      :users [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:openid "varchar(200)" "UNIQUE" "NOT NULL"]
	      [:email "varchar(50)" "DEFAULT ''" "NOT NULL"]
	      [:name "varchar(50)" "DEFAULT ''" "NOT NULL"]
	      [:about "text" "DEFAULT ''" "NOT NULL"]]
      ;; global settings pertaining to the app:
      ;;  - db-schema version (for future migrations)
      ;;  - plugin-settings (when we implement plugins)
      :settings [[:name "varchar(50)" "PRIMARY KEY" "NOT NULL"]
		 [:site_id "integer" "NULL"] ;; NULL == global
		 [:value "varchar(100)" "NOT NULL"]]})
;; alter/drop/add column are not implemented yet
(declare setup-tables alter-column drop-column add-column)
(defn schema-migration ;; how to upgrade from one version to then next
  [n]
  (let [update-path {1 first-schema
		     }]
    (get update-path n)))

(def schema-data ;; first-time installation data
     {:settings [{:name "schema-version" :value "1"}]})

(def access
     {:reader 0
      ;; add other access-levels if schema needs be
      :admin 9001}) ; its over 9000!


(defmacro with-db
  "Identical to with-connection, except the db settings are already
  provided. Doesn't open a new connection if one already exists"
  [& body]
  `(if (find-connection)
     (do ~@body)
     (with-connection db-config ~@body)))

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

(defn str-query
  "Converts a query to string, using as-str."
  {:test
   (fn []
     (let [expected "select * from t"]
       (assert (= expected (str-query "select * from t")))
       (assert (= expected (str-query :select :* :from :t)))
       (assert (= expected (str-query "select" :* "from t")))))}
  [& query]
  (apply str (interpose " " (map as-str query))))

(defn query-words
  "Simply breaks a query to its words."
  {:test
   (fn []
     (assert (= (list "select" "*" "from" "t")
		(query-words "select * from t"))))}
  [& query]
  (re-split #" +" (.toLowerCase (apply as-str query))))

(defn has-clause?
  "Determines if the given string query has a given word the given word
  cannot have spaces. All keywords are convert to strings use as-str."
  {:test
   (fn []
     (let [base "select * from t"]
       (assert (true? (has-clause? base :select)))
       (assert (true? (has-clause? base "select")))
       (assert (false? (has-clause? base "where")))
       (assert (false? (has-clause? base :where)))))}
  [query clause]
  (let [query (if (or (seq? query) (vector? query))
		query (query-words query))
	clause (.toLowerCase (as-str clause))]
    (not (not (some #{clause} query)))))

(defn insert-clause
  "Adds a query partial to a given clause in the query. If the clause
  already exists, the unifier is used to join the query-partial with the
  existing clause. Unifier defaults to 'AND'."
  {:test
   (fn []
     (let [base "select * from t"]
       (binding [has-clause? (fn [& _] false)]
	 (assert (= (str base " where a=a")
		    (insert-clause base :where "a=a")))
	 (assert (= (str base " from a")
		    (insert-clause base "from" "a"))))
       (assert (= (str base " where 1=1")
		  (insert-clause base :where "1=1")))
       (assert (= (str base " where 1=1 and 2=2")
		  (insert-clause (str base " where 2=2") :where "1=1")))
       (assert (= "select * from t2 , t"
		  (insert-clause base :from "t2" ",")))))}
  [query clause query-partial & [unifier]]
  (let [unifier (if unifier unifier :and)
	clause (.toLowerCase (as-str clause))
	words (query-words query)]
    (apply
     str-query
     (if (has-clause? words clause)
       (map (fn [x] (if (= x clause)
		      (str-query x query-partial unifier)
		      x)) words)
       (conj (vec words) clause query-partial)))))

(defn query-substitution
  "Used for printing sql-queries. This isn't used in sql-query directly, but
  is a good approximation."
  [query substitutions]
  (let [query-partials (re-split #"\?" (str query))
	subs (map #(str "'" (re-gsub #"'" "\\\\'" (as-str %)) "'") substitutions)]
    (if (<= (count subs) 0)
      query
      (apply str (interleave query-partials subs)))))

(defn sql-query
  "Executes a given query with substitutional replacements of ? in the
  query string."
  [#^String query & substitutions]
  (if *print-queries*
    (println (str "Query: " (query-substitution query substitutions))))
  (with-query-results results
    (into [query] (map #(if (keyword? %) (as-str %) %) substitutions))
    (into [] results)))

(defn get-metadata
  "Queries all the tables in the database for metadata (not the clj kind)."
  []
  (into [] (resultset-seq
	    (-> (connection)
		(.getMetaData)
		(.getTables nil nil nil (into-array ["TABLE" "VIEW"]))))))

(defn valid-connection?
  "Tests to see if the database config settings are valid."
  [] (try
      (with-db (get-metadata)) true
      (catch Exception _ false)))

(defn count-rows
  "Counts the number of rows in a given table. This is done via SQL's COUNT, so
  is faster than fetching all the rows and doing a (count) if you simply just
  want to determine the number of rows in the table.

  Like sql-query, this needs to be wrapped in a database connection.

  Accepts a query-parser to modify the base query, 'select count(id) from table'.
  Use insert-clause for easily editing the query."
  [table & [query-parser & substitutions]]
  (let [substitutions (or substitutions [])
	query-parser (or query-parser identity)]
    (apply (comp :count first sql-query)
     ((comp query-parser str-query)
      :select (as-str "count(" table ".id)") :from table)
     substitutions)))

(defn get-tables
  "Lists all the table names."
  [] (map #(% :table_name) (filter #(% :table_name) (get-metadata))))

(defn find-table
  "Checks if the given table name exists. Keywords convert to strings.
  Returns string of table if found or nil."
  [name] (some #{(as-str name)} (get-tables)))

(defn table-exists?
  "Checks if the given table name exists. Keywords converts to strings.
  Returns boolean only."
  [name] (not (not (find-table name))))

(defn create-table-if-not-exists
  "Identical to create-table, but ensures the table doesn't exist before
  attempting to create it."
  [name & specs]
  (if (not (table-exists? name))
    (apply create-table name specs)))

(defn drop-table-if-exists
  "Identical to drop-table, but sliently fails instead of throwing
  an exception. Also accepts more than one table."
  [& names]
  ;(dorun (map #(try (drop-table %) (catch Exception _)) names)))
  (dorun (map #(if (table-exists? %) (drop-table %)) names)))

(defn setup-tables
  "Takes a hashmap of sequences which correspond {key value} as
  {table-name spec}. Tables are created only if they don't already exist."
  [tables]
  (transaction
   (dorun
    (map (fn [[n s]] (apply create-table-if-not-exists n s)) tables))))

(defn teardown-tables
  "Drops tables if they exist. Accepts the same data structure setup-tables
  utilizes for convinence."
  [tables]
  (transaction (apply drop-table-if-exists (keys tables))))

(defn find-or-insert
  "Finds for a given condition in a table. If no rows are returned, then
  insert-record map is used to create a new record; then the condition
  is re-performed and returned. If insert-record is nil, the query results
  are simply returned with no insert-records operation."
  [table select-cond-and-replacements & [insert-record]]
  (let [query (str-query "select * from" table :where
		   (first select-cond-and-replacements))
	r (apply sql-query query
		 (rest select-cond-and-replacements))]
    (if (and (empty? r) (not insert-record))
      (do (insert-records table insert-record)
	  (sql-query query))
      r)))

(defn table-association
  "Returns the table name if in <table-name>_id format from a field or nil
  otherwise."
  {:test
   (fn []
     (assert (= "user" (table-association :user_id)))
     (assert (= "user" (table-association "user_id")))
     (assert (nil? (table-association :user)))
     (assert (nil? (table-association "user"))))}
  [field]
  (if (.endsWith (as-str field) "_id")
    ((apply comp (repeat 3 chop)) (as-str field))))

(defn table-association?
  "Returns true if the table name if in <table-name>_id format from a field.
  Returns false otherwise."
  [field]
  (not (not (table-association field))))

(defn get-by-id
  "Gets the first row in a table filtered by an id."
  [table id]
  (first (sql-query (str-query "select * from" table :where "id=?") id)))

(defn pluralize
  "A primitive pluralization of a given word."
  [word]
  (cond
   (.endsWith word "us") (str (chop (chop word)) "i")
   (.endsWith word "y") (str (chop word) "ies")
   (or (.endsWith word "x") (.endsWith word "s")) (str word "es")
   true (str word "s")))

(defn get-field
  "Returns a delayed function call to a relationship or simply returns
  the field value."
  [field value]
  (if (and (table-association? field) (not (nil? value)))
    (delay (with-db (get-by-id (pluralize (table-association field)) value)))
    value))

(defn seq-or-vec?
  "Returns true if the given value is a seq or vector. Identical to writing:
  (not (not (or (seq? value) (vector? value))))."
  [i] (not (not (or (seq? i) (vector? i)))))

(defn relate-to-one
  "Updates a row to have a belongs-to relationship to a given table. This
  automatically detects belongs-to relationships via _id fields (eg - user_id).
  All the related fields are lazy: running a query only when the value is
  required and is memoized afterwards.

  The corresponding tables are assumed to be pluralized.
  Works on a sequence of records or just a single record."
  [record]
  (if (seq-or-vec? record)
    (map #(relate-to-one %) record)
    (try
     (apply hash-map
	    (interleave (keys record)
			(map get-field (keys record) (vals record))))
     (catch ClassCastException _ record))))

(defn map-by
  "Converts a resultset into a hash map with field as the key with the
  row's map as its value."
  [field results]
  (apply hash-map (interleave (map #(% field) results) results)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level definitions: Functions pertaining to the schemas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: these functions MAY BREAK if the schema changes! This is
;;          especially true since the schema is not near-final yet. ~Jeff

(defn- site-clause
  "Generates a site.id=id where clause. Relies on *state* global ref."
  [] (str "sites.id=" (:id *site*)))

(defn add-site-clause
  "A shortcut for adding a site-clause if needed. The original query should
  never contain a sites where clause.

  A given query is modified to include the 'sites' table in the from clause and
  'sites.id=n' in the where clause where n=(*site* :id). If *site* is logically
  false, then the given query is returned unchanged."
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
  "Automatically adds 'site.id=*site*' where clause if *site* is set. If not
  the given query is executed, unchanged. For consistancy, the given queries
  should be as explicit as possible: like using table.* instead of just *.

  For example, if the query given is:
    'select t.* from t'
  site-query produces one of the two:
    ;; if *site* is logically false (unchanged query)
    'select t.* from t'
    ;; if the *site* is logically true, filters the query to the specific site
    ;; where n is the (*site* :id)
    'select t.* from sites , t where sites.id=n'

  This query generation method is performed by add-site-clause, which utilizes
  insert-clause to figure out where to insert the proper sql partials."
  [query & substitutions]
  (apply sql-query (add-site-clause query) substitutions))

(defn posts
  "Gets all published posts or force fetching all posts. Accepts a query-parser
  which can be used modify the base query provided by this function. The queries
  vary based on the keyword:

  {:all 'select posts.* from posts'
   :published 'select posts.* from posts where posts.published
               <= current_timestamp'
   :drafts 'select posts.* from posts where posts.published > current_timestamp'}

  It is recommended to use insert-clause to assist in modifying the queries. Like
  most high-level functions, posts defaults to executing the query via
  site-query - meaning the proper site.id is automatically added if need be.

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

(if-not (valid-connection?)
  (println "Warning: Failed to connect to DB. (Check DB configs?)"))