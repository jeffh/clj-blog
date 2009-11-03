(ns hui.blog.db
  "Provides database functionality for the blog."
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join chop)]
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

;; represents the tables we want in the database.
;; these should be in create-table parameter syntax such that:
;;   (apply create-table :key (schemas :key))
;; would work.
(def schemas
     {:sites [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:domain "varchar(50)" "UNIQUE" "NOT NULL"]
	      [:title "varchar(50)" "NOT NULL"]
	      [:theme "varchar(50)" "DEFAULT 'default'" "NOT NULL"]]
      ;; many-to-many relationship between sites and users
      :membership [[:user_id "varchar(200)" "NOT NULL"]
		   [:site_id "integer" "NOT NULL"]
		   [:access "integer" "DEFAULT 0" "NOT NULL"]
		   ["PRIMARY KEY (user_id, site_id)"]]
      :posts [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:site_id "integer" "NOT NULL"]
	      [:title "varchar(150)" "NOT NULL"]
	      [:body "text" "NOT NULL"]
	      [:created "timestamp with time zone"
	       "DEFAULT current_timestamp" "NOT NULL"]
	      [:published "timestamp with time zone" "NULL"]]
      ;; in reality, the only real users are admins
      :users [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:openid "varchar(200)" "UNIQUE" "NOT NULL"]
	      [:email "varchar(50)" "DEFAULT ''" "NOT NULL"]
	      [:name "varchar(50)" "DEFAULT ''" "NOT NULL"]
	      [:about "text" "DEFAULT ''" "NOT NULL"]]})
(def access
     {:reader 0
      :admin 9001}) ; its over 9000!


(defmacro with-db
  "Identical to with-connection, except the db settings are already
  provided."
  [& body]
  `(with-connection db-config ~@body))

(defn sql-query
  "Executes a given query with substitutional replacements of ? in the
  query string."
  [query & substitutions]
  (with-query-results results
    (into [query] (map as-str substitutions))
    (into [] results)))

(defn get-metadata
  "Queries all the tables in the database for metadata."
  []
  (into [] (resultset-seq
	    (-> (connection)
		(.getMetaData)
		(.getTables nil nil nil (into-array ["TABLE" "VIEW"]))))))

(defn valid-connection?
  "Tests to see if the database config settings are valid."
  []
  (try
   (with-db (get-metadata)) true
   (catch Exception _ false)))

(defn get-tables
  "Lists all the table names."
  [] (map #(% :table_name) (filter #(% :table_name) (get-metadata))))

(defn table-exists?
  "Checks if the given table name exists. Keywords converts to strings."
  [name]
  (some #{(as-str name)} (get-tables)))

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

(defn kwd-query
  "Converts a list of keywords to strings. Good for queries."
  [& args]
  (apply as-str (interleave args (repeat " "))))

(defn find-or-insert
  "Finds for a given condition in a table. If no rows are returned, then
  insert-record map is used to create a new record; then the condition
  is re-performed and returned. If insert-record is nil, the query results
  are simply returned with no insert-records operation."
  [table select-cond-and-replacements & [insert-record]]
  (let [query (kwd-query "select * from" table :where
		   (first select-cond-and-replacements))
	r (apply sql-query query
		 (rest select-cond-and-replacements))]
    (if (and (empty? r) (not insert-record))
      (do (insert-records table insert-record)
	  (sql-query query))
      r)))

(defn table-association
  "Returns the table name if in <table-name>_id format from a field.
  Returns nil otherwise."
  [field]
  (if (.endsWith (as-str field) "_id")
    ((apply comp (repeat 3 chop)) (as-str field))))

(defn table-association?
  "Determines if the given field is a table association or not."
  [field]
  (not (nil? (table-association field))))

(defn get-by-id
  "Gets the first row in a table filtered by an id."
  [table id]
  (first (sql-query (kwd-query "select * from" table :where "id=?") id)))

(defn get-field
  "Returns a delayed function call to a relationship or simply returns
  the field value."
  [field value]
  (if (table-association? field)
    (delay (get-by-id (table-association field) value))
    value))

(defn lazy-relationships
  "Takes a query result and filters all fields ending in '_id' as a hint
  to association to: <table-name>_id."
  [record]
  (apply hash-map
	 (interleave (keys record)
		     (map get-field (keys record) (vals record)))))

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

(defn posts
  "Gets all published posts or force fetching all posts."
  ([] (sql-query "select * from posts where published <= current_timestamp"))
  ([type]
     (cond
      (= type :all) (sql-query "select * from posts")
      (= type :published) (posts)
      (= type :drafts)
      (sql-query
       "select * from posts where published > current_timestamp"))))

(defn post
  "Gets/Set a post by id. Inserts a new row if id happens to be a map."
  ([id] (cond
	 (map? id) (insert-records :posts id)
	 true (sql-query "select * from posts where id=?" id)))
  ([id record] (update-or-insert-values :posts ["id=?" id] record)))

(defn user
  "Gets/Sets a user by openid. Inserts a new row if the id happens to
  be a map."
  ([id] (cond
	 (map? id) (insert-records :users id)
	 true (sql-query "select * from users where openid=?" id)))
  ([id record] (update-or-insert-values
		:users ["openid=?" id] record)))

(defn settings
  "Gets all settings."
  [] (sql-query "select * from settings"))
(defn setting
  "Gets/Sets a setting by name. If settings the given name doesn't
  exist, the value is created."
  ([name]
     ((first
       (sql-query "select * from settings where name=?" name))
      :value))
  ([name val] (update-or-insert-values
	       :settings ["name=?" (as-str name)]
	       {:name (as-str name) :value (as-str val)})))

(defn sites
  "Gets all sites available in the database."
  [] (sql-query "select * from sites"))
(defn sites-map [] (map-by :id (sites)))
;; cache by the app server for performance reasons
;; reload-sites! is used to force reload
(def *sites* (ref (with-db (sites-map))))

(defn reload-sites!
  "Reloads the sites information from the database."
  [] (dosync (with-db (ref-set sites-cache (sites-map)))))