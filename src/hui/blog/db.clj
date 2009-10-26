(ns hui.blog.db
  "Provides database functionality for the blog."
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join)]
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
	      [:title "varchar(50)" "NOT NULL"]]
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
      :users [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:openid "varchar(200)" "UNIQUE" "NOT NULL"]
	      [:email "varchar(50)" "DEFAULT \"\"" "NOT NULL"]
	      [:name "varchar(50)" "DEFAULT \"\"" "NOT NULL"]]
      :settings [[:name "varchar(50)" "PRIMARY KEY" "NOT NULL"]
		 [:value "text" "NOT NULL"]]})
(def access
     (with-meta
       ;; current access flags
       {:reader 0, :admin 9001} ; its over 9000!
       ;; version of the access flags
       {:version 1}))
;; TODO: provide an upgrade route


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
    (into [query] substitutions)
    (into [] results)))

(defn get-tables
  "Queries all the tables in the database."
  []
  (into [] (resultset-seq
	    (-> (connection)
		(.getMetaData)
		(.getTables nil nil nil (into-array ["TABLE" "VIEW"]))))))

(defn table-exists?
  "Checks if the given table name exists."
  [name]
  (some #(= (% :table_name) (as-str name)) (get-tables)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level definitions
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
  ([name] (sql-query "select * from settings where name=?" name))
  ([name val] (update-or-insert-values
	       :settings ["name=?" (as-str name)]
	       {:name (as-str name) :value (as-str val)})))