(ns #^{:author "Jeff Hui"
       :doc "Provides database connection information."}
  hui.blog.db
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join)]
	compojure))

;; imports all vars as local vars to this namespace,
;; including other files that use or require this file.
;; This allows access to func & vars in other libraries
;; through this file.
(immigrate 'clojure.contrib.sql)

(def db-config {:classname "org.postgresql.Driver"
		:subprotocol "postgresql"
		:subname "blog"
		:user "jeff"
		:password "xuqa+Hu&aK@w&s+!+#phawadrestesec"})

(def schemas
     {:posts [[:id "serial" "PRIMARY KEY" "NOT NULL"]
	      [:title "varchar(150)" "NOT NULL"]
	      [:body "text" "NOT NULL"]
	      [:created "timestamp with time zone"
	       "DEFAULT current_timestamp" "NOT NULL"]
	      [:published "timestamp with time zone" "NULL"]]
      :users [[:openid "varchar(200)" "PRIMARY KEY" "NOT NULL"]
	      [:email "varchar(50)"]
	      [:name "varchar(50)"]
	      [:is_admin "boolean" "DEFAULT false" "NOT NULL"]]
      :settings [[:name "varchar(50)" "PRIMARY KEY" "NOT NULL"]
		 [:value "text" "NOT NULL"]]})

(defmacro with-connection-config
  "Identical to with-connection, except the db settings are already
  provided."
  [& body]
  `(with-connection db-config ~@body))
(defmacro wcc [& body] `(with-connection-config ~@body))

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
  ([id record] (update-or-insert-values :posts ["id=?" id] params)))

(defn user
  "Gets/Sets a user by openid. Inserts a new row if the id happens to
  be a map."
  ([id] (cond
	 (map? id) (insert-records :users id)
	 true (sql-query "select * from users where openid=?" id)))
  ([id record] (update-or-insert-values
		:users ["openid=?" id] params)))

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