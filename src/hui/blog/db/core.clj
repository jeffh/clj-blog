(ns hui.blog.db.core
  "Provides core, general-purpose database functionality."
  (:use [clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join chop re-split re-gsub)]
	clojure.contrib.sql
	clojure.test
	hui.blog.utils :only (pluralize)))

;; database configuration settings use by with-connection-config
;; TODO: we should really support MYSQL / Other databases
;;       which basically means fixing schemas definition
(defonce db-config {:classname "org.postgresql.Driver"
		    :subprotocol "postgresql"
		    :subname "blog"
		    :user "jeff"
		    :password "xuqa+Hu&aK@w&s+!+#phawadrestesec"})
(defonce *print-queries* true) ;; if true, prints executed queries to *out*

(defmacro with-db
  "Identical to with-connection, except the db settings are already
  provided. Doesn't open a new connection if one already exists"
  [& body]
  `(if (find-connection)
     (do ~@body)
     (with-connection db-config ~@body)))

(defn str-query
  "Converts a query to string, using as-str."
  {:test
   (fn []
     (are [x] (= "select * from t" (apply str-query x))
	  ["select * from t"]
	  [:select :* :from :t]
	  ["select" :* "from t"]))}
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
     (are [x y] (x (has-clause? "select * from t" y))
	  true? :select
	  true? "select"
	  false? "where"
	  false? :where))}
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
	 (are [x y] (= (str base " " x) (apply insert-clause base y))
	      "where a=a" [:where "a=a"]
	      "from a" ["from" "a"]))
       (binding [has-clause? (fn [& _] true)]
	 (are [x y z] (= x (apply insert-clause z y))
	      (str base " where 1=1 and 2=2")
	      [:where "1=1"] (str base " where 2=2")
	      "select * from t2 , t"
	      [:from "t2" ","] base))))}
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

(defn rename-table
  "Renames a table to a new name. Executes a SQL92-syntax."
  [table-name new-table-name]
  (sql-query "alter table ? rename to ?" table-name new-table-name))

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