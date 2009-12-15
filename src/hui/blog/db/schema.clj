(ns hui.blog.db.schema
  "Handles database schemas, such as migrations."
  (:use [clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (re-gsub)]
	[clojure.contrib.sql :only (drop-table)]
	hui.blog.db.core))

;; TODO: migrations are not finished yet.

;; internal API

;; stores all migrations
(defonce *migrations* (ref {}))
(defstruct migration-struct :up :down)

(defn create-migration
  "Creates a migration unit."
  [& m]
  (let [m (apply hash-map m)]
    (assoc m
	:up
	(if-not (m :up)
	  (fn [& _] (println "WARN: Empty :up migration."))
	  (m :up))
	:down
	(if-not (m :up)
	  (fn [& _] (println "WARN: Empty :down migration."))
	  (m :up)))))

(defn latest-version
  "Gets the highest version number in the migration sequence.
  Returns 0 if there are no migrations."
  ([] (latest-version @*migrations*))
  ([migrations]
     (cond (= (count migrations) 0) 0
	   (= (count migrations) 1) (first (keys migrations))
	   :else (apply max (keys migrations)))))

(defn latest-migration
  "Gets the highest-versioned migration. Returns nil if no versions."
  ([] (latest-migration @*migrations*))
  ([migrations]
     (if-not (= (count migrations) 0)
       (migrations (latest-version migrations)))))
;  (let [highest-ver (latest-version migrations)]
;    (first (filter #(= (latest-version migrations) (get % :version 0))
;		   migrations))))

(def sql-types {:primary-key {:name "integer PRIMARY KEY"}
		:string {:name "varchar" :limit 255}
		:text {:name "text"}
		:integer {:name "integer"}
		:float {:name "float"}
		:decimal {:name "decimal"}
		:datetime {:name "timestamp"}
		:time {:name "time"}
		:date {:name "date"}
		:binary {:name "bytea"}
		:boolean {:name "boolean"}})

(defn quoted-str
  "Quotes a string for sql."
  [s] (re-gsub #"'" "\\'" s))

(defn mapify
  "Creates a hash-map out of a list where keys with no values are simply
  keys whose values are true."
  [valueless-keys & kv-pairs]
  (loop [f (first kv-pairs)
	 r (rest kv-pairs)
	 p []]
    (if f
      (if (some #{f} valueless-keys)
	(recur (first r) (rest r) (conj p f true))
	(recur (second r) (rest (rest r)) (conj p f (first r))))
      (apply hash-map p))))

(defn new-field-properties
  "Creates a sequence to represent field properties."
  [type & properties]
  (let [type (sql-types (keyword type))
	f (if properties
	    (merge type (apply mapify [:null :not-null] properties))
	    type)]
    [(as-str (f :name) (if (f :limit)
			    (str "(" (f :limit) ")")))
     (as-str (if (or (f :not-null) (not (f :null)))
	       "NOT NULL"))
     (as-str (if (contains? f :default)
	       (str "DEFAULT "
		    (cond
		     (nil? (f :default)) "NULL"
		     (number? (f :default)) (f :default)
		     true (str "'" (quoted-str (f :default)) "'")))))]))

(defn new-field
  "Creates a clojure data structure to represent a field."
  {:test (fn []
	   (assert (= (new-field "id" :primary-key)
		      ["id" "integer PRIMARY KEY" "NOT NULL" ""]))
	   (assert (= (new-field "name" :string :limit 100)
		      ["name" "varchar(100)" "NOT NULL" ""])))}
  ([field] (apply new-field field)) ; assume it's a sequence
  ([field-name type & properties]
     ;; [field-name & sql-partials]
     (into [(as-str field-name)]
	   (apply new-field-properties type (or properties [])))))
	

(defn new-table
  "Creates a clojure data structure to represent a table."
  [table-name & fields]
  {(keyword table-name) (map new-field fields)})

;; public API

;; not implemented yet
;(declare add-table drop-table alter-table rename-table sql-query
;	 rename-column drop-column add-column)
(def *table* nil)

(defn add-table
  "Creates a table on the database."
  [table-name & fields]
  (setup-tables (apply new-table table-name fields)))

(defn get-current-table
  "Gets the table value if in a with-table block. Nil if otherwise."
  [] *table*)
(defn current-table?
  "Determines if the code is in a with-table block."
  [] (if (get-current-table) true false))
(defmacro with-table
  "Performs operations on a given table. Access via *table* or
  (get-current-table)."
  [table-name & body]
  `(binding [*table* (get-current-table)] ~@body))
(defmacro alter-table
  "Performs a given set of operations on a table."
  [table-name & body]
  `(with-table ~table-name (fn [] ~body)))

(defn add-column
  "Adds a column to the given table. table-name? is not needed if called in a 
  with-table block."
  {:arglists '([table-name? field]
	       [table-name? field-name field-type & field-properties])}
  [& args]
  (let [table-name (if (current-table?) (get-current-table) (first args))
	args (if (current-table?) args (rest args))
	field (if (> (count args) 1)
		(apply new-field args)
		(first args))]
    (sql-query (apply as-str "alter table ? add " (interpose " " field))
	       table-name)))

(defn rename-column
  "Renames a given column of a table to a new column name. table-name? is
  not needed if called in a with-table block."
  {:arglists '([table-name? column-name new-column-name])}
  [& args]
  (let [table-name (if (current-table?) (get-current-table) (first args))
	args (if (current-table?) args (rest args))
	[column-name new-column-name] args]
    (sql-query "alter table ? rename ? to ?"
	       table-name column-name new-column-name)))

(defn drop-column
  "Removes a given column of a table. table-name? is not needed if called in
  a with-table block."
  {:arglists '([table-name? column-name])}
  [& args]
  (let [table-name (if (current-table?) (get-current-table) (first args))
	args (if (current-table?) args (rest args))
	column-name (first args)]
    (sql-query "alter table ? drop ?"
	       table-name column-name)))

(defn register-migration
  "Adds a migration function to this database. If :down-fn is not provided, a
  simple function that prints a warning to *out* is used. :version is a number
  that uniquely identifies it and when it is called in the versioning scheme."
  [up-fn & kwargs]
  (let [kwargs (apply hash-map kwargs)
	version (or (kwargs :version) (inc (latest-version @*migrations*)))
	down-fn (kwargs :down-fn nil)
	mig (if down-fn
	      (create-migration :up up-fn :down down-fn)
	      (create-migration :up up-fn))]
    (dosync (alter *migrations* assoc version mig))))

(defmacro defmigration
  "Defines a database schema migration step. Optionally accepts migration
  version number. Although a downward migration is optional, it is
  recommended to implement one."
  {:arglists '([version? up down?])}
  [& args]
  (let [version (if (number? (first args)) (first args))
	args (if (number? (first args)) (rest args) args)
	[up down] args]
    `(let [d# ~down]
       (register-migration (fn [] ~up)
			   :down-fn (if d# (fn [] d#))
			   :version ~version))))

(defn migrate-up
  "Runs the given migration upgrade function."
  [migration] ((:up migration)))
(defn migrate-down
  "Runs the given migration downgrade function."
  [migration] ((:down migration)))

;(defn migrate-to
;  "Returns a sequence of functions from one migration version to another. The
;  functions are either upgrade or downgrade functions depending on the 
;  difference between the 2 versions."
;  [current-version change-to-version]
;  (let [diff (- change-to-version current-version)
;	fn-name (if (pos? diff) :up-fn :down-fn)
;	numbers (range current-version change-to-version
;		       (if (neg? diff) -1 1))]
;    (if-not (neg? diff)
;      (filter #(