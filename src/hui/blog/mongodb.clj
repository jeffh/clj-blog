(ns hui.blog.mongodb
  "Provides database functionality for the blog."
  (:use clojure.contrib.except
	[clojure.contrib.java-utils :only (as-str)])
  (:import [com.mongodb Mongo BasicDBObject BasicDBList DBCollection
	    DBObject ObjectId]
	   java.util.Map
	   java.net.URI))

(def *mongo-connection* nil)
(def *mongo-db* nil)
(def *mongo-collection* nil)

(defn- get-or-fail [v s] (or v (throw-arg s)))
;; quick access to global vars or fail immediately
(defn current-connection
  "Returns the current connection if in a (with-mongo-conn) code block or
  throws an exception otherwise. Use *mongo-connection* if you want access
  without an exception."
  [] (get-or-fail *mongo-connection* "Not in mongo connection block."))
(defn current-db
  "Returns the current database if in a (with-mongo-db) code block or throws an
  exception otherwise. Use *mongo-db* if you want access without an exception."
  [] (get-or-fail *mongo-db* "Not in mongo db block."))
(defn current-collection
  "Returns the current collection if in a (with-mongo-coll) code block or
  throws an exception otherwise. Use *mongo-collection* if you want access
  without an exception."
  [] (get-or-fail *mongo-collection* "Not in mongo collection block."))
;; checks for global vars
(defn current-connection?
  "Returns true if in a (current-connection) code block."
  [] (if *mongo-connection* true false))
(defn current-db?
  "Returns true if in a (current-db) code block."
  [] (if *mongo-db* true false))
(defn current-collection?
  "Returns true if in a (current-collection) code block."
  [] (if *mongo-collection* true false))

(defn obj-id
  "Creates an ObjectId instance from a string when possible or simply returns
  the given value."
  [s] (if (and (string? s) (ObjectId/isValid s)) (ObjectId. s) s))

;; block wrapping macros
(defmacro with-mongo-conn
  "All mongo connection operations to default to operating on this connection."
  [mongo-object & body]
  `(binding [*mongo-connection* ~mongo-object] ~@body))
(defmacro with-mongo-db
  "Allows all mongo db operations to default to functioning on this database."
  [mongo-db & body]
  `(binding [*mongo-db* ~mongo-db] ~@body))
(defmacro with-mongo-coll
  "Allows all mongo collection operations to default to functioning on this
  collection."
  [mongo-coll & body]
  `(binding [*mongo-collection* ~mongo-coll] ~@body))

(declare mongo-connect get-db)
(defmacro with-mongo
  "connection-args => arguments for (mongo-connect)
  db-name => database name to select for (get-db)

  Wraps with-mongo-connection and with-mongo-db."
  [connection-args db-name & body]
  `(with-mongo-conn
     (apply mongo-connect ~connection-args)
     (with-mongo-db (get-db ~db-name)
       ~@body)))

(defn str-name
  "Gets the name of a given database or collection."
  [o] (.getName o))

(defn mongo-connect
  "Creates a mongodb connection. Optionally accepts username and password.
  Throws an exception if the authentication failed."
  {:arglists '([host port] [host port username password])}
  [host port & [username & [password]]]
  (let [m (Mongo. host port)]
    (if (and username password)
      (if (.authenticate m username password)
	m ; success
	(throw-arg "Invalid authentication for mongodb connection."))
      m)))

;; connect operations - functions that operate on connection instance
(defn get-dbs
  "Returns a seq of all the databases available on a given connection."
  ([] (get-dbs (current-connection)))
  ([connection] (seq (.getDatabaseNames connection))))

(defn db-exists?
  "Returns true if the given string is a mongodb in the provided server
  connection."
  ([db] (db-exists? (current-connection) db))
  ([connection db] (if (some #{db} (get-dbs connection)) true false)))

(defn remove-db
  "Removes the given database if it exists."
  ([db] (remove-db (current-connection db)))
  ([connection db] (.getDB connection db)))

(defn get-db
  "Gets the current database for the given mongodb connection. Returns a mongo
  db object. If the database does not exist, it is created."
  ([db] (get-db (current-connection) db))
  ([connection db] (.getDB connection (as-str db))))

(defn get-colls
  "Gets all the collection names in the database."
  ([] (get-colls (current-db)))
  ([db] (set (.getCollectionNames db))))
;; end connection operations

;; db operations - functions that operate on database instances
(defn coll-exists?
  "Returns true if the given collection name is in the given database."
  ([coll-name] (coll-exists? (current-db) coll-name))
  ([db coll-name] (if (some #{coll-name} (get-colls)) true false)))

(defn get-coll
  "Gets a collection from the given database. Collections hold a series of 
  documents. Returns a mongo collection object. If collection doesn't exist
  it is created lazily."
  ([coll-name] (get-coll (current-db) coll-name))
  ([db coll-name] (.getCollection db (as-str coll-name))))

(defn drop-coll
  "Drops the given collection or string-name."
  ([mcoll]
     (cond (not mcoll) nil ; do nothing
	   (instance? DBCollection mcoll) (.drop mcoll)
	   :else (drop-coll (current-db) (str mcoll))))
  ([db mcoll] (drop-coll (get-coll db mcoll))))
;; end database operations

;; collection operations - functions that operate on dbcollection instance
(declare create-doc)
(defn ensure-index
  "m => {\"field\" #{1 -1} ...}
  Ensures indexes on fields, from the given map, are created on the given
  collection. 1 creates indexes by ascending where -1 is descending.

  Ensures the given field has an index for the collection. If in a
  (with-mongo-coll) block, the mcoll is optional.

  This function is optimized for redundant use. Optional arguments:
    :force => Logical true forces the creation of indexes even if not necessary.
              Defaults to true.
    :unique => Logical true ensures each row has unique values for the given
               keys. Defaults to false."
  {:arglists '([m & kwargs] [mcoll m & kwargs])}
  [m & kwargs]
  (let [mcoll? (instance? DBCollection m)
	mcoll (if mcoll? m (current-collection))
	m (if mcoll? (first kwargs) m)
	kwargs (if mcoll? (rest kwargs) kwargs)
	{:keys [force unique] :or {force true}} (apply hash-map kwargs)]
    (.ensureIndex mcoll (create-doc m)
		  (if force true false)
		  (if unique true false))))

(defn drop-index
  "m => {\"field\" (or 1 -1) ...}
  Where 1 are ascending indexes and -1 is descending.

  If in a (with-mongo-coll) block, mcoll is optional.

  Removes indexes from a given collection. If no keys are provided, all
  indexes are removed from the collection."
  {:arglists '([] [mcoll] [m] [mcoll m])}
  ([] (drop-index (current-collection)))
  ([mcoll]
     (if (instance? DBCollection mcoll)
       (.dropIndexes mcoll)
       (drop-index (current-collection) mcoll)))
  ([mcoll m] (.dropIndex m)))

(defn re-index
  "Forces a reindexing of the collection's indexes. This is usually done
  automatically when needed. mcoll is optional if this function is called in
  a (with-mongo-coll) block."
  ([] (re-index (current-collection)))
  ([mcoll] (.reIndex mcoll)))

(defn count-all-docs
  "Gets the number of documents in a given collection."
  ([] (count-all-docs (current-collection)))
  ([mcoll] (.getCount mcoll)))

;; document operations
(defn create-list
  "Creates a list of values for use in a document. Accepts a seq-able item."
  [s]
  (let [rng (range (count s))
	s (if (map? s) s (apply hash-map (interleave rng (seq s))))]
    (doto (BasicDBList.) (.putAll s))))

(defn- recursive-as-str
  "Recursively applies as-str to all keywords in a (nested) map."
  [m]
  (cond (map? m)
	(apply hash-map
	       (interleave (map as-str (keys m))
			   (map recursive-as-str (vals m))))
	(keyword? m) (as-str m)
	(or (seq? m) (vector? m)) (map as-str m)
	:else m))

(defn create-doc
  "Creates a mongo db document in memory. This hasn't been included into the
  database yet. Accepts a map of all key-value pairs. All keys are converted to
  strings utilizing clojure.contrib.java-utils/as-str."
  [m]
  (if (some #{DBObject} (supers (class m)))
    m ;; no changes, it's already a DBObject-interfaced object
    (let [doc (BasicDBObject.)
	  m (recursive-as-str m)]
      (doto (BasicDBObject.) (.putAll m)))))

(defn doc-map
  "Converts a document to a clojure data structure that is independent from
  from data source. Assumes the keys are safe to convert to keywords."
  [doc]
  (cond (instance? Map doc) 
	(let [doc (into {} doc)]
	  (apply hash-map (interleave (map keyword (keys doc))
				      (map doc-map (vals doc)))))
	(instance? BasicDBList doc) (map doc-map (into [] doc))
	;(instance? ObjectId doc) (str doc)
	:else doc))

(defn doc-maps
  "Identical to (map doc-map (seq docs))."
  [docs] (map doc-map (seq docs)))
;; end document operations - back to collection operations

(let [seq? (fn [x] (or (list? x) (vector? x)))]
  (defn insert-docs
    "Inserts a given document or sequence of docuemnts into a collection. If
    the given document is a map, it is automatically converted into a document
    first."
    ([doc] (insert-docs (current-collection) doc))
    ([mcoll doc]
       (if (seq? doc)
	 (dorun (map (partial insert-docs mcoll) doc))
	 (.insert mcoll (create-doc doc)))))

  (defn save-docs
    "Saves the document or sequence of documents to the collection. If the
    collection has _id, it is inserted or updated."
    ([doc] (save-docs (current-collection) doc))
    ([mcoll doc]
       (if (seq? doc)
	 (dorun (map (partial save-docs mcoll) doc))
	 (.save mcoll (create-doc doc)))))

  (defn delete-docs
    "Removes the given document or sequence of documents from the collection."
    ([doc] (delete-docs (current-collection) doc))
    ([mcoll doc]
       (if (seq? doc)
	 (dorun (map (partial delete-docs mcoll) doc))
	 (.remove mcoll doc))))

  (declare get-docs)
  (defn delete-docs-if
    "Removes a set of documents from a collection based on a :where query-map
    from the (get-docs)."
    ([type query-map] (delete-docs-if (current-collection) query-map))
    ([mcoll type query-map]
       (delete-docs mcoll (get-docs mcoll type query-map)))))

(defn first-doc
  "Returns the first document in the collection."
  ([] (first-doc (current-collection)))
  ([mcoll] (.findOne mcoll)))

(defn get-doc
  "Returns the first document that matches the given id."
  ([id] (get-doc (current-collection id)))
  ([mcoll id] (.findOne mcoll id)))


(defn modify-cursor
  "Shortcut for modifying a cursor. Returns the modified cursor instance.
  Use seq afterwards to access its contents.

  Options:
    :batch => number of documents to fetch at a time from the server.
    :skip => number of documents to skip before fetching results.
    :limit => maximum number of documents to return.
    :sort => a map of the sorting in {field #{1 -1} ...} format where
             1 is ascending and -1 is descending order.
    :hint => a map of indexed fields to use to improve performance.

  Other helpful methods you can call on cursor instance that isn't directly
  supported by this function are:

    .explain => returns a DBObject of query details Use (doc-map) to convert
                to clojure map.
    .snapshot => Doesn't work with sorting or explicit hints. Runs the query
                 on a immutable collection state. Assures no duplicate or
                 missing objects from the start to end of the query's
                 execution. Query responses under 1MB are nearly identical to
                 being snapshotted."
  [cursor & kwargs]
  (let [{:keys [batch skip limit sort hint]} (apply hash-map (or kwargs []))]
    (if batch (.batchSize cursor (int batch)))
    (if skip (.skip cursor (int skip)))
    (if limit (.limit cursor (int limit)))
    (if sort (.sort cursor (create-doc sort)))
    (if hint (.hint cursor (if (string? hint) hint (create-doc hint))))
    cursor))

(defn find-all-docs
  "mcoll => Mongo Collection instance or default collection from block.
  query-map => {field value ...}
     value => #{string int float date binary code regex objectid nil array
                {$op value}}
     $op => #{\"$gt\" \"$lt\" \"$gte\" \"$lte\" \"$ne\" \"$in\" \"$nin\"
              \"$mod\" \"$all\" \"$size\" \"$exists\" #\"regular expression\"}
  fields => {key #{1 0}}

  Searches the given collection for documents. Accepts a query-map which
  is identical to 'WHERE x=y' sql mapping. Fields provides a map of fields
  to include or exclude by setting the key's value to true or false."
  {:arglists '([] [mcoll] [query-map] [query-map fields] [mcoll query-map]
		 [mcoll query-map fields])}
  [& args]
  (if (instance? DBCollection (first args))
    (let [[mcoll query-map fields] args
	  query-map (if query-map (create-doc query-map) query-map)]
      (cond fields (.find mcoll (create-doc query-map) (create-doc fields))
	    query-map (.find mcoll (create-doc query-map))
	    :else (.find mcoll)))
    (find-all-docs (current-collection) (first args) (second args))))

(defn get-docs
  "type => #{string keyword}
  mcoll => DBCollection instance
           Optionally accepts the mongodb collection instance to operate on or
           defaults to the collection provided by the closest block statement.

  Finds a document or series of documents based on type given. This function
  was inspired off of Ruby-on-Rails' ActiveRecord.find method. If type is
  an ObjectId instance, the document whose _id field is equal to it is
  returned. If type is a string, it is converted to an ObjectId instance first.
  If type is a keyword, it follows one of the following paths:

    :first => Runs a query where the first item is returned.
    :all => Runs a query where all the items are returned.

  In conjunction, there are kwarg options these keyword types accept:
  
    :where => Accepts a map of field-to-value pairs. See mongodb
              documentation for query args. Defaults to no restrictions.
              Mongodb accepts sub-key access using the dot-notation and supports
              more complex queries using a map as the value:

              {:number { :$gt 3 }} ;; finds docs where number field > 3

              See note below about keywords [1].
    :fields => Accepts a map of field-to-[boolean-get-field-value?] pairs.
               Defaults to nil, which fetches all fields/keys from documents.
               See note below [1].
    :limit => Maximum number of documents to fetch. Always 1 for :first and
              nil for :all.
    :sort => Accepts a map of field-to-[ascending/descending] pairs. Defaults
             to nil. See note below [1].
    :batch => Number of documents to fetch per query. This is used if the number
              of documents to fetch is extremely large and working with the
              entire set would have memory issues. Defaults to nil.
    :skip => Number of documents that match the query to skip before returning
             documents. Defaults to nil.
    :hint => Accepts a map of field-to-[boolean-use-index] pairs. Defaults
             to nil. See note below [1].
    :process-one => The function that processes a DBObject instance. This is
                    called when get-docs is given an id or :first as its type.
                    Defaults to (doc-map) function.
    :process-cursor => The function that processes DBCursor instance. This is
                       called when get-docs is given :all as its type. Defaults
                       to (doc-maps).

  [1] All maps' keys will be converted to string using
  clojure.contrib.java-utils/as-str function; therefore, keywords are
  identical to strings without the prefixed colon (:).
  (eg - :hello becomes \"hello\")"
  {:arglists '([type & kwargs] [mcoll type & kwargs])}
  [type & kwargs]
  ;; first, check for mcoll
  (let [kwargs (if (nil? kwargs) [] kwargs)
	has-mcoll? (instance? DBCollection type) ; true if found
	mcoll (if has-mcoll? type (current-collection))
	type (if has-mcoll? (first kwargs) type)
	type (#{:all :first} type) ; the kw types we accept
	;; add type to kwargs if we used mcoll
	kwargs (if has-mcoll? (rest kwargs) kwargs)
	kwargs (if (and kwargs (even? (count kwargs))) (apply hash-map kwargs))
	{:keys [where fields process-one process-cursor]
	 :or {where {}, process-one doc-map, process-cursor doc-maps}} kwargs
	count= (fn [x] (and kwargs (= (count kwargs) x)))
	type (obj-id type)]
    (throw-if (nil? type) "Type needs to be defined or is invalid.")
    (throw-if (nil? kwargs) "kwargs is not an even number of arguments.")
    (cond (instance? ObjectId type) (process-one (get-doc mcoll type))
	  (and (= type :first) (count= 0)) (process-one (first-doc mcoll))
	  (= type :first)
	  (process-one
	   (first (seq (apply modify-cursor
			      (find-all-docs mcoll where fields)
			      :limit 1
			      (interleave (keys kwargs) (vals kwargs))))))
	  (= type :all)
	  (process-cursor
	   (apply modify-cursor
		  (find-all-docs mcoll where fields)
		  (interleave (keys kwargs) (vals kwargs))))
	  :else (throw-arg "Invalid find type operation."))))

(let [p1 (fn [x] (if x 1 0))
      pc (fn [x] (.count x))]
  (defn count-docs
    "Counts the number of docs based on (get-docs) query. Passes all its
    arguments to (find-docs), except modifies :process-one and :process-cursor
    kwargs for counting purposes. Unlike get-docs, specifying the type
    is required."
    [& args]
    (apply get-docs
	   (conj (into [] args) :process-one p1, :process-cursor pc))))