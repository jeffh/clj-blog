(ns hui.blog.db.core
  "Tests the core databases by interacting with a remote database."
  (:use hui.blog.db.core
	clojure.contrib.sql
	clojure.test))

(def db-settings ; test database connectivity
     {:driver :postgres
      :host "localhost"
      :database "tests"
      :user "jeff"
      :password "xuqa+Hu&aK@w&s+!+#phawadrestesec"})

(defn- clear-db
  "Removes all tables from the test database."
  [& text]
  (let [tables (get-tables)]
    (when-not (empty? tables)
      (println (str (apply str text) "Dropping " (count tables) " table(s)."))
      (dorun (map #(drop-table %) tables)))))
(defmacro in-test-environment
  "Runs tests in a given environment. Removes all tables after the tests finish."
  [& body]
  `(binding [db-config (ref db-settings)
	     *print-queries* false]
     (with-db
       (clear-db "[test:init]   ")
       (try ~@body
	    (catch Exception e#
	      (clear-db "[test:except] ")
	      (throw e#)))
       (clear-db "[test:deinit] "))))

(deftest test-db-connection
  (dosync (ref-set db-config {}))
  (is (false? (valid-connection?)))
  (dosync (ref-set db-config db-settings))
  (is (true? (valid-connection?))))

(deftest test-create-table
  (in-test-environment
   (are [x y] (x y)
	empty? (get-tables)
	true? (create-table-if-not-exists :first_table
					  ["id" "int" "primary key"]
					  ["name" "varchar(200)"])
	(partial = 1) (count (get-tables))
	nil? (create-table-if-not-exists :first_table [])
	(partial = 2) (inc (count (get-tables))) ; to be different in error output
	(partial = "first_table") (find-table :first_table))))

(deftest test-drop-table
  (in-test-environment
   (create-table :mytable
		 ["id" "int" "primary key"]
		 ["name" "varchar(200)"])
   (are [x y] (x y)
	(partial = 1) (count (get-tables))
	true? (drop-table-if-exists :mytable)
	(partial = 0) (count (get-tables))
	(partial = [nil nil]) (drop-table-if-exists :mytable :mytable2))))

(deftest test-table-operations
  (in-test-environment
   (create-table :mytable
		 ["id" "int" "primary key"]
		 ["name" "varchar(200)"])
   (is (= (count-rows :mytable) 0))
   (rename-table :mytable :itable)
   (is (= (find-table :itable) "itable"))
   (is (= (count (get-tables)) 1))))

(deftest test-setup-and-teardown
  (in-test-environment
   (let [schema {:name [["id" "int" "primary key"]
			["name" "varchar(200)"]]
		 :number [["id" "int" "primary key"]
			  ["name_id" "int"]
			  ["value" "int"]]}]
     (setup-tables schema)
     (is (= (count (get-tables)) 2))
     (is (and (find-table :name) (find-table :number)))
     (setup-tables schema) ;; does nothing
     (is (= (count (get-tables)) 2))
     (is (= [true true] (teardown-tables schema)))
     (is (= (count (get-tables)) 0)))))

(deftest test-insert-then-get-id
  (in-test-environment
   (create-table :name
		 [:id :serial :primary :key]
		 [:name "varchar(200)"])
   (create-table :other_pk
		 [:iden :serial :primary :key]
		 [:name "varchar(200)"])
   (is (= (insert-then-get-id :name {:name "HI"}) 1))
   (is (= (insert-then-get-id :name {:name "LO"}) 2))
   (is (= (insert-then-get-id :other_pk {:name "TEST"} :iden) 1))))

(deftest test-relate-to-one
  (in-test-environment
   (let [schema {:name [["id" "int" "primary key"]
			["name" "varchar(200)"]]
		 :number [["id" "int" "primary key"]
			  ["name_id" "int"]
			  ["value" "int"]]}]
     (setup-tables schema)
     (is (= (count (get-tables)) 2))
     )))