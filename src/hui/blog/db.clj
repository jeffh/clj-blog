(ns #^{:author "Jeff Hui"
       :doc "Provides PostgreSQL interface."}
  hui.blog.db
  (:require [clojure.contrib.sql :as sql])
  (:use compojure))

(def db {:classname "org.postgresql.Driver"
	 :subprotocol "postgresql"
	 :subname "production"
	 :user "postgres"})

(defn sql-query
  "Executes a given query and returns results in a list."
  [query]
  (sql/with-connection db
    (sql/with-query-results res [query] (into [] res))))