(ns hui.blog.db
  "Provides database functionality for the blog."
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (str-join chop re-split re-gsub)]
	clojure.contrib.except
	compojure))

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

;; imports all vars as local vars to this namespace,
;; including other files that use or require this file.
;; This allows access to func & vars in other libraries
;; through this file.
;;
;; For documentation of functions provided by clojure.contrib.sql:
;;   http://richhickey.github.com/clojure-contrib/sql-api.html
(immigrate 'clojure.contrib.sql)
(immigrate 'hui.blog.db.core) ; general-purpose db code
(immigrate 'hui.blog.db.schema) ; schema management
(immigrate 'hui.blog.db.lang) ; high-level DSL for this blog

(def access
     {:reader 0
      ;; add other access-levels if schema needs be
      :admin 9001}) ; its over 9000!

(if-not (valid-connection?)
  (println "Warning: Failed to connect to DB. (Check DB configs?)"))