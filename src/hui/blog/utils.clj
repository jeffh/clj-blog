(ns hui.blog.utils
  "Provides utility functions. All functions in this namespace are pure."
  (:use compojure
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (re-split re-gsub)])
  (:import [java.net URL]))

(defn merge-url
  "Merges relative url fragments. Ensures there are no duplicate slashes (/)
  with the exception of http[s]:// at the start of the string"
  [& fragments]
  (let [url-frags (filter #(not= % "")
			  (re-split #"/" (reduce str fragments)))
	str-url (reduce str (interleave url-frags (repeat "/")))]
    (re-gsub #"^([hH][tT]{2}[pP][sS]?):" "$1:/" str-url)))

;(defn build-url
;  [& partials]
;  (in

(defn absolute-url
  "Generates the root of the url for this incoming request or takes an
  absolute url without the server/domain and appends the information to
  it."
  [request & relative-url-fragments]
  (apply merge-url
   (str (URL. (as-str (:scheme request))
	      (:server-name request)
	      (if (= (:server-port request) 80) -1 (:server-port request))
	      "/"))
   relative-url-fragments))

(defmacro enable-session-for
  "Enables session support for the given functions."
  [& funcs]
  `(decorate-with (with-session {:type :cookie, :expires 600}) ~@funcs))