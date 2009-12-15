(ns hui.blog.utils
  "Provides utility functions. All functions in this namespace are pure."
  (:use compojure
	[clojure.contrib.java-utils :only (as-str)]
	[clojure.contrib.str-utils :only (re-split re-gsub)]
	clojure.test)
  (:import java.net.URL
	   java.util.regex.Pattern))

(defn character?
  "Returns true if the given value is a Character class."
  [c] (instance? Character c))

(defn re-compile
  "Merges a series of parameters as a string and concats them together as a
  single string for Pattern.compile."
  [& strings] (Pattern/compile (apply str strings)))

(defn re-quote
  "Escapes a given regex string so that its contents are safe from regex
  evaluation. This is good for escaping user input from injecting custom
  regex in your own expressions."
  [s] (Pattern/quote s))

(defn re-safe
  "Creates a Pattern instance where the given string is matched directly
  without any regular expressions. Good for escaping user input from
  injecting regular expressions."
  [s] (Pattern/compile (re-quote s)))

(defn merge-url
  "Merges relative url fragments. Ensures there are no duplicate slashes (/)
  with the exception of http[s]:// at the start of the string"
  {:test (fn []
	   (are [x y] (= x (apply merge-url y))
		"http://google.com/"
		["http://" "google.com"],
		"https://google.com/search/"
		["https://" "google.com" "/search"],
		"localhost:80/wtf/ya/"
		["localhost" ":80" "/wtf/" "ya"]))}
  [& fragments]
  (let [url-frags (filter #(not= % "")
			  (re-split #"/" (reduce as-str fragments)))
	str-url (reduce str (interleave url-frags (repeat "/")))]
    (re-gsub #"^([hH][tT]{2}[pP][sS]?):" "$1:/" str-url)))

(defn absolute-url
  "Generates the root of the url for this incoming request or takes an
  absolute url without the server/domain and appends the information to
  it."
  {:test (fn []
	   (let [headers (fn [scheme name port]
			   {:scheme scheme
			    :server-name name
			    :server-port port})
		 h1 (headers "http" "localhost" 88)
		 h2 (headers "https" "google.com" 80)
		 h3 (headers "http" "domain.com" 80)]
	     (are [x y] (= x (apply absolute-url y))
		  "http://localhost:88/" [h1]
		  "http://localhost:88/test/" [h1 :test]
		  "http://localhost:88/test/1/" [h1 "test/" "1/"]
		  "https://google.com/" [h2]
		  "http://domain.com/" [h3])))}
  [request & relative-url-fragments]
  (apply merge-url
   (str (URL. (as-str (:scheme request))
	      (:server-name request)
	      (if (= (:server-port request) 80) -1 (:server-port request))
	      "/"))
   relative-url-fragments))

(defn str-blank?
  "Returns true if the string is empty or has only whitespace characters."
  {:test (fn []
	   (are [x y] (x (str-blank? y))
		true? "", true? " \t\n", false? "hi"))}
  [s]
  (every? #(Character/isWhitespace %) s))

(defn isUpperCase?
  "Returns true if the string is in all uppercase."
  {:test (fn []
	   (are [x y] (x (isUpperCase? y))
		true? "A", true? "HELLO",
		false? "b", false? "falsy"))}
  [s] (let [s (str s)] (= s (.toUpperCase s))))