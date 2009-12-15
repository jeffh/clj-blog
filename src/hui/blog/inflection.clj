(ns hui.blog.inflection
  "Enables english-based inflection capabilities such as pluralization."
  (:use [hui.blog.utils :only (isUpperCase? re-compile)]
	[clojure.contrib.str-utils :only (re-gsub re-partition)]
	[clojure.contrib.except :only (throw-arg)]
	clojure.test))

;; words that can't be pluralized: eg - money
(def uncountable-words (ref #{}))
;; this is like (seq {}) form, that is: ((key val) (key val) .. etc)
;; maps singular => plural
(def plural-mapping (ref '()))
;; maps plural => singular
(def singular-mapping (ref '()))

(defn add-uncountable-inflection
  "Adds a sequence of words to the immutable pluralization/singularization
  list. These words do not have plural:singular mappings. Eg - money."
  [& words] (dosync (alter uncountable-words into words)))
(defn remove-uncountable-inflection
  "Removes a sequence of words from the uncountable list; most likely
  because the word was moved to mapping sequence."
  [& words] (dosync (ref-set uncountable-words
			     (remove (set words) @uncountable-words))))

(defn- convert-to-regex
  "Converts vectors and strings into patterns for inflection functions."
  [value] (cond (vector? value) (apply re-compile value)
		(string? value) (re-compile "(?i)" value "$")
		true value))

(defn- inflection-args-valid
  "Validates there are even number of args greater than 1. Used for
  inflection functions."
  [x y pairs] (and x y (= (mod (count pairs) 2) 0)))

(defn add-plural-inflection
  "Adds a normal regex singular to plural mapping."
  [singular-re plural-replace & more-pairs]
  (if-not (inflection-args-valid singular-re plural-replace more-pairs)
    (throw-arg "Invalid singular/plural pair(s)."))
  (let [singular-re (convert-to-regex singular-re)
	plural-replace (apply str plural-replace)]
    (dosync (alter plural-mapping conj [singular-re plural-replace]))
    (if more-pairs (let [[s p & r] more-pairs] (recur s p r)))))

(defn add-singular-inflection
  "Adds a normal regex plurla to singular mapping."
  [plural-re singular-replace & more-pairs]
  (if-not (inflection-args-valid plural-re singular-replace more-pairs)
    (throw-arg "Invalid singular/plural pair(s)."))
  (let [plural-re (convert-to-regex plural-re)
	singular-replace (apply str singular-replace)]
    (dosync (alter singular-mapping conj [plural-re singular-replace]))
    (if more-pairs (let [[s p & r] more-pairs] (recur s p r)))))

(defn add-irregular-inflection
  "Adds an irregular mapping of plural:singular words. This is for words
  whose pluralizations is abnormally different from normal rules."
  [singular plural & more-pairs]
  (if-not (inflection-args-valid singular plural more-pairs)
    (throw-arg "Invalid singular/plural pair(s)."))
  (dosync
   (remove-uncountable-inflection singular plural)
   (let [[fsing & rsing] (str singular)
	 [fplur & rplur] (str plural)
	 [fsing fplur] [(str fsing) (str fplur)]
	 all (map #(apply str %) [fsing rsing fplur rplur])
	 [fsing rsing fplur rplur] all]
     (if (and (isUpperCase? fsing) (isUpperCase? fplur))
       (do
	 (add-plural-inflection
	  ["(?i)(" fsing ")" rsing "$"] ["\1" rplur]
	  ["(?i)(" fplur ")" rplur "$"] ["\1" rplur])
	 (add-singular-inflection
	  ["(?i)(" fplur ")" rplur "$"] ["\1" rsing]))
       (let [upper-and-lower (fn [x] [(.toUpperCase x) (.toLowerCase x)])
	     firsts (into (upper-and-lower fsing) (upper-and-lower fplur))
	     [ufsing lfsing ufplur lfplur] firsts]
	 (apply add-plural-inflection
		(interleave
		 (map (fn [x y] [x "(?i)" y])
		      firsts [rsing rsing rplur rplur])
		 (map (fn [x] [x rplur]) firsts)))
	 (add-singular-inflection
	  [ufplur "(?i)" rplur] [ufsing rsing]
	  [lfplur "(?i)" rplur] [lfsing rsing])))))
  (if more-pairs (let [[s p & r] more-pairs] (recur s p r))))

(defn- create-mapping
  "Generates a function that can be used for pluralizing or singularizing."
  [map-ref]
  (fn [word] 
    (or (some #(and (.equalsIgnoreCase % word) word) @uncountable-words)
	(loop [[rule repl] (first @map-ref)
	       r (rest @map-ref)]
	  (let [new (re-gsub rule repl word)]
	    (if (> (count (re-partition rule word)) 1)
	      (re-gsub rule repl word)
	      (recur (first r) (rest r))))))))

(defmacro defmapping
  "Syntantic sugar for creating a pluralize/singular mapping."
  {:private true}
  [symbol m-ref doc & tests]
  `(def #^{:doc ~doc :test (fn [] ~@tests)} ~symbol (create-mapping ~m-ref)))

(defmapping pluralize plural-mapping
  "Pluralizes a given word. Does nothing if the word is already plural."
  (are [x y] (= x (pluralize y))
       "pizzas" "pizza"
       "matrices" "matrix"
       "cakes" "cake"
       "Oranges" "Orange"
       "apples" "apples"
       "money" "money"))

(defmapping singularize singular-mapping
  "Singulizes a given word. Does nothing if the word is already singular."
  (are [x y] (= x (pluralize y))
       "pizza" "pizzas"
       "matrix" "matrices"
       "cake" "cakes"
       "Orange" "Oranges"
       "apple" "apple"
       "money" "money"))

;;;;; pluralization / singularization knowledge base from RAILs ;;;;;
(add-plural-inflection ;; "(?i) is prepended and $ is postfixed to RE
 "" "s"
 "s" "s"
 "(ax|test)is" "$1es"
 "(octop|vir)us" "$1i"
 "(alias|status)" "$1es"
 "(bu)s" "$1ses"
 "(buffal|tomat)o" "$1oes"
 "([ti])um" "$1a"
 "sis" "ses"
 "(?:([^f])fe|([lr])f)" "$1$2ves"
 "(hive)" "$1s"
 "([^aeiouy]|qu)y" "$1ies"
 "(x|ch|ss|sh)" "$1es"
 "(matr|vert|ind)(?:ix|ex)" "$1ices"
 "([m|l])ouse" "$1ices"
 "(quiz)" "$1zes")

(add-singular-inflection
 "s" ""
 "(n)ews" "$1ews"
 "([ti])a" "$1um"
 "((a)naly|(b)a|(d)iango|(p)arenthe|(p)rongo|(s)ynop|(t)he)ses" "$1$2sis"
 "(^analy)ses" "$1sis"
 "([^f])ves" "$1fe"
 "(hive)s" "$1"
 "(tive)s" "$1"
 "([lr])ves" "$1f"
 "([^aeiouy]|qu)ies" "$1y"
 "(s)eries" "$1eries"
 "(m)ovies" "$1ovie"
 "(x|ch|ss|sh)es" "$1"
 "([m|l])ice" "$1ouse"
 "(bus)es" "$1"
 "(o)es" "$1"
 "(shoe)s" "$1"
 "(cris|ax|test)es" "$1"
 "(octop|vir)i" "$1us"
 "(alias|status)es" "$1"
 "(ox)en" "$1"
 "(vert|ind)ices" "$1ex"
 "(matr)ices" "$1ix"
 "(quiz)zes" "$1"
 "(database)s" "$1")

(add-irregular-inflection
 "person" "people"
 "man" "men"
 "child" "children"
 "move" "moves"
 "cow" "kine")

(add-uncountable-inflection
 "equipment" "information" "rice" "money" "species" "series" "fish" "sheep")
