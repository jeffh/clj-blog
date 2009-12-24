(ns hui.blog.html
  (:use compojure
	compojure.validation.predicates)
  (:import java.util.Date))

(defn base-html
  [title header sidebar body footer & [style]]
  (html
   (doctype :html5)
   [:html
    [:head
     [:title (escape-html title)]
     [:link {:rel :stylesheet :type "text/css"
	     :href (or style "/public/style.css")}]]
    [:body
     [:div#ct
      [:div#hd header]
      [:div#sb sidebar]
      [:div#bd body]
      [:div#ft footer]]]]))

(defn header-html
  [title subtitle]
  (html [:h1
	 [:a {:href "/"} (str title)]
	 [:span (str subtitle)]]))
;	 [:a {:href "/rss" :class :rss}
;	  [:img {:src "/public/feed-icon-28x28.png"
;		 :title "Subscribe to RSS Feed"
;		 :alt "Subscribe to RSS"}]]]))

(defn sidebar-box
  [title-html contents-html]
  (html [:div.box [:h2 (str title-html)] [:div.sub (str contents-html)]]))

(defn sidebar-html
  [about-html & [boxes-map]]
  (html
   [:div.profile
    [:h2 "About"]
    [:div.sub (str about-html)]]
   (map sidebar-box (keys boxes-map) (vals boxes-map))))

(defn post-html
  [post]
  (html
   [:div.post
    [:h2
     [:a {:href (str "/" (:slug post) "/") :class :perma}
      (escape-html (:title post))]
     [:span (str (:published post))]
     [:a {:href (str "/" (:slug post) "/#comments")
	  :class :clink} "comments"]]
    [:div.sub (:body post)]]))

(defn posts-html
  [posts]
  (if (empty? posts)
    (html [:div.no-posts [:h2 "Hey pal, nothing here yet."]
	   [:div.sub [:p "Check back in a few days, thanks!"]]])
    (html (map post-html posts))))

(defn post-validate [params]
  (validation
   params
   [:title present? "title must not be blank"]
   [:body present? "body must not be blank"]
   [:target present? ""]))
(defn post-form [siteid]
  (html
   (form-to [:post "/new/"]
    (error-summary)
    (decorate-fields error-class
      [:p (text-field :title)]
      [:p (text-area {:cols 40 :rows 10} :body)]
      [:p
       (hidden-field :target siteid)
       (submit-button "Post")]))))

(defn footer-html
  [author]
  (html [:div#ft
	 [:p "&copy; 2009 " (str author) ". Powered by Clojure-Blog."]]))

(defn index-page
  [site author posts]
  (let [title (escape-html (:title site))]
    (base-html
     title
     (header-html title "Streaming Tidbits")
     (sidebar-html "I've been too busy programming to write one up :(")
     (str
      (post-form (:_id site))
      (posts-html posts))
     (footer-html "Jeff Hui"))))