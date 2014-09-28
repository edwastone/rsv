(ns rsv.util
  (:require
   [net.cgrand.enlive-html :as html]
   [clojure.math.numeric-tower :as math]
   [clojure.java.jdbc :as sql]
   [clojurewerkz.urly.core :as urly]
   [ring.util.response :refer [redirect]]))

(def ^:dynamic *user-agent*
  "Mozilla/5.0 (fake User-Agent)")
(def host "localhost:3000")

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#()))

(defn fetch-url [url]
  (with-open [inputstream (-> (java.net.URL. url)
                              .openConnection
                              (doto (.setRequestProperty "User-Agent"
                                                         *user-agent*))
                              .getContent)]
    {:tag "root" :content (html/html-resource inputstream)}))
(defn to-absolute-url [u0 u1]
  (let [ur0 (urly/url-like u0) ur1 (urly/url-like u1)]
    (if (urly/absolute? ur1) (str ur1)
    (.mutateQuery (.mutatePath ur0 (urly/path-of ur1)) (urly/query-of ur1))))
  )
(def db {:subprotocol "postgresql"
               :subname "//localhost:5432/ubuntu"
               :user "ubuntu"
               :password "ubuntu"})
(defn filter-length-one [dict]
  (into {}
        (map #(update-in % [1] first)
        (filter #(= 1 (count (% 1))) dict))))
(defn filter1return2 [f L1 L2] (filter (comp f first) (map list L1 L2)))
(defn every-second [path] (mapv second (partition 2 path)))
(defn traverse [T P]
  (reduce #(if (or (= %1 nil) (>= %2 (count (:content %1)))) nil (nth (:content %1) %2)) (cons T P)))

(defn rss-handler [page-id]
  (let [existing-copy (sql/query
         db
         ["SELECT *
          FROM rsvPage
          WHERE hashUrl = ? and
          revid = (SELECT max(revid) FROM rsvPage WHERE hashUrl=?)"
          (int (bigdec page-id))
          (int (bigdec page-id))
         ])]
    (if (empty? existing-copy) (redirect "/")
      (let [p (first existing-copy) url (p :url) s1 (read-string (p :data)) date (p :time)]
      {:status 200
       :headers {"Content-Type" "text/html;charset=UTF-8"}
       :body
       (str
    "<?xml version=\"1.0\"?>
    <rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">
      <channel>
        <title>" (-> (html/select s1 [:title]) first :content first) "</title>
        <link>" url "</link>
        <description>RSS feed auto-generated</description>
        <language>en-ca</language>
        <pubDate>" (.format (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z")
                            (new java.util.Date (.getTime date))) "</pubDate>
          <atom:link href=\"" host "/page/" page-id "/rss\" rel=\"self\" type=\"application/rss+xml\"/> "
        (apply str (map
     (fn [r] (:rss r))
     (sql/query db ["SELECT rss FROM rsventry WHERE url = ?" url])))
        "</channel>
    </rss>"
        )}))))
