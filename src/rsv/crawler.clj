(ns rsv.crawler
  (:require
   [net.cgrand.enlive-html :as html]
   [clojure.math.numeric-tower :as math]
   [clojure.java.jdbc :as sql]
   [clojurewerkz.urly.core :as urly]))
(use 'rsv.util)
(use 'rsv.parser)
(def default-refresh 120)

;(sql/db-do-commands db "drop table rsventry")
;(sql/db-do-commands db "drop table rsvpage")
(try
(sql/db-do-commands
 db
 (sql/create-table-ddl
  :rsvEntry
  [:url :text]
  [:time :timestamp "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"]
  [:revid :int]
  [:pathHash :int]
  [:entryHash :int]
  [:data :text]
  [:rss :text]
   ) "alter table rsventry add CONSTRAINT uc3 UNIQUE(url,pathHash,entryHash)")
  (catch Exception e (.getNextException e)))
(try
(sql/db-do-commands
 db
 (sql/create-table-ddl
  :rsvPage
  [:url :text]
  [:hashUrl :int]
  [:time :timestamp "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"]
  [:revid :int]
  [:change :int]
  [:minlapse :int]
  [:lapse :int]
  [:data :text]
   ) "alter table rsvpage add CONSTRAINT pageid UNIQUE(url,revid)")
  (catch Exception e (.getNextException e)))


(defn row2description [row] (html/text {:tag :p :content (interleave (map val row) (repeat " "))}))
(defn atag [element] (str "<a href=\"" ((element :attrs) :href) "\">" (-> element :content first) "</a>"))
(defn row2links [url row]
  (map #(update-in % [:attrs :href] (partial to-absolute-url url))
       (filter #(= :a (:tag %)) (map val row)))
)
(defn abc-text [s] (clojure.string/replace s #"[0123456789]" ""))
(defn rss-item [url row]
  (let [text (row2description row) links (row2links url row)]
    (str "<item> <title>" (subs text 0 (min (count text) 100)) "</title>"
     "<description><![CDATA[" (apply str (interleave (conj (map atag links) text) (repeat "<br/>"))) "]]></description>"
     "<pubDate>" (.format (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z")  (new java.util.Date)) "</pubDate>"
     "<link><![CDATA[" (:href (:attrs (first links))) "]]></link>"
     "<guid><![CDATA[" (:href (:attrs (first links))) "]]></guid></item>" "\n")
    ))

(defn get-element [c parser]
  (map
  (fn [parser] {(hash [(:mod parser) (:tail parser)]) (rsv.util/traverse (nth c (:mod parser)) (:tail parser))})
   (:children parser))
)

(defn get-row [c parsers]
  (reduce merge {} (mapcat #(get-element c %) parsers)))

(defn get-table [context R parser]
  (let [psize (:delta parser)]
    (map
     #(let [data (get-row % (:children parser)) rss (rss-item (:url context) data)]
        {:data data :rss rss :hash (hash (abc-text (row2description data)))})
     (partition psize psize {} (R :content))
     )))

(defn get-tables [context s1 parser]
  (mapcat #(let [r (:head %) R (rsv.util/traverse s1 r)]
          (map (fn [x] (assoc x :table (hash r))) (get-table (assoc context :table r) R %)))
       (:children parser))
  )

(defn get-urls[] (sql/query
         db
         ["SELECT url,max(revid) as revid,max(time+cast( lapse||' secs' as interval)) as nextTime, now() as now
          FROM rsvPage
          GROUP BY url"]))

(defn get-lapse-crawl [urls]
   (reduce min default-refresh
      (map #(- (.getTime (:nexttime %)) (.getTime (:now %))) urls)))

(defn my-rand-nth [L]
  (if (nil? L)  nil
    (if (empty? L) nil (rand-nth L))))

(defn pick-one-url [urls] (my-rand-nth (filter #(< (.getTime (:nexttime %)) (.getTime (:now %))) urls)))

(defn get-page [url-item]  (last (sql/query
         db
         ["SELECT *
          FROM rsvPage
          where url = ? and revid = ?" (:url url-item) (:revid url-item)])))

(defn get-sql-items [s1 s2 url]
  (get-tables {:url url} s1
              (rsv.parser/get-parsers s1 s2)))



(defn count-new [sql-items page]
  (try (doall (map identity sql-items)) (catch Exception e 0));;force sequence to be realized
  (apply + (doall (map (fn [x]
  (try (sql/insert! db :rsvEntry
             {:url (:url page)
              :revid (inc (:revid page))
              :pathHash (:table x)
              :entryHash (:hash x)
              :data (pr-str (:data x))
              :rss (:rss x)
              }) 1
    (catch Exception e 0)
)) sql-items))))

(defn visit! [url-item]
  (println "Visiting" url-item)
  (let [page (get-page url-item)
        s1 (rsv.util/fetch-url (:url page))
        sql-items (get-sql-items s1 (read-string (:data page)) (:url page))
        nrows (count-new sql-items page)]
    (println nrows " new items.")
    (println (map :rss sql-items))
    (sql/insert! db :rsvPage
             {:url (:url page)
              :hashUrl (:hashUrl page)
              :revid (inc (page :revid))
              :change nrows
              :minlapse (page :minlapse)
              :lapse (page :minlapse)
              :data (pr-str s1)
              }))
  (println "Done visiting" url-item))

(defn refresh-once! []
  (println "Refreshing...")
  (let [urls (get-urls)
        lapse (get-lapse-crawl urls)
        url-item (pick-one-url urls)
        ]
    (if (nil? url-item)
      (do (println "Waiting " lapse) (Thread/sleep (* 1000 lapse)))
      (try
        (visit! url-item)
        (catch Exception e
          (println "ERROR: "(.getMessage e))
          ;(.printStackTrace e)
          )))
   ))
;;(refresh-once!)
(println "Starting loop:")
(future (while true (refresh-once!)))

;; (let [urls (get-urls)
;;         lapse (get-lapse-crawl urls)
;;         url-item (pick-one-url urls)]
;;         (def page (get-page url-item))
;;        (def s1 (rsv.util/fetch-url (:url page)))
;;        (def s2 (read-string (:data page)))
;;       (def sql-items (get-sql-items s1 s2 (:url page)))
;;     (def nrows (count-new sql-items page))
;;   url-item
;;      )


;;  (def pattern (rsv.parser/get-parsers s1 s2))

;; (map :rss (get-tables {:url (:url page)} s1 pattern))
;; (:head pattern)
;; (traverse s1 [0 1 0 0 2 0 0 0])
;; (let
;;     [nodes1 (-> s1 tree2paths keep-unique)
;;      nodes2 (-> s2 tree2paths keep-unique)
;;      path-pairs (filter check-pair (vals (group-by :id (concat nodes1 nodes2))))
;;      pattern-pairs (map pathpair2pattern path-pairs)
;;      pattern1 (-> nodes1 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s1))
;;      pattern2 (-> nodes2 enumerate-headtail group-headtail enumerate-pattern (score-child-pattern s2))
;;      patterns (sum-score-child (concat pattern1 pattern2))
;;      validated-patterns (sum-score-child (concat patterns (mapcat #(filter (partial pattern-pair-match? %) patterns) pattern-pairs)))
;;      ]
;;     (-> validated-patterns score-mod-pattern score-delta-pattern score-table score-page)
;;   (filter #(= [1 4] (:tail %)) patterns)
;;   )
;; (get-tables {:url ""} s1
;;               )
;; (traverse s1 [0 1 0 0 2 0 0 34 1 4])
;; (traverse s2 [0 1 0 0 2 0 0 88 1 4])
;; (def url "https://news.ycombinator.com/active")
;; (def s1 (fetch-url url))
;; (def pattern
;;   {:score 13/23, :children [{:score 13/23, :children [{:score 29/92, :children [{:score 29/92, :mod 0, :delta 3, :tail [2 0], :head [1 0 0 2 0 0]} {:score 29/92, :mod 0, :delta 3, :tail [2], :head [1 0 0 2 0 0]} {:score 29/92, :mod 0, :delta 3, :tail [0], :head [1 0 0 2 0 0]}], :mod 0, :delta 3, :head [1 0 0 2 0 0]} {:score 1/4, :children [{:score 1/4, :mod 1, :delta 3, :tail [1 0], :head [1 0 0 2 0 0]} {:score 15/46, :mod 1, :delta 3, :tail [1], :head [1 0 0 2 0 0]} {:score 29/92, :mod 1, :delta 3, :tail [1 2], :head [1 0 0 2 0 0]}], :mod 1, :delta 3, :head [1 0 0 2 0 0]}], :delta 3, :head [1 0 0 2 0 0]}], :head [1 0 0 2 0 0]}
;;   )

;; (-> pattern :children first :children second)
;; (def R (rsv.util/traverse s1 (:head pattern)))
;; (get-row (second (partition 3 3 {} (R :content))) (-> pattern :children first :children first :children))
;; (-> pattern :children first :children)
;; (def res (get-tables {:url url} s1 pattern))
;; (get-tables {:url url} s1 pattern)
;; (-> res first :rss)


;; (def url0 "https://news.ycombinator.com/active")
;; (def url1 "https://news.ycombinator.com/")
;; (try
;; (sql/insert! db :rsvPage
;;              {:url url0
;;               :hashUrl (hash url0)
;;               :revid 0
;;               :change 0
;;               :minlapse 300
;;               :lapse 300
;;               :data (pr-str (fetch-url url1))
;;               })
;; (catch Exception e (.getNextException e)))
