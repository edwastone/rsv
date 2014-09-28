(ns rsv.register
  (:require
      [clojure.java.jdbc :as sql])
  )
(use 'rsv.util)

(defn prepare-url [url minlapse]
  (println url)
  (let [existing-copy (sql/query
         db
         ["SELECT url,max(revid)
          FROM rsvPage
          GROUP BY url
          HAVING url = ?" url]) minlapse (max 60 (* 60 (int (bigdec minlapse))))]
    (if (empty? existing-copy)
      (sql/insert! db :rsvPage
             {:url url
              :hashUrl (hash url)
              :revid 0
              :change 0
              :minlapse minlapse
              :lapse minlapse
              :data (pr-str (fetch-url url))
              })))
  (str "page/" (hash url))
)
