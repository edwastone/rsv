(ns rsv.handler
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [ring.util.response :refer [resource-response response redirect]]
            [ring.middleware.params :refer [wrap-params]]
            [rsv.register]
            [rsv.util]
            [rsv.crawler]
            ))

(defroutes app-routes
  (GET  "/" [] (resource-response "index.html" {:root "public"}))
  (POST  "/submit" [url minlapse]
         (redirect (rsv.register/prepare-url url minlapse)))
  (compojure.core/context "/page/:page-id" [page-id]
           (GET "/" [] (response (str "<a href=\"/page/" page-id "/rss\">RSS</a>")))
           (GET "/rss" [] (rsv.util/rss-handler page-id) ))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site app-routes)
      (wrap-params)))

;;TODO
;;Thread to crawl the database, sleep, and crawl again. runs indefinitely
