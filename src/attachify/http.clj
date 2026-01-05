(ns attachify.http
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.telemere :as tel]
            [attachify.result :refer [success]]
            [attachify.result-log :refer [log-and-success log-and-failure]]))

(defn get2 [url api-token]
  (let [headers {"Authorization" (str "Bearer " api-token)
                 "Content-Type"  "application/json; charset=utf-8"}]
    (try
      (let [response (http/get url {:headers headers :as :json})
            status (:status response)]
        (if (= status 200)
          (do
            (log-and-success)
            (success (:body response)))
          (log-and-failure (str "Error " status " http/get URL " url))))
      (catch Exception e
        (log-and-failure (str "Error http/get URL " url " " (.getMessage e)))))))

(defn post2 [url api-token body]
  (let [headers {"Authorization" (str "Bearer " api-token)
                 "Content-Type"  "application/json; charset=utf-8"}]
    (try
      (let [response (http/post url {:headers headers
                                     :body    (json/encode body)
                                     :as      :auto})
            status (:status response)]
        (if (= status 200)
          (do
            (log-and-success)
            (success (:body response)))
          (log-and-failure (str "Error " status " http/post URL " url))))
      (catch Exception e
        (log-and-failure (str "Error http/post URL " url " " (.getMessage e)))))))