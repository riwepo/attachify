(ns attachify.http
  (:require [clj-http.client :as http]
            [attachify.result :refer [success]]
            [attachify.result-log :refer [log-and-success log-and-failure]]))

(defn get2 [url api-token data-type]
  ;; possible data types :text :json :byte-array
  (let [headers {"Authorization" (str "Bearer " api-token)}]
    (try
      (let [response (http/get url {:headers headers :as data-type})
            status (:status response)]
        (if (= status 200)
          (do
            (log-and-success nil "get2 succeeded")
            (success (:body response)))
          (log-and-failure "get2 failed" url status)))
      (catch Exception e
        (log-and-failure "get2 failed" url (.getMessage e))))))

(defn post2 [url api-token content content-type]
  ;; if content-type is json, the content must be json encoded
  (let [headers {"Authorization" (str "Bearer " api-token)
                 "Content-Type" content-type}]
    (try
      (let [response (http/post url {:headers headers
                                     :body    content
                                     :as      :auto})
            status (:status response)]
        (if (= status 200)
          (do
            (log-and-success nil "post2 succeeded")
            (success (:body response)))
          (log-and-failure "post2 failed" status url)))
      (catch Exception e
        (log-and-failure "post2 failed exception" url (.getMessage e))))))