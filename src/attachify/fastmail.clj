(ns attachify.fastmail
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(def hostname "api.fastmail.com")                           ;
(def username "attachify")
(def authUrl (str "https://" hostname "/.well-known/jmap"))


(def my-api-url "https://api.fastmail.com/jmap/api/")
(def my-access-token "fmu1-5c164056-5db4226acdc2c14ad1008fecc8082146-0-63118a547fec2b239a083d31bb977231")

(defn get-session
  [auth-url access-token]
  (let [headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        response (http/get auth-url {:headers headers :as :json})]
    (:body response)))

(defn fetch-inbox-emails
  [api-url access-token]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        ;; Step 1: Query email IDs in Inbox
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls [
                                  ["Email/query"
                                   {
                                    :accountId "accountId",
                                    :filter   { :inMailbox "inboxId" },
                                    :sort     [ { :property "receivedAt", :isAscending false } ],
                                    :limit    10}
                                   ,
                                   "a"]]}


        response (http/post api-url
                            {:headers headers
                             :body (json/encode query-body)
                             :as :auto})]

    (println headers)
    (println query-body)
    (println "Response status:" (:status response))
    (println "Response headers:" (:headers response))
    (println "Response body (string):" (:body response))))



(comment
    (fetch-inbox-emails my-api-url my-access-token)
  nil)

