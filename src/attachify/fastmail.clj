(ns attachify.fastmail
  (:require [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [cheshire.core :as json]))

(def my-hostname "api.fastmail.com")                        ;
(def my-username "attachify")
(def my-auth-url (str "https://" my-hostname "/.well-known/jmap"))
(def my-api-url "https://api.fastmail.com/jmap/api/")
(def my-access-token "fmu1-5c164056-5db4226acdc2c14ad1008fecc8082146-0-63118a547fec2b239a083d31bb977231")

(defn fetch-session
  [auth-url access-token]
  (let [headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        response (http/get auth-url {:headers headers :as :json})]
    (:body response)))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn fetch-inbox-id
  [api-url access-token account-id]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [
                     ["Mailbox/query"
                      {
                       :accountId account-id,
                       :filter    {:role "inbox"}}
                      "a"]]}

        response (http/post api-url
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        response-body (:body response)
        method-response (first (:methodResponses response-body))
        query-data (second method-response)
        inbox-id (first (:ids query-data))]
    (println "Method response:" method-response)
    (println "Query data:" query-data)
    (println "Inbox id:" inbox-id)
    inbox-id))

(defn fetch-inbox-emails
  [api-url access-token account-id inbox-id]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        ;; Step 1: Query email IDs in Inbox
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [
                     ["Email/query"
                      {
                       :accountId account-id,
                       :filter    {:inMailbox inbox-id}}
                      "a"]]}
        response (http/post api-url
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})]
    (println headers)
    (println query-body)
    (println "Response status:" (:status response))
    (println "Response headers:" (:headers response))
    (println "Response body (string):" (:body response))))

(comment
  (def session (fetch-session my-auth-url my-access-token))
  (pprint session)
  (def account-id (get-account-id session))
  (println account-id)
  (def inbox-id (fetch-inbox-id my-api-url my-access-token account-id))
  (println inbox-id)
  (fetch-inbox-emails my-api-url my-access-token account-id inbox-id)
  nil)

