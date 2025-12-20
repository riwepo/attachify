(ns attachify.fastmail
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
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

(defn fetch-email-ids
  [api-url access-token account-id mailbox-id]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [
                     ["Email/query"
                      {
                       :accountId account-id,
                       :filter    {:inMailbox mailbox-id}}
                      "a"]]}
        response (http/post api-url
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        body (:body response)
        method-response (first (:methodResponses body))
        method-response-data (second method-response)
        email-ids (:ids method-response-data)]
    email-ids))

(defn fetch-email-by-id
  [api-url access-token account-id email-id]
  (let [headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Email/get"
                      {:accountId account-id
                       :ids       [email-id]}
                      "a"]]}
        response (http/post api-url
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        body (:body response)
        method-response (first (:methodResponses body))
        method-response-data (second method-response)
        emails (:list method-response-data)]
    (first emails))) ;; return the single email map

(defn inline-image-blob-ids
  [email]
  (->> (:attachments email)
       (filter #(and
                  (= "inline" (:disposition %))
                  (str/starts-with? (:type %) "image/")))
       (map :blobId)
       (remove nil?)
       (into [])))

(defn fetch-blob
  [api-url access-token account-id blob-id]
  (let [headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Blob/get"
                      {:accountId account-id
                       :ids       [blob-id]}
                      "a"]]}
        response (http/post api-url
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        body (:body response)
        method-response (first (:methodResponses body))
        method-response-data (second method-response)
        blobs (:list method-response-data)]
    (println response)
    (first blobs))) ;; returns a map with blob metadata and (usually) a 'blob' or 'data' field with content

(comment
  (def session (fetch-session my-auth-url my-access-token))
  (pprint session)
  (def account-id (get-account-id session))
  (println account-id)
  (def inbox-id (fetch-inbox-id my-api-url my-access-token account-id))
  (println inbox-id)
  (def email-ids (fetch-email-ids my-api-url my-access-token account-id inbox-id))
  (def email-id (second email-ids))
  (println email-id)
  (def email (fetch-email-by-id my-api-url my-access-token account-id email-id))
  (pprint email)
  (def blob-ids (inline-image-blob-ids email))
  (def blob-id (first blob-ids))
  (print blob-id)
  (def blob (fetch-blob my-api-url my-access-token account-id blob-id))
  (println blob)
  nil)

