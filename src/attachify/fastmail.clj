(ns attachify.fastmail
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clj-http.client :as http]
            [cheshire.core :as json])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(def my-hostname "api.fastmail.com")                        ;
(def my-auth-url (str "https://" my-hostname "/.well-known/jmap"))
(def my-access-token "fmu1-5c164056-5db4226acdc2c14ad1008fecc8082146-0-63118a547fec2b239a083d31bb977231")

(defn url-encode [data]
  ;; Use URLEncoder/encode with the UTF-8 charset
  (URLEncoder/encode data StandardCharsets/UTF_8))

(defn fetch-session
  [auth-url access-token]
  (let [headers {"Authorization" (str "Bearer " access-token)
                 "Content-Type"  "application/json; charset=utf-8"}
        response (http/get auth-url {:headers headers :as :json})]
    (:body response)))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn get-download-url [session]
  (:downloadUrl session))

(defn get-api-url [session]
  (:apiUrl session))

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

(defn download-blob
  [download-url-template access-token account-id blob-id content-type filename]
  (let [filename (or filename "file")
        encoded-filename (url-encode filename)
        encoded-type (url-encode content-type)
        download-url (-> download-url-template
                         (str/replace "{accountId}" account-id)
                         (str/replace "{blobId}" blob-id)
                         (str/replace "{name}" encoded-filename)
                         (str/replace "{type}" encoded-type))
        headers {"Authorization" (str "Bearer " access-token)}]
    (println "Downloading blob from URL:" download-url)
    (http/get download-url {:headers headers :as :byte-array})))

(defn fetch-first-inline-image-blob-id
  [auth-url access-token]
  (let [session (fetch-session auth-url access-token)
        account-id (get-account-id session)
        download-url (get-download-url session)
        api-url (get-api-url session)
        inbox-id (fetch-inbox-id api-url access-token account-id)
        email-ids (fetch-email-ids api-url access-token account-id inbox-id)
        email-id (second email-ids)
        email (fetch-email-by-id api-url access-token account-id email-id)
        blob-ids (inline-image-blob-ids email)
        blob-id (first blob-ids)]
    (println "Session:" session)
    (println "Account ID:" account-id)
    (println "Download URL:" download-url)
    (println "API URL:" api-url)
    (println "Inbox ID:" inbox-id)
    (println "Email IDs:" email-ids)
    (println "Selected Email ID:" email-id)
    (println "Email:" email)
    (println "Inline Image Blob IDs:" blob-ids)
    (println "Selected Blob ID:" blob-id)
    {:api-url api-url :download-url download-url :account-id account-id :inbox-id inbox-id :email-id email-id :blob-id blob-id}))

(comment
  (def session (fetch-session my-auth-url my-access-token))
  (pprint session)
  (def account-id (get-account-id session))
  (println account-id)
  (def download-url (get-download-url session))
  (println download-url)
  (def api-url (get-api-url session))
  (println api-url)
  (def inbox-id (fetch-inbox-id api-url my-access-token account-id))
  (println inbox-id)
  (def email-ids (fetch-email-ids api-url my-access-token account-id inbox-id))
  (def email-id (second email-ids))
  (println email-id)
  (def email (fetch-email-by-id api-url my-access-token account-id email-id))
  (pprint email)
  (def blob-ids (inline-image-blob-ids email))
  (def blob-id (first blob-ids))
  (print blob-id)
  nil
  (def my-data (fetch-first-inline-image-blob-id my-auth-url my-access-token))
  (println my-data)
  ;(download-blob (:download-url my-data) my-access-token (:account-id my-data) (:blob-id my-data) "email-attachment")
  nil)

