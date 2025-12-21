(ns attachify.fastmail
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-http.client :as http]
            [cheshire.core :as json])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]))

(defn get-email-auth-url
  [config]
  (let [hostname (:email-hostname config)
        auth-url-template (:email-auth-url config)]
    (str/replace auth-url-template "{hostname}" hostname)))

(defn url-encode [data]
  ;; Use URLEncoder/encode with the UTF-8 charset
  (URLEncoder/encode data StandardCharsets/UTF_8))

(defn fetch-session
  [config]
  (let [headers {"Authorization" (str "Bearer " (:email-api-token config))
                 "Content-Type"  "application/json; charset=utf-8"}
        auth-url (get-email-auth-url config)
        response (http/get auth-url {:headers headers :as :json})
        session (:body response)]
    (assoc session :api-token (:email-api-token config))))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn get-download-url [session]
  (:downloadUrl session))

(defn get-api-url [session]
  (:apiUrl session))

(defn fetch-inbox-id
  [session]
  (let [
        headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [
                     ["Mailbox/query"
                      {
                       :accountId (get-account-id session),
                       :filter    {:role "inbox"}}
                      "a"]]}

        response (http/post (get-api-url session)
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
  [session mailbox-id]
  (let [
        headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [
                     ["Email/query"
                      {
                       :accountId (get-account-id session),
                       :filter    {:inMailbox mailbox-id}}
                      "a"]]}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        body (:body response)
        method-response (first (:methodResponses body))
        method-response-data (second method-response)
        email-ids (:ids method-response-data)]
    email-ids))

(defn fetch-email-by-id
  [session email-id]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Email/get"
                      {:accountId (get-account-id session)
                       :ids       [email-id]}
                      "a"]]}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        body (:body response)
        method-response (first (:methodResponses body))
        method-response-data (second method-response)
        emails (:list method-response-data)]
    (first emails))) ;; return the single email map

(defn inline-image-blobs
  [email]
  (->> (:attachments email)
       (filter #(and
                  (= "inline" (:disposition %))
                  (str/starts-with? (:type %) "image/")))
       (remove #(nil? (:blobId %)))
       (map #(hash-map :id (:blobId %)
                       :type    (:type %)))
       (into [])))

(def mime->ext
  {"image/jpeg" ".jpg"
   "image/png"  ".png"
   "image/gif"  ".gif"
   "image/svg+xml" ".svg"
   "application/pdf" ".pdf"})
   ;; add more mappings as needed

(defn add-extension-if-missing
  [filename ext]
  (if (or (str/blank? ext)
          (str/ends-with? filename ext))
    filename
    (str filename ext)))

(defn download-blob
  [session blob filename]
  (let [filename (or filename "file")
        ext (get mime->ext (:type blob))
        filename-with-ext (add-extension-if-missing filename ext)
        encoded-filename (url-encode filename-with-ext)
        encoded-type (url-encode (:type blob))
        download-url (-> (get-download-url session)
                         (str/replace "{accountId}" (get-account-id session))
                         (str/replace "{blobId}" (:id blob))
                         (str/replace "{name}" encoded-filename)
                         (str/replace "{type}" encoded-type))
        headers {"Authorization" (str "Bearer " (:api-token session))}]
    (println "Downloading blob from URL:" download-url)
    (http/get download-url {:headers headers :as :byte-array})))

(defn fetch-first-inline-image-blob
  [config]
  (let [session (fetch-session config)
        inbox-id (fetch-inbox-id session)
        email-ids (fetch-email-ids session inbox-id)
        email-id (second email-ids)
        email (fetch-email-by-id session email-id)
        blobs (inline-image-blobs email)
        blob (first blobs)]
    (println "Session:")
    (pprint session)
    (println "Inbox ID:" inbox-id)
    (println "Email IDs:" email-ids)
    (println "Selected Email ID:" email-id)
    (println "Email:" email)
    (println "Inline Image Blobs:" blobs)
    (println "Selected Blob:" blob)
    {:session session
     :inbox-id inbox-id
     :email-id email-id
     :blob blob}))

(comment
  (defn load-config
    []
    (-> "resources/config.edn"
        slurp
        edn/read-string))
  (def config (load-config))
  (def session (fetch-session config))
  (pprint session)
  (def inbox-id (fetch-inbox-id session))
  (println inbox-id)
  (def email-ids (fetch-email-ids session inbox-id))
  (def email-id (second email-ids))
  (println email-id)
  (def email (fetch-email-by-id session email-id))
  (pprint email)
  (def blobs (inline-image-blobs email))
  (def blob (first blobs))
  (print blob)
  nil
  (def my-data (fetch-first-inline-image-blob config))
  (pprint my-data)
  (def download (download-blob (:session my-data) (:blob my-data) "email-attachment"))
  (pprint download)
  nil)

