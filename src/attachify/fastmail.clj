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
  [access-token session]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
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
  [access-token session mailbox-id]
  (let [
        headers {"Authorization" (str "Bearer " access-token)
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
  [access-token session email-id]
  (let [headers {"Authorization" (str "Bearer " access-token)
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
  [access-token session blob filename]
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
        headers {"Authorization" (str "Bearer " access-token)}]
    (println "Downloading blob from URL:" download-url)
    (http/get download-url {:headers headers :as :byte-array})))

(defn fetch-first-inline-image-blob
  [auth-url access-token]
  (let [session (fetch-session auth-url access-token)
        inbox-id (fetch-inbox-id access-token session)
        email-ids (fetch-email-ids access-token session inbox-id)
        email-id (second email-ids)
        email (fetch-email-by-id access-token session email-id)
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
  (def session (fetch-session my-auth-url my-access-token))
  (pprint session)
  (def inbox-id (fetch-inbox-id my-access-token session))
  (println inbox-id)
  (def email-ids (fetch-email-ids my-access-token session inbox-id))
  (def email-id (second email-ids))
  (println email-id)
  (def email (fetch-email-by-id my-access-token session email-id))
  (pprint email)
  (def blobs (inline-image-blobs email))
  (def blob (first blobs))
  (print blob)
  nil
  (def my-data (fetch-first-inline-image-blob my-auth-url my-access-token))
  (pprint my-data)
  (def download (download-blob my-access-token (:session my-data) (:blob my-data) "email-attachment"))
  (pprint download)
  nil)

