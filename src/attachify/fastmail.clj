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

(defn fetch-mailbox-data
  [session]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Mailbox/get"
                      {:accountId (get-account-id session)
                       :ids       nil}                      ;; nil means all mailboxes
                      "a"]]}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode query-body)
                             :as      :auto})
        response-body (:body response)
        method-response (first (:methodResponses response-body))
        mailbox-data (second method-response)]
    mailbox-data))

(defn get-inbox-id
  [mailbox-data]
  (some (fn [mbox]
          (when (= (:role mbox) "inbox")
            (:id mbox)))
        (:list mailbox-data)))

(defn get-drafts-id
  [mailbox-data]
  (some (fn [mbox]
          (when (= (:role mbox) "drafts")
            (:id mbox)))
        (:list mailbox-data)))

(defn get-processed-id
  [mailbox-data]
  (some (fn [mbox]
          (when (= (:name mbox) "Processed")
            (:id mbox)))
        (:list mailbox-data)))

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
    (first emails)))                                        ;; return the single email map

(defn inline-image-blobs
  [email]
  (->> (:attachments email)
       (filter #(and
                  (= "inline" (:disposition %))
                  (str/starts-with? (:type %) "image/")))
       (remove #(nil? (:blobId %)))
       (map #(hash-map :id (:blobId %)
                       :type (:type %)))
       (into [])))

(def mime->ext
  {"image/jpeg"      ".jpg"
   "image/png"       ".png"
   "image/gif"       ".gif"
   "image/svg+xml"   ".svg"
   "application/pdf" ".pdf"})
;; add more mappings as needed

(defn add-extension-if-missing
  [filename ext]
  (if (or (str/blank? ext)
          (str/ends-with? filename ext))
    filename
    (str filename ext)))

(defn move-email-to-processed
  [session mailbox-data email-id]
  (let [processed-id (get-processed-id mailbox-data)
        headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        set-msg-payload {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                         :methodCalls
                         [["Email/set"
                           {:accountId (get-account-id session)
                            :update    {email-id {:mailboxIds {processed-id true}}}}
                           "a"]]}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode set-msg-payload)
                             :as      :auto})]
    (println "Move response:" (:body response))
    response))



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
        mailbox-data (fetch-mailbox-data session)
        inbox-id (get-inbox-id mailbox-data)
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
    {:session  session
     :inbox-id inbox-id
     :email-id email-id
     :blob     blob}))

(defn create-draft-email
  [session mailbox-data email-text from-address to-address]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        drafts-id (get-drafts-id mailbox-data)
        email-id "draftId" ;; client-side temporary id matching your JSON example
        email-object
        { :from [{:email from-address}]
         :to [{:email to-address}]
         :subject "My Draft Email Subject"
         :mailboxIds {drafts-id true}
         :keywords {"$draft" true}
         :textBody [{:partId "body"
                     :type "text/plain"}]
         :bodyValues {"body" {:charset "utf-8"
                              :value email-text}}}
        method-calls
        [["Email/set"
          {:accountId account-id
           :create {email-id email-object}}
          "0"]]
        request-body {:using ["urn:ietf:params:jmap:core"
                              "urn:ietf:params:jmap:mail"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode request-body)
                             :as      :auto})]
    (println "Create draft email response:" (:body response))
    response))



(defn send-email
  [session email address]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        email-id "new-email" ;; client-side temporary id
        submission-id "new-submission"
        ;; Ensure :from is a vector of maps, :to is vector of {:email ...}
        email-object (-> email
                         (dissoc :mailboxIds) ;; remove mailboxIds to avoid invalidProperties error
                         (assoc :to (if (vector? address)
                                      (mapv #(hash-map :email %) address)
                                      [{:email address}])))
        method-calls
        [
         ;; Create Email
         ["Email/set"
          {:accountId account-id
           :create {email-id email-object}}
          "a"]

         ;; Submit Email for sending
         ["EmailSubmission/set"
          {:accountId account-id
           :create {submission-id {:emailId email-id
                                   :envelope {:mailFrom (get-in email [:from 0 :email])
                                              :rcptTo (if (vector? address)
                                                        address
                                                        [address])}}}}
          "b"]]

        request-body {:using ["urn:ietf:params:jmap:core"
                              "urn:ietf:params:jmap:mail"
                              "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode request-body)
                             :as      :auto})]
    (println "Send email response:" (:body response))
    response))

(defn send-simple-email
  [session from-email to-email subject body-text]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        email-id "e1"
        submission-id "s1"
        email-object {:from [{:email from-email}]
                      :to [{:email to-email}]
                      :subject subject
                      :textBody body-text}
        method-calls
        [
         ["Email/set"
          {:accountId account-id
           :create {email-id email-object}}
          "a"]
         ["EmailSubmission/set"
          {:accountId account-id
           :create {submission-id {:emailId email-id
                                   :envelope {:mailFrom from-email
                                              :rcptTo [to-email]}}}}
          "b"]]

        request-body {:using ["urn:ietf:params:jmap:core"
                              "urn:ietf:params:jmap:mail"
                              "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode request-body)
                             :as      :auto})]
    (println "Response:" (:body response))
    response))

(comment
  (defn load-config
    []
    (-> "resources/config.edn"
        slurp
        edn/read-string))
  (def config (load-config))
  (def session (fetch-session config))
  (pprint session)
  (def mailbox-data (fetch-mailbox-data session))
  (pprint mailbox-data)
  (def inbox-id (get-inbox-id mailbox-data))
  (println inbox-id)
  (def processed-id (get-processed-id mailbox-data))
  (println processed-id)
  (def inbox-email-ids (fetch-email-ids session inbox-id))
  (println inbox-email-ids)
  (def processed-email-ids (fetch-email-ids session processed-id))
  (println processed-email-ids)
  (def email-id (second inbox-email-ids))
  (println email-id)
  (move-email-to-processed session mailbox-data email-id)
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
  (def send-response (send-email session
                                 {:from     [{:email "attachify@fastmail.com" :name "Me"}]
                                  :subject  "Hello"
                                  :textBody "This is a test email."}
                                 "riwepo.work@gmail.com"))
  (pprint send-response)
  (def simple-send-response (send-simple-email
                              session
                              "attachify@fastmail.com"
                              "riwepo.work@gmail.com"
                              "test-subject"
                              "test-body"))
  (pprint simple-send-response)
  (def create-draft-response (create-draft-email session
                                                 mailbox-data
                                                 "This email is saved as a draft."
                                                 "attachify@fastmail.com"
                                                 "riwepo.work@gmail.com"))
  (pprint create-draft-response)


  nil)

