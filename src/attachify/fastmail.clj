(ns attachify.fastmail
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-http.client :as http]
            [cheshire.core :as json])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.util UUID]))

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

(defn get-upload-url [session]
  (:uploadUrl session))

(defn get-api-url [session]
  (:apiUrl session))

(defn fetch-identity-data
  [session]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        request-body {:using       ["urn:ietf:params:jmap:core"
                                    "urn:ietf:params:jmap:mail"
                                    "urn:ietf:params:jmap:submission"]
                      :methodCalls [["Identity/get"
                                     {:accountId account-id
                                      :ids       nil}
                                     "a"]]}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode request-body)
                             :as      :auto})
        body (:body response)
        method-responses (:methodResponses body)
        identity-get-response (first (filter #(= "Identity/get" (first %)) method-responses))
        identity-data (get-in identity-get-response [1 :list])]
    identity-data))

(defn get-identity
  [identity-data]
  (get-in identity-data [0 :id]))


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

(defn get-trash-id
  [mailbox-data]
  (some (fn [mbox]
          (when (= (:role mbox) "trash")
            (:id mbox)))
        (:list mailbox-data)))

(defn get-sent-id
  [mailbox-data]
  (some (fn [mbox]
          (when (= (:role mbox) "sent")
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

(defn bytes->string
  [byte-array & {:keys [charset] :or {charset "UTF-8"}}]
  (String. byte-array charset))

(defn strip-bom [s]
  (if (.startsWith s "\uFEFF")
    (subs s 1)
    s))

(defn replace-nbsp
  [s]
  (str/replace s #"\u00A0" " "))

(defn download-blob
  [session blob filename]
  (try
    (let [filename (or filename "file")
          ext (get mime->ext (:type blob))
          filename-with-ext (add-extension-if-missing filename ext)
          encoded-filename (url-encode filename-with-ext)
          encoded-type (url-encode (:type blob))
          download-url (-> (get-download-url session)
                           (str/replace "{accountId}" (get-account-id session))
                           (str/replace "{blobId}" (:blobId blob))
                           (str/replace "{name}" encoded-filename)
                           (str/replace "{type}" encoded-type))
          headers {"Authorization" (str "Bearer " (:api-token session))}
          response (http/get download-url {:headers headers :as :byte-array})
          bytes (:body response)
          decoded-content (if (and (:type blob)
                                   (or (str/starts-with? (:type blob) "text/")
                                       (= (:type blob) "application/json")
                                       (= (:type blob) "application/xml")))
                            ;; decode text-like content as string
                            (-> (bytes->string bytes :charset "UTF-8")
                                replace-nbsp)
                            ;; else keep raw bytes (e.g. images, pdfs)
                            bytes)]
      {:success true
       :error false
       :error-message nil
       :value decoded-content})
    (catch Exception e
      {:success false
       :error true
       :error-message (str "Failed to download or decode blob: " (.getMessage e))
       :value nil})))

(defn upload-blob
  [session blob]
  (try
    (let [upload-url (-> (get-upload-url session)
                         (str/replace "{accountId}" (get-account-id session)))
          headers {"Authorization" (str "Bearer " (:api-token session))
                   "Content-Type" (:type blob)}
          response (http/post upload-url {:headers headers
                                          :body (:value blob)
                                          :throw-exceptions false})
          status (:status response)
          content-type (some-> (get-in response [:headers "Content-Type"])
                               str/lower-case)
          body (if (and content-type (str/includes? content-type "application/json"))
                 (json/parse-string (:body response) true)
                 nil)]
      (if (and (= status 200) (contains? body :blobId))
        {:success true
         :error false
         :error-message nil
         :value (:blobId body)}
        {:success false
         :error true
         :error-message (str "Upload failed with status " status " and body: " body)
         :value nil}))
    (catch Exception e
      {:success false
       :error true
       :error-message (str "Exception during upload: " (.getMessage e))
       :value nil})))


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


(defn send-email
  [session email address]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        email-id "new-email"                                ;; client-side temporary id
        submission-id "new-submission"
        ;; Ensure :from is a vector of maps, :to is vector of {:email ...}
        email-object (-> email
                         (dissoc :mailboxIds)               ;; remove mailboxIds to avoid invalidProperties error
                         (assoc :to (if (vector? address)
                                      (mapv #(hash-map :email %) address)
                                      [{:email address}])))
        method-calls
        [
         ;; Create Email
         ["Email/set"
          {:accountId account-id
           :create    {email-id email-object}}
          "a"]

         ;; Submit Email for sending
         ["EmailSubmission/set"
          {:accountId account-id
           :create    {submission-id {:emailId  email-id
                                      :envelope {:mailFrom (get-in email [:from 0 :email])
                                                 :rcptTo   (if (vector? address)
                                                             address
                                                             [address])}}}}
          "b"]]

        request-body {:using       ["urn:ietf:params:jmap:core"
                                    "urn:ietf:params:jmap:mail"
                                    "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers headers
                             :body    (json/encode request-body)
                             :as      :auto})]
    (println "Send email response:" (:body response))
    response))

(defn create-and-send-email
  [session send-identity mailbox-data email-text from-address to-address]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        drafts-id (get-drafts-id mailbox-data)
        sent-id (get-sent-id mailbox-data)
        identity-id send-identity
        draft-id "draft_message"
        submission-id "submission_id"
        email-object
        { :from [{:email from-address}]
         :to [{:email to-address}]
         :subject "My Sent Email Subject"
         :mailboxIds {drafts-id true}
         :textBody [{:partId "body"
                     :type "text/plain"}]
         :bodyValues {"body" {:charset "utf-8"
                              :value email-text}}}
        email-submission
        {:emailId (str "#" draft-id) ;; Reference draft by client creation id with #
         :identityId identity-id}
        ;; Step 1: Create draft and send email
        method-calls-step1
        [["Email/set"
          {:accountId account-id
           :create {draft-id email-object}}
          "0"]
         ["EmailSubmission/set"
          {:accountId account-id
           :create {submission-id email-submission}}
          "1"]]
        request-body-step1 {:using       ["urn:ietf:params:jmap:core"
                                          "urn:ietf:params:jmap:mail"
                                          "urn:ietf:params:jmap:submission"]
                            :methodCalls method-calls-step1}
        response-step1 (http/post (get-api-url session)
                                  {:headers headers
                                   :body (json/encode request-body-step1)
                                   :as :auto})
        ;; Extract real email id from response
        create-body (:body response-step1)
        method-responses (:methodResponses create-body)
        email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
        created-map (get-in email-set-response [1 :created])
        real-email-id (get-in created-map [(keyword draft-id) :id])]
    (if (nil? real-email-id)
      (do
        (println "Failed to get real email id from create response:" create-body)
        response-step1)
      (let [;; Step 2: Move email from Drafts to Trash
            update-request-body {:using       ["urn:ietf:params:jmap:core"
                                               "urn:ietf:params:jmap:mail"]
                                 :methodCalls [["Email/set"
                                                {:accountId account-id
                                                 :update {real-email-id
                                                          {:mailboxIds {sent-id true}}}}

                                                "2"]]}
            response-step2 (http/post (get-api-url session)
                                      {:headers headers
                                       :body (json/encode update-request-body)
                                       :as :auto})]
        (println "Move draft to trash response:" (:body response-step2))
        response-step2))))

(defn create-and-send-email2
  [session send-identity mailbox-data email-object from-address to-address]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        account-id (get-account-id session)
        drafts-id (get-drafts-id mailbox-data)
        sent-id (get-sent-id mailbox-data)
        identity-id send-identity
        draft-id "draft_message"
        submission-id "submission_id"
        ;; Override some fields in email-object to ensure mailbox and addresses are correct
        email-object-updated
        (-> email-object
            (assoc :mailboxIds {drafts-id true})
            (assoc :from [{:email from-address}])
            (assoc :to [{:email to-address}]))
        email-submission
        {:emailId (str "#" draft-id) ;; Reference draft by client creation id with #
         :identityId identity-id}
        ;; Step 1: Create draft and send email
        method-calls-step1
        [["Email/set"
          {:accountId account-id
           :create {draft-id email-object-updated}}
          "0"]
         ["EmailSubmission/set"
          {:accountId account-id
           :create {submission-id email-submission}}
          "1"]]
        request-body-step1 {:using       ["urn:ietf:params:jmap:core"
                                          "urn:ietf:params:jmap:mail"
                                          "urn:ietf:params:jmap:submission"]
                            :methodCalls method-calls-step1}
        response-step1 (http/post (get-api-url session)
                                  {:headers headers
                                   :body (json/encode request-body-step1)
                                   :as :auto})
        ;; Extract real email id from response
        create-body (:body response-step1)
        method-responses (:methodResponses create-body)
        email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
        created-map (get-in email-set-response [1 :created])
        real-email-id (get-in created-map [(keyword draft-id) :id])]
    (if (nil? real-email-id)
      (do
        (println "Failed to get real email id from create response:" create-body)
        response-step1)
      (let [;; Step 2: Move email from Drafts to Sent mailbox
            update-request-body {:using       ["urn:ietf:params:jmap:core"
                                               "urn:ietf:params:jmap:mail"]
                                 :methodCalls [["Email/set"
                                                {:accountId account-id
                                                 :update {real-email-id
                                                          {:mailboxIds {sent-id true}}}}
                                                "2"]]}
            response-step2 (http/post (get-api-url session)
                                      {:headers headers
                                       :body (json/encode update-request-body)
                                       :as :auto})]
        (println "Move draft to sent mailbox response:" (:body response-step2))
        response-step2))))

(defn get-blob-info
  [email]
  (let [text-blobs (map (fn [part]
                          {:role :textBody
                           :blobId (:blobId part)
                           :type (:type part)})
                        (:textBody email))
        html-blobs (map (fn [part]
                          {:role :htmlBody
                           :blobId (:blobId part)
                           :type (:type part)})
                        (:htmlBody email))
        attachment-blobs (map (fn [att]
                                {:role :attachment
                                 :blobId (:blobId att)
                                 :type (:type att)})
                              (:attachments email))]
    (->> (concat text-blobs html-blobs attachment-blobs)
         (filter #(some :blobId [%])) ;; only keep entries with blobId
         (map #(select-keys % [:role :blobId :type]))
         (into []))))

(defn extract-email-body
  [email]
  {:text (get-in email [:bodyValues "text" :value])
   :html (get-in email [:bodyValues "html" :value])})

(defn build-draft-email
  [blobs attachment-info from-address to-address drafts-id subject]
  (let [text (some #(when (= (:role %) :textBody) (:value %)) blobs)
        html (some #(when (= (:role %) :htmlBody) (:value %)) blobs)
        id-map (into {}
                     (map (fn [{:keys [blobId]}]
                            [(:old blobId) (:new blobId)])
                          attachment-info))
        attachments (->> blobs
                         (filter #(= (:role %) :attachment))
                         (map (fn [att]
                                (let [old-id (:blobId att)
                                      new-id (get id-map old-id)]
                                  {:blobId new-id
                                   :type (:type att)
                                   :disposition "attachment"})))
                         vec)]
    {:from [{:email from-address}]
     :to [{:email to-address}]
     :mailboxIds {drafts-id true}
     :subject subject
     :textBody (when text [{:partId "text"}])
     :htmlBody (when html [{:partId "html"}])
     :bodyValues (cond-> {}
                         text (assoc "text" {:value text :charset "utf-8"})
                         html (assoc "html" {:value html :charset "utf-8"}))
     :attachments attachments}))



(defn create-draft-email
  [session email-object]
  (let [account-id (get-account-id session)
        draft-id "draft_message"
        method-calls [["Email/set"
                       {:accountId account-id
                        :create {draft-id email-object}}
                       "0"]]
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers {"Authorization" (str "Bearer " (:api-token session))
                                       "Content-Type" "application/json; charset=utf-8"}
                             :body (json/encode request-body)
                             :as :auto})
        body (:body response)
        method-responses (:methodResponses body)
        email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
        created-map (get-in email-set-response [1 :created])
        real-email-id (get-in created-map [(keyword draft-id) :id])]
    (pprint body)
    (if real-email-id
      {:success true
       :error false
       :value real-email-id}
      {:success false
       :error true
       :error-message (get-in email-set-response [1 :notCreated (keyword draft-id) :description])})))

(defn upload-all-attachments
  [session blobs]
  (loop [remaining blobs
         results []]
    (if (empty? remaining)
      {:success true
       :error false
       :error-message nil
       :value results}
      (let [blob (first remaining)]
        (if (= (:role blob) :attachment)
          (let [upload-result (upload-blob session blob)]
            (if (:error upload-result)
              {:success false
               :error true
               :error-message (:error-message upload-result)
               :value nil}
              (recur (rest remaining)
                     (conj results {:role (:role blob)
                                    :blobId {:old (:blobId blob)
                                             :new (:value upload-result)}
                                    :type (:type blob)}))))
          ;; Not an attachment, skip it
          (recur (rest remaining) results))))))




;; Step 2: Update draft email to add attachments referencing existing blobIds

(defn update-draft-attachments
  [session draft-email-id attachments]
  (let [account-id (get-account-id session)
        method-calls [["Email/set"
                       {:accountId account-id
                        :update {draft-email-id {:attachments attachments}}}
                       "0"]]
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers {"Authorization" (str "Bearer " (:api-token session))
                                       "Content-Type" "application/json; charset=utf-8"}
                             :body (json/encode request-body)
                             :as :auto})]
    response))

;; Step 3: Submit email

(defn submit-email
  [session draft-email-id identity-id]
  (let [account-id (get-account-id session)
        submission-id "submission_id"
        email-submission {:emailId draft-email-id :identityId identity-id}
        method-calls [["EmailSubmission/set"
                       {:accountId account-id
                        :create {submission-id email-submission}}
                       "0"]]
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail" "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        response (http/post (get-api-url session)
                            {:headers {"Authorization" (str "Bearer " (:api-token session))
                                       "Content-Type" "application/json; charset=utf-8"}
                             :body (json/encode request-body)
                             :as :auto})]
    response))

(defn download-all-blobs
  [session blob-info]
  (loop [remaining blob-info
         results []]
    (if (empty? remaining)
      ;; All blobs processed successfully, return vector with :value added and no error keys
      {:success true
       :error false
       :error-message nil
       :value (mapv
                (fn [blob download]
                  (assoc blob :value (:value download)))
                blob-info
                results)}
      (let [blob (first remaining)
            filename (str (name (:role blob))) ;; Use role name as filename base
            download-result (download-blob session blob filename)]
        (if (:error download-result)
          ;; Error occurred, return immediately with error info
          {:success false
           :error true
           :error-message (:error-message download-result)
           :value nil}
          ;; No error, accumulate download result and continue
          (recur (rest remaining)
                 (conj results download-result)))))))





(comment
  (defn load-config
    []
    (-> "resources/config.edn"
        slurp
        edn/read-string))
  (def config (load-config))
  (def session (fetch-session config))
  (pprint session)
  (def identity-data (fetch-identity-data session))
  (pprint identity-data)
  (def send-identity (get-identity identity-data))
  (println send-identity)
  (def mailbox-data (fetch-mailbox-data session))
  (pprint mailbox-data)
  (def inbox-id (get-inbox-id mailbox-data))
  (println inbox-id)
  (def drafts-id (get-drafts-id mailbox-data))
  (println drafts-id)
  (def trash-id (get-trash-id mailbox-data))
  (println trash-id)
  (def processed-id (get-processed-id mailbox-data))
  (println processed-id)
  (def inbox-email-ids (fetch-email-ids session inbox-id))
  (println inbox-email-ids)
  (def processed-email-ids (fetch-email-ids session processed-id))
  (println processed-email-ids)
  (def email-id (first inbox-email-ids))
  (println email-id)
  (move-email-to-processed session mailbox-data email-id)
  (def email (fetch-email-by-id session email-id))
  (pprint email)
  (def blob-info (get-blob-info email))
  (pprint blob-info)
  (def download-blobs-result (download-all-blobs session blob-info))
  (pprint download-blobs-result)
  (def blobs (:value download-blobs-result))
  (pprint blobs)
  (def upload-all-attachments-result (upload-all-attachments session blobs))
  (pprint upload-all-attachments-result)
  (def attachment-info (:value upload-all-attachments-result))
  (pprint attachment-info)
  (def draft-email-object (build-draft-email
                            blobs
                            attachment-info
                            "attachify.com"
                            "riwepo.work@gmail.com"
                            drafts-id
                            (:subject email)))
  (pprint draft-email-object)
  (def create-draft-email-result (create-draft-email
                                   session
                                   draft-email-object))
  (pprint create-draft-email-result)

  nil)

