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
        auth-url (get-email-auth-url config)]
    (try
      (let [response (http/get auth-url {:headers headers :as :json})
            status (:status response)]
        (if (and (>= status 200) (< status 300))
          (let [session (:body response)]
            (if (map? session)
              {:success true
               :error false
               :error-message nil
               :value (assoc session :api-token (:email-api-token config))}
              {:success false
               :error true
               :error-message "Invalid session format"
               :value nil}))
          {:success false
           :error true
           :error-message (str "Failed to fetch session, status: " status)
           :value nil}))
      (catch Exception e
        {:success false
         :error true
         :error-message (str "Error fetching session: " (.getMessage e))
         :value nil}))))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn get-download-url [session]
  (:downloadUrl session))

(defn get-upload-url [session]
  (:uploadUrl session))

(defn get-api-url [session]
  (:apiUrl session))

(defn fetch-identity-info
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
                                     "a"]]}]
    (try
      (let [response (http/post (get-api-url session)
                                {:headers headers
                                 :body    (json/encode request-body)
                                 :as      :auto})
            status (:status response)
            body (:body response)]
        (if (and (>= status 200) (< status 300))
          (let [method-responses (:methodResponses body)
                identity-get-response (first (filter #(= "Identity/get" (first %)) method-responses))
                identity-info (get-in identity-get-response [1 :list])]
            (if (and identity-get-response (sequential? identity-info))
              {:success true
               :error false
               :error-message nil
               :value identity-info}
              {:success false
               :error true
               :error-message "Identity/get response missing or malformed"
               :value nil}))
          {:success false
           :error true
           :error-message (str "Failed to fetch identity info, status: " status)
           :value nil}))
      (catch Exception e
        {:success false
         :error true
         :error-message (str "Error fetching identity info: " (.getMessage e))
         :value nil}))))

(defn get-identity-id
  [identity-info]
  (get-in identity-info [0 :id]))

(defn fetch-mailbox-info
  [session]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Mailbox/get"
                      {:accountId (get-account-id session)
                       :ids       nil}                      ;; nil means all mailboxes
                      "a"]]}]
    (try
      (let [response (http/post (get-api-url session)
                                {:headers headers
                                 :body    (json/encode query-body)
                                 :as      :auto})
            status (:status response)
            response-body (:body response)]
        (if (and (>= status 200) (< status 300))
          (let [method-responses (:methodResponses response-body)
                mailbox-get-response (first (filter #(= "Mailbox/get" (first %)) method-responses))
                mailbox-info (:list (second mailbox-get-response))]
            (if mailbox-get-response
              {:success true
               :error false
               :error-message nil
               :value mailbox-info}
              {:success false
               :error true
               :error-message "Mailbox/get response missing"
               :value nil}))
          {:success false
           :error true
           :error-message (str "Failed to fetch mailbox info, status: " status)
           :value nil}))
      (catch Exception e
        {:success false
         :error true
         :error-message (str "Error fetching mailbox info: " (.getMessage e))
         :value nil}))))


(defn get-mailbox-id-by-name
  [mailbox-info mailbox-name]
  (some (fn [mbox]
          (when (= (:name mbox) mailbox-name)
            (:id mbox)))
        mailbox-info))

(defn get-mailbox-id-by-role
  [mailbox-info mailbox-role]
  (some (fn [mbox]
          (when (= (:role mbox) mailbox-role)
            (:id mbox)))
        mailbox-info))

(defn fetch-email-ids
  [session mailbox-id]
  (let [headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}
        query-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                    :methodCalls
                    [["Email/query"
                      {:accountId (get-account-id session)
                       :filter    {:inMailbox mailbox-id}}
                      "a"]]}]
    (try
      (let [response (http/post (get-api-url session)
                                {:headers headers
                                 :body    (json/encode query-body)
                                 :as      :auto})
            status (:status response)
            body (:body response)]
        (if (and (>= status 200) (< status 300))
          (let [method-responses (:methodResponses body)
                error-response (first (filter #(= "error" (first %)) method-responses))
                email-query-response (first (filter #(= "Email/query" (first %)) method-responses))]
            (cond
              error-response
              (let [{:keys [arguments type]} (second error-response)
                    error-msg (str "API error: " type ", arguments: " arguments)]
                {:success false
                 :error true
                 :error-message error-msg
                 :value nil})
              email-query-response
              (let [method-response-data (second email-query-response)
                    email-ids (:ids method-response-data)]
                {:success true
                 :error false
                 :error-message nil
                 :value email-ids})
              :else
              {:success false
               :error true
               :error-message "Neither Email/query nor error response found"
               :value nil}))
          {:success false
           :error true
           :error-message (str "Failed to fetch email ids, status: " status)
           :value nil}))
      (catch Exception e
        {:success false
         :error true
         :error-message (str "Error fetching email ids: " (.getMessage e))
         :value nil}))))


(defn fetch-email
  [session email-id]
  (try
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
                               :body (json/encode query-body)
                               :as :auto})
          body (:body response)
          method-responses (:methodResponses body)
          method-response (first method-responses)
          method-response-data (second method-response)
          emails (:list method-response-data)
          email (first emails)]
      (if email
        {:success true
         :error false
         :value email}
        {:success false
         :error true
         :error-message (str "Email with id " email-id " not found")
         :value nil}))
    (catch Exception e
      {:success false
       :error true
       :error-message (str "Exception fetching email by id: " (.getMessage e))
       :value nil})))                              ;; return the single email map

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

(defn move-email-to-mailbox
  [session email-id mailbox-id]
  (try
    (let [headers {"Authorization" (str "Bearer " (:api-token session))
                   "Content-Type"  "application/json; charset=utf-8"}
          set-msg-payload {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                           :methodCalls
                           [["Email/set"
                             {:accountId (get-account-id session)
                              :update    {email-id {:mailboxIds {mailbox-id true}}}}
                             "a"]]}
          response (http/post (get-api-url session)
                              {:headers headers
                               :body    (json/encode set-msg-payload)
                               :as      :auto})
          body (:body response)
          method-responses (:methodResponses body)
          email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
          not-created (get-in email-set-response [1 :notCreated])]
      (if (or (nil? email-set-response) (seq not-created))
        {:success false
         :error true
         :error-message (str "Failed to move email " email-id " to mailbox " mailbox-id
                             ". Details: " not-created)
         :value false}
        {:success true
         :error false
         :value true}))
    (catch Exception e
      {:success false
       :error true
       :error-message (str "Exception during move-email-to-mailbox: " (.getMessage e))
       :value false})))


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
    (if real-email-id
      {:success true
       :error false
       :value real-email-id}
      {:success false
       :error true
       :error-message (get-in email-set-response [1 :notCreated (keyword draft-id) :description])})))

(defn upload-attachments
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

(defn submit-email
  [session email-id identity-id]
  (let [account-id (get-account-id session)
        submission-id "submission_id"
        email-submission {:emailId email-id :identityId identity-id}
        method-calls [["EmailSubmission/set"
                       {:accountId account-id
                        :create {submission-id email-submission}}
                       "0"]]
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail" "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}]
    (try
      (let [response (http/post (get-api-url session)
                                {:headers headers
                                 :body    (json/encode request-body)
                                 :as      :auto})
            status (:status response)]
        (if (and (>= status 200) (< status 300))
          {:success true
           :error false
           :error-message nil
           :value true}
          {:success false
           :error true
           :error-message (str "Failed to submit email, status: " status)
           :value nil}))
      (catch Exception e
        {:success false
         :error true
         :error-message (str "Error submitting email: " (.getMessage e))
         :value nil}))))


(defn download-blobs
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
  (def identity-info (fetch-identity-info session))
  (pprint identity-info)
  (def send-identity (get-identity-id identity-info))
  (println send-identity)
  (def mailbox-info (fetch-mailbox-info session))
  (pprint mailbox-info)
  (def inbox-email-ids (fetch-email-ids session (get-mailbox-id-by-role mailbox-info "inbox")))
  (println inbox-email-ids)
  (def email-id (first inbox-email-ids))
  (println email-id)
  (def email (fetch-email session email-id))
  (pprint email)
  (def blob-info (get-blob-info email))
  (pprint blob-info)
  (def download-blobs-result (download-blobs session blob-info))
  (pprint download-blobs-result)
  (def blobs (:value download-blobs-result))
  (pprint blobs)
  (def upload-attachments-result (upload-attachments session blobs))
  (pprint upload-attachments-result)
  (def attachment-info (:value upload-attachments-result))
  (pprint attachment-info)
  (def draft-email-object (build-draft-email
                            blobs
                            attachment-info
                            "attachify@fastmail.com"
                            "riwepo.work@gmail.com"
                            (get-mailbox-id-by-role mailbox-info "drafts")
                            (:subject email)))
  (pprint draft-email-object)
  (def create-draft-email-result (create-draft-email
                                   session
                                   draft-email-object))
  (pprint create-draft-email-result)

  nil)

