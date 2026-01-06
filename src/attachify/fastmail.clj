(ns attachify.fastmail
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [taoensso.telemere :as tel]
            [attachify.result :refer [success success?]]
            [attachify.result-log :refer [log-and-success log-and-failure]]
            [attachify.config :refer [load-config get-email-auth-url]]
            [attachify.http :as http2])
  (:import [java.net URLEncoder]
           [java.nio.charset Charset StandardCharsets]))

(defn url-encode [data]
  (^[String Charset] URLEncoder/encode data StandardCharsets/UTF_8))

(defn fetch-session
  [config]
  (let [auth-url (get-email-auth-url config)
        api-token (:email-api-token config)
        get-result (http2/get2 auth-url api-token)]
    (println get-result)
    (if (success? get-result)
      (let [session (:value get-result)]
        (if (map? session)
          (log-and-success "fetch-session succeeded" (assoc session :apiToken (:email-api-token config)))
          (log-and-failure "Invalid session format")))
      get-result)))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn get-identity-id
  [identity-info]
  (get-in identity-info [0 :id]))

(defn fetch-identity-info
  [session]
  (let [url (:apiUrl session)
        api-token (:apiToken session)
        account-id (get-account-id session)
        request-body {:using       ["urn:ietf:params:jmap:core"
                                    "urn:ietf:params:jmap:mail"
                                    "urn:ietf:params:jmap:submission"]
                      :methodCalls [["Identity/get"
                                     {:accountId account-id
                                      :ids       nil}
                                     "a"]]}
        post-result (http2/post2 url api-token request-body)]
    (if (success? post-result)
      (let [body (:value post-result)
            method-responses (:methodResponses body)
            error-response (first (filter #(= "error" (first %)) method-responses))
            identity-get-response (first (filter #(= "Identity/get" (first %)) method-responses))]

        (cond
          error-response
          (let [{:keys [arguments type]} (second error-response)
                error-msg (str "API error: " type ", arguments: " arguments)]
            (log-and-failure error-msg))

          identity-get-response
          (let [identity-info (get-in identity-get-response [1 :list])]
            (if (sequential? identity-info)
              (log-and-success "fetch-identity-info succeeded" (get-identity-id identity-info))
              (log-and-failure "Identity/get response malformed")))

          :else
          (log-and-failure "Neither Identity/get nor error response found")))
      (log-and-failure (:error post-result)))))

(defn fetch-mailbox-info
  [session]
  (let [url (:apiUrl session)
        api-token (:apiToken session)
        account-id (get-account-id session)
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls
                      [["Mailbox/get"
                        {:accountId account-id
                         :ids       nil}
                        "a"]]}
        post-result (http2/post2 url api-token request-body)]
    (if (success? post-result)
      (let [body (:value post-result)
            method-responses (:methodResponses body)
            error-response (first (filter #(= "error" (first %)) method-responses))
            mailbox-get-response (first (filter #(= "Mailbox/get" (first %)) method-responses))]
        (println mailbox-get-response)
        (cond
          error-response
          (let [{:keys [arguments type]} (second error-response)
                error-msg (str "API error: " type ", arguments: " arguments)]
            (log-and-failure error-msg))

          mailbox-get-response
          (let [result (:list (second mailbox-get-response))]
            (log-and-success "fetch-mailbox-info succeeded" result))

          :else
          (log-and-failure "Neither Mailbox/get nor error response found")))
      (log-and-failure (:error post-result)))))

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
  (let [url (:apiUrl session)
        api-token (:apiToken session)
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls
                      [["Email/query"
                        {:accountId (get-account-id session)
                         :filter    {:inMailbox mailbox-id}}
                        "a"]]}
        post-result (http2/post2 url api-token request-body)]
    (if (success? post-result)
      (let [body (:value post-result)
            method-responses (:methodResponses body)
            error-response (first (filter #(= "error" (first %)) method-responses))
            email-query-response (first (filter #(= "Email/query" (first %)) method-responses))]
        (cond
          error-response
          (let [{:keys [arguments type]} (second error-response)
                error-msg (str "API error: " type ", arguments: " arguments)]
            (log-and-failure error-msg))

          email-query-response
          (log-and-success "fetch-email-ids succeeded" (:ids (second email-query-response)))

          :else
          (log-and-failure "Neither Email/query nor error response found")))
      (log-and-failure (:error post-result)))))

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
          response (http/post (:apiUrl session)
                              {:headers headers
                               :body    (json/encode query-body)
                               :as      :auto})
          body (:body response)
          method-responses (:methodResponses body)
          error-response (first (filter #(= "error" (first %)) method-responses))
          email-get-response (first (filter #(= "Email/get" (first %)) method-responses))]
      (cond
        error-response
        (let [{:keys [arguments type]} (second error-response)
              error-msg (str "API error: " type ", arguments: " arguments)]
          (log-and-failure error-msg))

        email-get-response
        (let [emails (:list (second email-get-response))
              email (first emails)]
          (if email
            (log-and-success "fetch-email succeeded" email)
            (log-and-failure (str "Email with id " email-id " not found"))))

        :else
        (log-and-failure "Neither Email/get nor error response found")))
    (catch Exception e
      (log-and-failure (str "Exception fetching email by id: " (.getMessage e))))))


(defn get-to-address [email]
  (get-in email [:to 0 :email]))

(def mime->ext
  {"image/jpeg"      ".jpg"
   "image/png"       ".png"
   "image/gif"       ".gif"
   "image/svg+xml"   ".svg"
   "application/pdf" ".pdf"})


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
          response (http/post (:apiUrl session)
                              {:headers headers
                               :body    (json/encode set-msg-payload)
                               :as      :auto})
          body (:body response)
          method-responses (:methodResponses body)
          error-response (first (filter #(= "error" (first %)) method-responses))
          email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
          not-created (get-in email-set-response [1 :notCreated])]
      (cond
        error-response
        (let [{:keys [arguments type]} (second error-response)
              error-msg (str "API error: " type ", arguments: " arguments)]
          (log-and-failure error-msg))

        (or (nil? email-set-response) (seq not-created))
        (log-and-failure (str "Failed to move email " email-id " to mailbox " mailbox-id
                              ". Details: " not-created))

        :else
        (log-and-success "move-email-to-mailbox succeeded")))
    (catch Exception e
      (log-and-failure (str "Exception during move-email-to-mailbox: " (.getMessage e))))))

(defn bytes->string
  [^bytes byte-array & {:keys [^String charset] :or {charset "UTF-8"}}]
  (String. byte-array charset))

;(defn strip-bom [s]
;  (if (.startsWith s "\uFEFF")
;    (subs s 1)
;    s))

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
          download-url (-> (:downloadUrl session)
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
      (tel/log! {:level :debug, :success true :blobId blob})
      (success decoded-content))
    (catch Exception e
      (log-and-failure (str "Failed to download or decode blob: " (.getMessage e))))))


(defn upload-blob
  [session blob]
  (try
    (let [upload-url (-> (:uploadUrl session)
                         (str/replace "{accountId}" (get-account-id session)))
          headers {"Authorization" (str "Bearer " (:api-token session))
                   "Content-Type"  (:type blob)}
          response (http/post upload-url {:headers          headers
                                          :body             (:value blob)
                                          :throw-exceptions false})
          status (:status response)
          content-type (some-> (get-in response [:headers "Content-Type"])
                               str/lower-case)
          body (if (and content-type (str/includes? content-type "application/json"))
                 (json/parse-string (:body response) true)
                 nil)]
      (if (and (= status 200) (contains? body :blobId))
        (log-and-success "upload-blob succeeded" (:blobId body))
        (log-and-failure (str "Upload failed with status " status " and body: " body))))
    (catch Exception e
      (log-and-failure (str "Exception during upload: " (.getMessage e))))))

(defn get-blob-info
  [email]
  (let [text-blobs (map (fn [part]
                          {:role   :textBody
                           :blobId (:blobId part)
                           :type   (:type part)})
                        (:textBody email))
        html-blobs (map (fn [part]
                          {:role   :htmlBody
                           :blobId (:blobId part)
                           :type   (:type part)})
                        (:htmlBody email))
        attachment-blobs (map (fn [att]
                                {:role   :attachment
                                 :name   (:name att)
                                 :blobId (:blobId att)
                                 :type   (:type att)})
                              (:attachments email))]
    (->> (concat text-blobs html-blobs attachment-blobs)
         (filter #(some :blobId [%]))                       ;; only keep entries with blobId
         (map #(select-keys % [:role :blobId :type :name]))
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
                                  {:name        (:name att)
                                   :blobId      new-id
                                   :type        (:type att)
                                   :disposition "attachment"})))
                         vec)]
    {:from        [{:email from-address}]
     :to          [{:email to-address}]
     :mailboxIds  {drafts-id true}
     :subject     subject
     :textBody    (when text [{:partId "text"}])
     :htmlBody    (when html [{:partId "html"}])
     :bodyValues  (cond-> {}
                          text (assoc "text" {:value text :charset "utf-8"})
                          html (assoc "html" {:value html :charset "utf-8"}))
     :attachments attachments}))


(defn create-draft-email
  [session email-object]
  (try
    (let [account-id (get-account-id session)
          draft-id "draft_message"
          method-calls [["Email/set"
                         {:accountId account-id
                          :create    {draft-id email-object}}
                         "0"]]
          request-body {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                        :methodCalls method-calls}
          response (http/post (:apiUrl session)
                              {:headers {"Authorization" (str "Bearer " (:api-token session))
                                         "Content-Type"  "application/json; charset=utf-8"}
                               :body    (json/encode request-body)
                               :as      :auto})
          body (:body response)
          method-responses (:methodResponses body)
          error-response (first (filter #(= "error" (first %)) method-responses))
          email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
          created-map (get-in email-set-response [1 :created])
          real-email-id (get-in created-map [(keyword draft-id) :id])]
      (cond
        error-response
        (let [{:keys [arguments type]} (second error-response)
              error-msg (str "API error: " type ", arguments: " arguments)]
          (log-and-failure error-msg))

        real-email-id
        (log-and-success "create-draft-email succeeded" real-email-id)

        :else
        (log-and-failure (get-in email-set-response [1 :notCreated (keyword draft-id) :description]
                                 "Unknown error creating draft email"))))
    (catch Exception e
      (log-and-failure (str "Exception creating draft email: " (.getMessage e))))))



(defn upload-attachments
  [session blobs]
  (loop [remaining blobs
         results []]
    (if (empty? remaining)
      (do
        (tel/log! {:level :debug, :success true})
        (success results))
      (let [blob (first remaining)]
        (if (= (:role blob) :attachment)
          (let [upload-result (upload-blob session blob)]
            (if (:error upload-result)
              (log-and-failure (:error-message upload-result))
              (recur (rest remaining)
                     (conj results {:role   (:role blob)
                                    :blobId {:old (:blobId blob)
                                             :new (:value upload-result)}
                                    :type   (:type blob)}))))
          ;; Not an attachment, skip it
          (recur (rest remaining) results))))))


(defn submit-email
  [session email-id identity-id]
  (let [account-id (get-account-id session)
        submission-id "submission_id"
        email-submission {:emailId email-id :identityId identity-id}
        method-calls [["EmailSubmission/set"
                       {:accountId account-id
                        :create    {submission-id email-submission}}
                       "0"]]
        request-body {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail" "urn:ietf:params:jmap:submission"]
                      :methodCalls method-calls}
        headers {"Authorization" (str "Bearer " (:api-token session))
                 "Content-Type"  "application/json; charset=utf-8"}]
    (try
      (let [response (http/post (:apiUrl session)
                                {:headers headers
                                 :body    (json/encode request-body)
                                 :as      :auto})
            status (:status response)
            body (:body response)
            method-responses (:methodResponses body)
            error-response (first (filter #(= "error" (first %)) method-responses))]
        (cond
          error-response
          (let [{:keys [arguments type]} (second error-response)
                error-msg (str "API error: " type ", arguments: " arguments)]
            (log-and-failure error-msg))

          (and (>= status 200) (< status 300))
          (do
            (tel/log! {:level :debug, :success true :email-id email-id})
            (success email-id))

          :else
          (log-and-failure (str "Failed to submit email, status: " status))))
      (catch Exception e
        (log-and-failure (str "Error submitting email: " (.getMessage e)))))))



(defn download-blobs
  [session blob-info]
  (loop [remaining blob-info
         results []]
    (if (empty? remaining)
      (do
        (tel/log! {:level :debug, :success true})
        (success
          (mapv
            (fn [blob download]
              (assoc blob :value (:value download)))
            blob-info
            results)))
      (let [blob (first remaining)
            filename (str (name (:role blob)))              ;; Use role name as filename base
            download-result (download-blob session blob filename)]
        (if (:error download-result)
          (log-and-failure (:error-message download-result))
          (recur (rest remaining)
                 (conj results download-result)))))))


(comment
  (def config (load-config))
  (def fetch-session-result (fetch-session config))
  (def session (:value fetch-session-result))
  (fetch-mailbox-info session)
  (fetch-email-ids session "P-F")
  nil)

