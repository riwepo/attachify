(ns attachify.fastmail
  (:require [clojure.string :as str]
            ;;[clojure.pprint :refer [pprint]]
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
    (if (success? get-result)
      (let [session (:value get-result)]
        (if (map? session)
          (log-and-success (assoc session :apiToken (:email-api-token config)) "fetch-session succeeded")
          (log-and-failure "fetch-session failed" "Invalid session format")))
      (log-and-failure "fetch-session failed" (:error get-result)))))

(defn get-account-id [session]
  (get-in session [:primaryAccounts :urn:ietf:params:jmap:mail]))

(defn process-response [extractor response-body]
  (let [method-responses (:methodResponses response-body)
        error-response (first (filter #(= "error" (first %)) method-responses))]
    (if error-response
      (let [{:keys [arguments type]} (second error-response)]
        (log-and-failure "post response indicated failure" (str "API error: type " type ", arguments: " arguments)))
      (let [result (extractor method-responses)]
        (if result
          (log-and-success result "post response indicated success")
          (log-and-failure "post response indicated failure" "unknown error"))))))

(defn extract-identity-info [method-responses]
  (let [identity-get-response (first (filter #(= "Identity/get" (first %)) method-responses))
        identity-info (get-in identity-get-response [1 :list])
        result (get-in identity-info [0 :id])]
    result))

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
        encoded-request-body (json/encode request-body)
        post-result (http2/post2 url api-token encoded-request-body "application/json; charset=utf-8")]
    (if (success? post-result)
      (let [process-response-result (process-response extract-identity-info (:value post-result))]
        (if (success? process-response-result)
          (log-and-success (:value process-response-result) "fetch-identity-info succeeded")
          (log-and-failure "fetch-identity-info failed" (:error process-response-result))))
      (log-and-failure "fetch-identity-info failed" (:error post-result)))))

(defn extract-mailbox-info [method-responses]
  (let [mailbox-get-response (first (filter #(= "Mailbox/get" (first %)) method-responses))
        result (:list (second mailbox-get-response))]
    result))

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
        encoded-request-body (json/encode request-body)
        post-result (http2/post2 url api-token encoded-request-body "application/json; charset=utf-8")]
      (if (success? post-result)
        (let [process-response-result (process-response extract-mailbox-info (:value post-result))]
          (if (success? process-response-result)
            (log-and-success (:value process-response-result) "fetch-mailbox-info succeeded")
            (log-and-failure "fetch-mailbox-info failed" (:error process-response-result))))
        (log-and-failure "fetch-mailbox-info failed" (:error post-result)))))

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

(defn extract-email-ids [method-responses]
  (let [email-query-response (first (filter #(= "Email/query" (first %)) method-responses))
        result (:ids (second email-query-response))]
    result))

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
        encoded-request-body (json/encode request-body)
        post-result (http2/post2 url api-token encoded-request-body "application/json; charset=utf-8")]
    (if (success? post-result)
      (let [process-response-result (process-response extract-email-ids (:value post-result))]
        (if (success? process-response-result)
          (log-and-success (:value process-response-result) "fetch-email-ids succeeded")
          (log-and-failure "fetch-email-ids failed" (:error process-response-result))))
      (log-and-failure "fetch-email-ids failed" (:error post-result)))))

(defn extract-email [method-responses]
  (let [email-get-response (first (filter #(= "Email/get" (first %)) method-responses))
        emails (:list (second email-get-response))
        result (first emails)]
    result))

(defn fetch-email
  [session email-id]
  (let [url (:apiUrl session)
        api-token (:apiToken session)
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls
                      [["Email/get"
                        {:accountId (get-account-id session)
                         :ids       [email-id]}
                        "a"]]}
        encoded-request-body (json/encode request-body)
        post-result (http2/post2 url api-token encoded-request-body "application/json; charset=utf-8")]
    (if (success? post-result)
       (let [process-response-result (process-response extract-email (:value post-result))]
         (if (success? process-response-result)
           (log-and-success (:value process-response-result) "fetch-email succeeded")
           (log-and-failure "fetch-email failed" (:error process-response-result))))
       (log-and-failure "fetch-email failed" (:error post-result)))))


(defn get-to-address [email]
  (get-in email [:to 0 :email]))

(def mime->ext
  {"image/jpeg"      ".jpg"
   "image/png"       ".png"
   "image/gif"       ".gif"
   "image/svg+xml"   ".svg"
   "application/pdf" ".pdf"
   "text/plain"      ".txt"
   "text/html"       ".html"})


(defn add-extension-if-missing
  [filename ext]
  (if (or (str/blank? ext)
          (str/ends-with? filename ext))
    filename
    (str filename ext)))

(defn extract-updated [method-responses]
  (let [email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
        result (get-in email-set-response [1 :updated])]
    result))

(defn move-email-to-mailbox
  [session email-id mailbox-id]
  (let [url (:apiUrl session)
        api-token (:apiToken session)
        request-body {:using ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls
                      [["Email/set"
                        {:accountId (get-account-id session)
                         :update    {email-id {:mailboxIds {mailbox-id true}}}}
                        "a"]]}
        encoded-request-body (json/encode request-body)
        post-result (http2/post2 url api-token encoded-request-body "application/json; charset=utf-8")]
    (if (success? post-result)
      (let [process-response-result (process-response extract-updated (:value post-result))]
        (if (success? process-response-result)
          (log-and-success (:value process-response-result) "move-email-to-mailbox succeeded")
          (log-and-failure "move-email-to-mailbox failed" (:error process-response-result))))
      (log-and-failure "move-email-to-mailbox failed" (:error post-result)))))

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

(defn get-file-extension [mime-type]
  (let [extension (get mime->ext mime-type)]
    (if extension
      (log-and-success extension "get-file-extension succeeded")
      (log-and-failure "get-file-extension failed" (str "unexpected mime type " mime-type)))))

(defn create-download-url [session blob-info filename]
  (let [get-file-extension-result (get-file-extension (:type blob-info))]
    (if (success? get-file-extension-result)
      (let [ext (:value get-file-extension-result)
            filename-with-ext (add-extension-if-missing filename ext)
            encoded-filename (url-encode filename-with-ext)
            encoded-type (url-encode (:type blob-info))
            download-url (-> (:downloadUrl session)
                             (str/replace "{accountId}" (get-account-id session))
                             (str/replace "{blobId}" (:blobId blob-info))
                             (str/replace "{name}" encoded-filename)
                             (str/replace "{type}" encoded-type))]
        (log-and-success download-url "create-download-url succeeded"))
      (log-and-failure "create-download-url failed" (:error get-file-extension-result)))))

(defn decode-blob-content [blob-info bytes]
  (let [decoded-content (if (and (:type blob-info)
                                 (or (= (:type blob-info) "application/json")
                                     (= (:type blob-info) "application/xml")))
                          ;; decode text-like content as string
                          (-> (bytes->string bytes :charset "UTF-8")
                              replace-nbsp)
                          ;; else keep raw bytes (e.g. images, pdfs)
                          bytes)]
    decoded-content))

(defn download-blob
  [session blob-info filename]
  (let [create-download-url-result (create-download-url session blob-info filename)]
    (if (success? create-download-url-result)
      (let [api-token (:apiToken session)
            download-url (:value create-download-url-result)
            get-result (http2/get2 download-url api-token)]
        (if (success? get-result)
          (let [bytes (:value get-result)
                decoded-content (decode-blob-content blob-info bytes)]
            ;; don't log the blob, it could be large
            (log-and-success nil "download-blob succeeded")
            (success decoded-content))
          (log-and-failure "download-blob failed" (:error get-result))))
      (log-and-failure "download-blob failed" (:error create-download-url-result)))))

(defn download-blobs
  [session blob-infos]
  (loop [remaining blob-infos
         results []]
    (if (empty? remaining)
      (do
        (tel/log! {:level :debug, :data (success nil)} "download-blobs succeeded")
        (success
          (mapv
            (fn [blob download]
              (assoc blob :value (:value download)))
            blob-infos
            results)))
      (let [blob-info (first remaining)
            filename (str (name (:role blob-info)))         ;; Use role name as filename base
            download-result (download-blob session blob-info filename)]
        (if (:error download-result)
          (log-and-failure "download-blobs failed" (:error download-result))
          (recur (rest remaining)
                 (conj results download-result)))))))

(defn get-upload-url [session]
  (let [upload-url (-> (:uploadUrl session)
                       (str/replace "{accountId}" (get-account-id session)))]
    upload-url))

(defn upload-blob
  [session blob]
  (let [upload-url (get-upload-url session)
        api-token (:apiToken session)
        post-result (http2/post2 upload-url api-token (:value blob) (:type blob))]
    (if (success? post-result)
      (let [body (:value post-result)]
        (if (contains? body :blobId)
          (log-and-success (:blobId body) "upload-blob succeeded")
          (log-and-failure "upload-blob failed")))
      (log-and-failure "upload-blob failed" (:error post-result)))))

(defn get-blobs-info
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

(defn extract-draft-id [method-responses]
  (let [draft-id "draft_message"
        email-set-response (first (filter #(= "Email/set" (first %)) method-responses))
        created (get-in email-set-response [1 :created])
        result (get-in created [(keyword draft-id) :id])]
    result))

(defn create-draft-email
  [session email-object]
  (let [account-id (get-account-id session)
        draft-id "draft_message"
        method-calls [["Email/set"
                       {:accountId account-id
                        :create    {draft-id email-object}}
                       "0"]]
        request-body {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                      :methodCalls method-calls}
        request-body-encoded (json/encode request-body)
        post-result (http2/post2 (:apiUrl session) (:apiToken session) request-body-encoded "application/json; charset=utf-8")]
    (if (success? post-result)
      (let [process-response-result (process-response extract-draft-id (:value post-result))]
        (if (success? process-response-result)
          (log-and-success (:value process-response-result) "create-draft-email succeeded")
          (log-and-failure "create-draft-email failed" (:error process-response-result))))
      (log-and-failure "create-draft-email failed" (:error post-result)))))

(defn upload-attachments
  [session blobs]
  (loop [remaining blobs
         results []]
    (if (empty? remaining)
      (log-and-success results "upload-attachments succeeded")
      (let [blob (first remaining)]
        (if (= (:role blob) :attachment)
          (let [upload-result (upload-blob session blob)]
            (if (:error upload-result)
              (log-and-failure "upload-attachments failed" (:error upload-result))
              (recur (rest remaining)
                     (conj results {:role   (:role blob)
                                    :blobId {:old (:blobId blob)
                                             :new (:value upload-result)}
                                    :type   (:type blob)}))))
          ;; Blob role is not :attachment, return error immediately
          (log-and-failure "upload-attachments failed" "Blob role is not :attachment"))))))

(defn extract-submitted-id [method-responses]
  (let [email-set-response (first (filter #(= "EmailSubmission/set" (first %)) method-responses))
        created (get-in email-set-response [1 :created])
        result (:submission_id created)]
    result))

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
        encoded-request-body (json/encode request-body)
        post-result (http2/post2
                      (:apiUrl session)
                      (:apiToken session)
                      encoded-request-body
                      "application/json; charset=utf-8")]
    (if (success? post-result)
      (let [process-response-result (process-response extract-submitted-id (:value post-result))]
        (if (success? process-response-result)
          (log-and-success (:value process-response-result) "submit-email succeeded")
          (log-and-failure "submit-email failed" (:error process-response-result))))
      (log-and-failure "submit-email failed" (:error post-result)))))


(comment
  (def config (load-config))
  (def fetch-session-result (fetch-session config))
  (def session (:value fetch-session-result))
  (fetch-mailbox-info session)
  (fetch-email-ids session "P-F")
  (str/join " " ["one" "two"])
  nil)

