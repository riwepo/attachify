(ns attachify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [attachify.fastmail :as fm]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))

(defn process-email
  [session identity-id mailbox-data email-id]
  (let [drafts-id (fm/get-drafts-id mailbox-data)
        processed-id (fm/get-processed-id mailbox-data)
        processing-id (some (fn [mbox] (when (= (:name mbox) "Processing") (:id mbox)))
                            (:list mailbox-data))
        ;; Step 1: Move source email to Processing folder
        move-to-processing-payload {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                                    :methodCalls [["Email/set"
                                                   {:accountId (fm/get-account-id session)
                                                    :update {email-id {:mailboxIds {processing-id true}}}}
                                                   "a"]]}
        move-response (fm/http-post (fm/get-api-url session)
                                    {:headers {"Authorization" (str "Bearer " (:api-token session))
                                               "Content-Type" "application/json; charset=utf-8"}
                                     :body (fm/json-encode move-to-processing-payload)
                                     :as :auto})]
    (pprint {:step "Moved to Processing" :response (:body move-response)})

    ;; Step 2: Fetch full email data
    (let [email (fm/fetch-email-by-id session email-id)
          ;; Step 3: Create draft copy of email (you may need to adapt this to your build-draft-email)
          draft-email-object (fm/build-draft-email
                               (fm/download-all-blobs session (fm/get-blob-info email))
                               ;; Assume attachments are uploaded and you have attachment-info
                               ;; For simplicity, pass empty attachment-info here or adapt as needed
                               []
                               (get-in email [:from 0 :email])
                               (get-in email [:to 0 :email])
                               drafts-id
                               (:subject email))
          create-draft-result (fm/create-draft-email session draft-email-object)]
      (if (:error create-draft-result)
        (do
          (println "Failed to create draft:" (:error-message create-draft-result))
          create-draft-result)
        (let [draft-id (:value create-draft-result)
              ;; Step 4: Send draft email
              send-result (fm/send-email session draft-email-object (get-in email [:to 0 :email]))
              ;; Step 5: Delete draft email (optional, you can implement Email/set update to remove draft)
              delete-draft-payload {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                                    :methodCalls [["Email/set"
                                                   {:accountId (fm/get-account-id session)
                                                    :destroy [draft-id]}
                                                   "b"]]}
              delete-response (fm/http-post (fm/get-api-url session)
                                            {:headers {"Authorization" (str "Bearer " (:api-token session))
                                                       "Content-Type" "application/json; charset=utf-8"}
                                             :body (fm/json-encode delete-draft-payload)
                                             :as :auto})
              ;; Step 6: Move source email to Processed folder
              move-to-processed-payload {:using       ["urn:ietf:params:jmap:core" "urn:ietf:params:jmap:mail"]
                                         :methodCalls [["Email/set"
                                                        {:accountId (fm/get-account-id session)
                                                         :update {email-id {:mailboxIds {processed-id true}}}}
                                                        "c"]]}
              move-processed-response (fm/http-post (fm/get-api-url session)
                                                    {:headers {"Authorization" (str "Bearer " (:api-token session))
                                                               "Content-Type" "application/json; charset=utf-8"}
                                                     :body (fm/json-encode move-to-processed-payload)
                                                     :as :auto})]
          (pprint {:step "Sent draft email" :response (:body send-result)})
          (pprint {:step "Deleted draft" :response (:body delete-response)})
          (pprint {:step "Moved to Processed" :response (:body move-processed-response)})
          {:success true})))))

(defn process-emails
  []
  (let [config (load-config)
        session (fm/fetch-session config)
        identity-data (fm/fetch-identity-data session)
        identity-id (fm/get-identity identity-data)
        mailbox-data (fm/fetch-mailbox-data session)
        inbox-id (fm/get-inbox-id mailbox-data)
        processing-id (some (fn [mbox] (when (= (:name mbox) "Processing") (:id mbox)))
                            (:list mailbox-data))
        ;; Get email ids in Processing folder first, then Inbox
        processing-email-ids (fm/fetch-email-ids session processing-id)
        inbox-email-ids (fm/fetch-email-ids session inbox-id)
        all-email-ids (concat processing-email-ids inbox-email-ids)]
    (doseq [email-id all-email-ids]
      (println "Processing email id:" email-id)
      (process-email session identity-id mailbox-data email-id))))


