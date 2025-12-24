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
  (let [processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-data "Processing")
        drafts-id (fm/get-mailbox-id-by-role mailbox-data "drafts")
        processed-id (fm/get-mailbox-by-role mailbox-data "processed")
        move-to-processing-result (fm/move-email-to-mailbox session email-id processing-mailbox-id)]
    (if (:error move-to-processing-result)
      (do
        (println "Error moving email to Processing folder:" (:error-message move-to-processing-result))
        move-to-processing-result)
      ;; Step 2: Fetch full email data with error handling
      (let [fetch-email-result (fm/fetch-email-by-id session email-id)]
        (if (:error fetch-email-result)
          (do
            (println "Error fetching email:" (:error-message fetch-email-result))
            fetch-email-result)
          (let [email (:value fetch-email-result)
                blob-info (fm/get-blob-info email)
                blobs-result (fm/download-all-blobs session blob-info)]
            (if (:error blobs-result)
              (do
                (println "Error downloading blobs:" (:error-message blobs-result))
                blobs-result)
              (let [blobs (:value blobs-result)
                    draft-email-object (fm/build-draft-email
                                         blobs
                                         ;; Pass empty attachment-info or adapt as needed
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
                        ;; Step 5: Delete draft email
                        delete-draft-result (fm/delete-email session draft-id)
                        ;; Step 6: Move source email to Processed folder
                        move-to-processed-result (fm/move-email-to-mailbox session email-id processed-id)]
                    ;; Log steps
                    (pprint {:step "Sent draft email" :response send-result})
                    (pprint {:step "Deleted draft email" :response delete-draft-result})
                    (pprint {:step "Moved email to Processed folder" :response move-to-processed-result})
                    {:success true}))))))))))

;; Note: You need to implement fm/delete-email, which calls Email/set with :destroy [draft-id]

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


