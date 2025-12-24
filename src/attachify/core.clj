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
        ;; Step 1: Move email to Processing folder
        move-to-processing-result (fm/move-email-to-mailbox session email-id processing-mailbox-id)]
    (if (:error move-to-processing-result)
      (do
        (println "Step 1: Error moving email to Processing folder:" (:error-message move-to-processing-result))
        move-to-processing-result)
      ;; Step 2: Fetch full email data
      (let [fetch-email-result (fm/fetch-email-by-id session email-id)]
        (if (:error fetch-email-result)
          (do
            (println "Step 2: Error fetching email:" (:error-message fetch-email-result))
            fetch-email-result)
          (let [email (:value fetch-email-result)
                blob-info (fm/get-blob-info email)
                ;; Step 3: Download blobs
                blobs-result (fm/download-all-blobs session blob-info)]
            (if (:error blobs-result)
              (do
                (println "Step 3: Error downloading blobs:" (:error-message blobs-result))
                blobs-result)
              (let [blobs (:value blobs-result)
                    ;; Step 4: Upload all attachments with error handling
                    upload-attachments-result (fm/upload-all-attachments session blobs)]
                (if (:error upload-attachments-result)
                  (do
                    (println "Step 4: Error uploading attachments:" (:error-message upload-attachments-result))
                    upload-attachments-result)
                  (let [attachment-info (:value upload-attachments-result)
                        draft-email-object (fm/build-draft-email
                                             blobs
                                             attachment-info
                                             (get-in email [:from 0 :email])
                                             (get-in email [:to 0 :email])
                                             drafts-id
                                             (:subject email))
                        ;; Step 5: Create draft email
                        create-draft-result (fm/create-draft-email session draft-email-object)]
                    (if (:error create-draft-result)
                      (do
                        (println "Step 5: Failed to create draft:" (:error-message create-draft-result))
                        create-draft-result)
                      (let [draft-id (:value create-draft-result)
                            ;; Step 6: Send draft email
                            send-result (fm/send-email session draft-email-object (get-in email [:to 0 :email]))
                            ;; Step 7: Delete draft email
                            delete-draft-result (fm/delete-email session draft-id)
                            ;; Step 8: Move source email to Processed folder
                            move-to-processed-result (fm/move-email-to-mailbox session email-id processed-id)]
                        ;; Log steps
                        (pprint {:step "Step 6: Sent draft email" :response send-result})
                        (pprint {:step "Step 7: Deleted draft email" :response delete-draft-result})
                        (pprint {:step "Step 8: Moved email to Processed folder" :response move-to-processed-result})
                        {:success true}))))))))))))

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


