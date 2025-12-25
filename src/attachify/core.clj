(ns attachify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [attachify.fastmail :as fm]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))

(defn get-to-address [config]
  (:to-address config))

(defn process-email
  [config session sender-id mailbox-info email-id]
  (let [processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processing")
        move-email-to-processing-result (fm/move-email-to-mailbox session email-id processing-mailbox-id)]
    (if (:error move-email-to-processing-result)
      (do
        (println "Step 1: Error moving email to Processing folder:" (:error-message move-email-to-processing-result))
        move-email-to-processing-result)
      (let [fetch-email-result (fm/fetch-email session email-id)]
        (if (:error fetch-email-result)
          (do
            (println "Step 2: Error fetching email:" (:error-message fetch-email-result))
            fetch-email-result)
          (let [email (:value fetch-email-result)
                blob-info (fm/get-blob-info email)
                download-blobs-result (fm/download-blobs session blob-info)]
            (if (:error download-blobs-result)
              (do
                (println "Step 3: Error downloading blobs:" (:error-message download-blobs-result))
                download-blobs-result)
              (let [blobs (:value download-blobs-result)
                    upload-attachments-result (fm/upload-attachments session blobs)]
                (if (:error upload-attachments-result)
                  (do
                    (println "Step 4: Error uploading attachments:" (:error-message upload-attachments-result))
                    upload-attachments-result)
                  (let [attachment-info (:value upload-attachments-result)
                        from-address (:from-address config)
                        to-address (get-to-address config)
                        draft-email-object (fm/build-draft-email
                                             blobs
                                             attachment-info
                                             from-address
                                             to-address
                                             (fm/get-mailbox-id-by-role mailbox-info "drafts")
                                             (:subject email))
                        create-draft-result (fm/create-draft-email session draft-email-object)]
                    (if (:error create-draft-result)
                      (do
                        (println "Step 5: Failed to create draft:" (:error-message create-draft-result))
                        create-draft-result)
                      (let [draft-email-id (:value create-draft-result)
                            submit-result (fm/submit-email session draft-email-id sender-id)]
                        (if (:error submit-result)
                          (do
                            (println "Step 6: Error submitting draft email:" (:error-message submit-result))
                            submit-result)
                          (let [sent-mailbox-id (fm/get-mailbox-id-by-role mailbox-info "sent")
                                move-email-to-sent-result (fm/move-email-to-mailbox session draft-email-id sent-mailbox-id)]
                            (if (:error move-email-to-sent-result)
                              (do
                                (println "Step 7: Error moving email to Sent:" (:error-message move-email-to-sent-result))
                                move-email-to-sent-result)
                              (let [processed-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processed")
                                    move-email-to-processed-result (fm/move-email-to-mailbox session email-id processed-mailbox-id)]
                                (if (:error move-email-to-processed-result)
                                  (do
                                    (println "Step 8: Error moving email to Processed folder:" (:error-message move-email-to-processed-result))
                                    move-email-to-processed-result)
                                  ;; All steps succeeded
                                  {:success true
                                   :error false
                                   :error-message nil
                                   :value true})))))))))))))))))



(defn process-emails
  []
  (let [config (load-config)
        fetch-session-result (fm/fetch-session config)]
    (if (:error fetch-session-result)
      (do
        (println "Error fetching session:" (:error fetch-session-result))
        fetch-session-result)
      (let [session (:value fetch-session-result)
            identity-info-result (fm/fetch-identity-info session)]
        (if (:error identity-info-result)
          (do
            (println "Error fetching identity info:" (:error identity-info-result))
            identity-info-result)
          (let [identity-info (:value identity-info-result)
                sender-id (fm/get-identity-id identity-info)
                mailbox-info-result (fm/fetch-mailbox-info session)]
            (if (:error mailbox-info-result)
              (do
                (println "Error fetching mailbox info:" (:error mailbox-info-result))
                mailbox-info-result)
              (let [mailbox-info (:value mailbox-info-result)
                    from-address (:from-address config)
                    inbox-mailbox-id (fm/get-mailbox-id-by-role mailbox-info "inbox")
                    processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processing")
                    processing-email-ids-result (fm/fetch-email-ids session processing-mailbox-id)]
                (if (:error processing-email-ids-result)
                  (do
                    (println "Error fetching email IDs from Processing mailbox:" (:error processing-email-ids-result))
                    processing-email-ids-result)
                  (let [processing-email-ids (:value processing-email-ids-result)
                        inbox-email-ids-result (fm/fetch-email-ids session inbox-mailbox-id)]
                    (if (:error inbox-email-ids-result)
                      (do
                        (println "Error fetching email IDs from Inbox mailbox:" (:error inbox-email-ids-result))
                        inbox-email-ids-result)
                      (let [inbox-email-ids (:value inbox-email-ids-result)
                            all-email-ids (concat processing-email-ids inbox-email-ids)]
                        (doseq [email-id all-email-ids]
                          (println "Processing email id:" email-id)
                          (let [result (process-email session sender-id mailbox-info from-address email-id)]
                            (when (:error result)
                              (println "Error processing email id" email-id ":" (:error result)))))
                        {:value true}))))))))))))



(comment
  (def process-emails-result (process-emails))
  (pprint process-emails-result)
  nil)


