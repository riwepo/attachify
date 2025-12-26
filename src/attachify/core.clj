(ns attachify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [taoensso.timbre :as timbre]
            [attachify.fastmail :as fm]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))

(defn extract-resend-username [to-address]
  (let [pattern #"^attachify\+(.+)@fastmail\.com$"
        matcher (re-matches pattern to-address)]
    (when matcher
      (second matcher))))

(defn build-resend-address [config resend-username]
  (let [resend-domain (:resend-domain config)
        resend-address (str resend-username "@" resend-domain)]
    resend-address))

(defn process-email
  [config session sender-id mailbox-info email-id]
  (timbre/debug (str "processing email " email-id))
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
                to-address (fm/get-to-address email)
                resend-username (extract-resend-username to-address)]
            (if (nil? resend-username)
              (do
                (println "Error: resend-username is nil, cannot proceed")
                {:error true
                 :error-message (str "resend-username not found in to-address " (str "'" to-address "'"))
                 :value nil})
              (let [resend-address (build-resend-address config resend-username)
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
                            draft-email-object (fm/build-draft-email
                                                 blobs
                                                 attachment-info
                                                 from-address
                                                 resend-address
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
                                       :value true})))))))))))))))))))




(defn process-emails
  []
  (let [config (load-config)
        fetch-session-result (fm/fetch-session config)]
    (if (:error fetch-session-result)
      (do
        (println "Error fetching session:" (:error-message fetch-session-result))
        fetch-session-result)
      (let [session (:value fetch-session-result)
            identity-info-result (fm/fetch-identity-info session)]
        (if (:error identity-info-result)
          (do
            (println "Error fetching identity info:" (:error-message identity-info-result))
            identity-info-result)
          (let [identity-info (:value identity-info-result)
                sender-id (fm/get-identity-id identity-info)
                mailbox-info-result (fm/fetch-mailbox-info session)]
            (if (:error mailbox-info-result)
              (do
                (println "Error fetching mailbox info:" (:error-message mailbox-info-result))
                mailbox-info-result)
              (let [mailbox-info (:value mailbox-info-result)
                    inbox-mailbox-id (fm/get-mailbox-id-by-role mailbox-info "inbox")
                    processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processing")
                    processing-email-ids-result (fm/fetch-email-ids session processing-mailbox-id)]
                (if (:error processing-email-ids-result)
                  (do
                    (println "Error fetching email IDs from Processing mailbox:" (:error-message processing-email-ids-result))
                    processing-email-ids-result)
                  (let [processing-email-ids (:value processing-email-ids-result)
                        inbox-email-ids-result (fm/fetch-email-ids session inbox-mailbox-id)]
                    (if (:error inbox-email-ids-result)
                      (do
                        (println "Error fetching email IDs from Inbox mailbox:" (:error-message inbox-email-ids-result))
                        inbox-email-ids-result)
                      (let [inbox-email-ids (:value inbox-email-ids-result)
                            all-email-ids (concat processing-email-ids inbox-email-ids)
                            success-count (atom 0)]
                        (doseq [email-id all-email-ids]
                          (let [result (process-email config session sender-id mailbox-info email-id)]
                            (if (:error result)
                              (println "Error processing email id" (str "'" email-id "'") ":" (:error-message result))
                              (swap! success-count inc))))
                        {:value @success-count}))))))))))))






(comment
  (def config (load-config))
  (pprint config)
  (def session (:value (fm/fetch-session config)))
  (pprint session)
  (def mailbox-info (:value (fm/fetch-mailbox-info session)))
  (pprint mailbox-info)
  (def inbox-id (fm/get-mailbox-id-by-role mailbox-info "inbox"))
  (pprint inbox-id)
  (def email-ids (:value (fm/fetch-email-ids session inbox-id)))
  (pprint email-ids)
  (def email (:value (fm/fetch-email session (first email-ids))))
  (pprint email)
  (get-in email [:to 0 :email])
  (def test-email {:to [{:email "attachify+riwepo.work@fastmail.com"}]})
  (def to-address (fm/get-to-address test-email))
  (def resend-user (extract-resend-username to-address))
  (print resend-user)
  (def resend-address (build-resend-address config resend-user))
  (print resend-address)
  (def process-emails-result (process-emails))
  (pprint process-emails-result)
  nil)


