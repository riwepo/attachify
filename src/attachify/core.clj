(ns attachify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [attachify.fastmail :as fm]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))

(defn process-email
  [session sender-id mailbox-info email-id from-address to-address]
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


;; Note: You need to implement fm/delete-email, which calls Email/set with :destroy [draft-id]

;(defn process-emails
;  []
;  (let [config (load-config)
;        session (fm/fetch-session config)
;        identity-data (fm/fetch-identity-data session)
;        identity-id (fm/get-identity identity-data)
;        mailbox-data (fm/fetch-mailbox-data session)
;        inbox-id (fm/get-inbox-id mailbox-data)
;        processing-id (some (fn [mbox] (when (= (:name mbox) "Processing") (:id mbox)))
;                            (:list mailbox-data))
;        ;; Get email ids in Processing folder first, then Inbox
;        processing-email-ids (fm/fetch-email-ids session processing-id)
;        inbox-email-ids (fm/fetch-email-ids session inbox-id)
;        all-email-ids (concat processing-email-ids inbox-email-ids)]
;    (doseq [email-id all-email-ids]
;      (println "Processing email id:" email-id)
;      (process-email session identity-id mailbox-data email-id))))


(comment
  (defn load-config
    []
    (-> "resources/config.edn"
        slurp
        edn/read-string))
  (def config (load-config))
  (def fetch-session-result (fm/fetch-session config))
  (pprint fetch-session-result)
  (def session (:value fetch-session-result))
  (def fetch-identity-info-result (fm/fetch-identity-info session))
  (def identity-info (:value fetch-identity-info-result))
  (pprint identity-info)
  (def sender-id (fm/get-identity-id identity-info))
  (println sender-id)
  (def fetch-mailbox-info-result (fm/fetch-mailbox-info session))
  (def mailbox-info (:value fetch-mailbox-info-result))
  (pprint mailbox-info)
  (def inbox-id (fm/get-mailbox-id-by-role mailbox-info "inbox"))
  (println inbox-id)
  (def fetch-email-ids-result (fm/fetch-email-ids session (fm/get-mailbox-id-by-role mailbox-info "inbox")))
  (pprint fetch-email-ids-result)
  (def inbox-email-ids (:value fetch-email-ids-result))
  (pprint inbox-email-ids)
  (def email-id (first inbox-email-ids))
  (def process-email-result (process-email
                              session
                              sender-id
                              mailbox-info
                              email-id
                              "attachify@fastmail.com"
                              "riwepo.work@gmail.com"))
  (pprint process-email-result)

  nil)


