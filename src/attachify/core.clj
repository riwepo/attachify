(ns attachify.core
  (:require [clojure.pprint :refer [pprint]]
            [taoensso.telemere :as tel]
            [attachify.fastmail :as fm]
            [attachify.config :refer [load-config]]
            [attachify.result :refer [failure failure?]]
            [attachify.result-log :refer [log-and-success log-and-failure]]))

(defn extract-plus-address [to-address]
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
  (tel/log! {:level :debug, :data {:sender-id sender-id :mailbox-info mailbox-info :email-id email-id}} "processing email")
  (let [processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processing")
        move-email-to-processing-result (fm/move-email-to-mailbox session email-id processing-mailbox-id)]
    (if (failure? move-email-to-processing-result)
      (log-and-failure "process-email failed Step 1" (:error move-email-to-processing-result))
      (let [fetch-email-result (fm/fetch-email session email-id)]
        (if (failure? fetch-email-result)
          (log-and-failure "process-email failed Step 2" (:error fetch-email-result))
          (let [email (:value fetch-email-result)
                to-address (fm/get-to-address email)
                resend-username (extract-plus-address to-address)]
            (if (nil? resend-username)
              (log-and-failure "process-email failed Step 3 / plus-addressing failed")
              (let [resend-address (build-resend-address config resend-username)
                    blobs-info (fm/get-blobs-info email)
                    download-blobs-result (fm/download-blobs session blobs-info)]
                (failure? download-blobs-result)
                (log-and-failure "process-email failed Step 4" (:error download-blobs-result))
                (let [blobs (:value download-blobs-result)
                      attachment-blobs (filter #(= (:role %) :attachment) blobs)
                      upload-attachments-result (fm/upload-attachments session attachment-blobs)]
                  (if (failure? upload-attachments-result)
                    (log-and-failure "process-email failed Step 5" (:error upload-attachments-result))
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
                      (if (failure? create-draft-result)
                        (log-and-failure "process-email failed Step 6" (:error create-draft-result))
                        (let [draft-email-id (:value create-draft-result)
                              submit-result (fm/submit-email session draft-email-id sender-id)]
                          (if (failure? submit-result)
                            (log-and-failure "process-email failed Step 7" (:error submit-result))
                            (let [sent-mailbox-id (fm/get-mailbox-id-by-role mailbox-info "sent")
                                  move-email-to-sent-result (fm/move-email-to-mailbox session draft-email-id sent-mailbox-id)]
                              (if (failure? move-email-to-sent-result)
                                (log-and-failure "process-email failed Step 8" (:error move-email-to-sent-result))
                                (let [processed-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processed")
                                      move-email-to-processed-result (fm/move-email-to-mailbox session email-id processed-mailbox-id)]
                                  (if (failure? move-email-to-processed-result)
                                    (log-and-failure "process-email failed Step 9" (:error move-email-to-processed-result))
                                    ;; All steps succeeded
                                    (log-and-success nil "email successfully processed")))))))))))))))))))




(defn process-emails
  []
  (let [config (load-config)
        fetch-session-result (fm/fetch-session config)]
    (if (failure? fetch-session-result)
      (log-and-failure "process-emails failed Step 1" (:error fetch-session-result))
      (let [session (:value fetch-session-result)
            identity-info-result (fm/fetch-identity-info session)]
        (if (failure? identity-info-result)
          (log-and-failure "process-emails failed Step 2" (:error identity-info-result))
          (let [sender-id (:value identity-info-result)
                mailbox-info-result (fm/fetch-mailbox-info session)]
            (if (failure? mailbox-info-result)
              (log-and-failure "process-emails failed Step 3" (:error mailbox-info-result))
              (let [mailbox-info (:value mailbox-info-result)
                    inbox-mailbox-id (fm/get-mailbox-id-by-role mailbox-info "inbox")
                    processing-mailbox-id (fm/get-mailbox-id-by-name mailbox-info "Processing")
                    processing-email-ids-result (fm/fetch-email-ids session processing-mailbox-id)]
                (if (failure? processing-email-ids-result)
                  (log-and-failure "process-emails failed Step 4" (:error processing-email-ids-result))
                  (let [processing-email-ids (:value processing-email-ids-result)
                        inbox-email-ids-result (fm/fetch-email-ids session inbox-mailbox-id)]
                    (if (failure? inbox-email-ids-result)
                      (log-and-failure "process-emails failed Step 5" (:error inbox-email-ids-result))
                      (let [inbox-email-ids (:value inbox-email-ids-result)
                            all-email-ids (concat processing-email-ids inbox-email-ids)]
                        (loop [ids all-email-ids
                               success-count 0]
                          (if (empty? ids)
                            (log-and-success nil "process-emails succeeded")
                            (let [email-id (first ids)
                                  process-email-result (process-email config session sender-id mailbox-info email-id)]
                              (if (failure? process-email-result)
                                (log-and-failure "process-emails failed Step 6" (str "'" email-id "'") (:error process-email-result))
                                (recur (rest ids) (inc success-count))))))))))))))))))






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
  (def email-id (first email-ids))
  (def email (:value (fm/fetch-email session email-id)))
  (pprint email)
  (def blobs-info (fm/get-blobs-info email))
  (pprint blobs-info)
  (def blob-info (first blobs-info))
  (pprint blob-info)
  (def download-blob-result (fm/download-blob session blob-info "fred.txt"))
  (pprint download-blob-result)
  (def download-blobs-result (fm/download-blobs session blobs-info))
  (pprint download-blobs-result)
  (def sender-id (:value (fm/fetch-identity-info session)))
  (pprint sender-id)
  (process-email config session sender-id mailbox-info email-id)



  (get-in email [:to 0 :email])
  (def test-email {:to [{:email "attachify+riwepo.work@fastmail.com"}]})
  (def to-address (fm/get-to-address test-email))
  (def resend-user (extract-plus-address to-address))
  (print resend-user)
  (def resend-address (build-resend-address config resend-user))
  (print resend-address)
  (def process-emails-result (process-emails))
  (pprint process-emails-result)
  nil)




