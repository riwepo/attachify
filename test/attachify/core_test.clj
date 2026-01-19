(ns attachify.core-test
  (:require [clojure.test :as t]
            [attachify.result :as res]
            [attachify.config :as cfg]
            [attachify.fastmail :as fm]
            [attachify.core :as c]))

(t/deftest extract-plus-address-no-payload
  (let [address "fred@fastmail.com"
        result (c/extract-plus-address address)]
    (t/is (= nil result))))

(t/deftest extract-plus-address-bad-domain
  (let [address "fred+bill@fastmailxxx.com"
        result (c/extract-plus-address address)]
    (t/is (= nil result))))

(t/deftest extract-plus-address-bad-address
  (let [address "attachifyxx+bill@fastmail.com"
        result (c/extract-plus-address address)]
    (t/is (= nil result))))

(t/deftest extract-plus-address-success
  (let [address "attachify+bill@fastmail.com"
        result (c/extract-plus-address address)]
    (t/is (= "bill" result))))

(t/deftest build-resend-address
  (let [config (cfg/load-config)
        mock-recipient "recipient"
        result (c/build-resend-address config mock-recipient)]
    (t/is (= "recipient@hotmail.com" result))))

(t/deftest process-email-move-to-processing-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-error-message "move-email-to-mailbox failed / mock error"
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/failure mock-error-message))]
    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 1 / " mock-error-message) (:error result)))))))

(t/deftest process-email-fetch-email-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-error-message "fetch-email failed / mock error"
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/failure mock-error-message))]
    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 2 / " mock-error-message) (:error result)))))))

(t/deftest process-email-extract-plus-address-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-email {:to [{:email "mock-recipient"}]}
        mock-error-message "plus-addressing failed"
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))]
    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 3 / " mock-error-message) (:error result)))))))

(t/deftest process-email-download-blobs-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-error-message "download-blobs failed / mock error"
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/failure mock-error-message))]
    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 4 / " mock-error-message) (:error result)))))))

(t/deftest process-email-upload-attachments-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-error-message "upload-attachments failed / mock error"
        mock-blobs {}
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/failure mock-error-message))]
    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 5 / " mock-error-message) (:error result)))))))

(t/deftest process-email-create-draft-email-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-error-message "create-draft-email failed / mock error"
        mock-blobs {}
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/success {}))
        mock-create-draft-email (fn [_session _email-id]
                                  (res/failure mock-error-message))]

    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments
                  fm/create-draft-email mock-create-draft-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 6 / " mock-error-message) (:error result)))))))

(t/deftest process-email-submit-draft-email-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id {}
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-error-message "submit-email failed / mock error"
        mock-blobs {}
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success {:created 1234}))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/success {}))
        mock-create-draft-email (fn [_session _email]
                                  (res/success 1234))
        mock-submit-email (fn [_session _email-id _identity_id]
                            (res/failure mock-error-message))]

    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments
                  fm/create-draft-email mock-create-draft-email
                  fm/submit-email mock-submit-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 7 / " mock-error-message) (:error result)))))))








