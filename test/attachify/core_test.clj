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
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]
                    :attachments [{:name "fred" :blobId 1234 :type "image/jpg"}]}
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

(t/deftest process-email-move-to-sent-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id 1234
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-error-message "move-email-to-mailbox failed / mock error"
        mock-blobs {}
        mock-submitted-email-id 5678
        call-count (atom 0)
        success-result (res/success mock-email)
        failure-result (res/failure mock-error-message)
        mock-move-email-to-mailbox
        (fn [_session _email-id _mailbox-id]
          (swap! call-count inc)
          (if (= @call-count 1)
            success-result
            failure-result))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/success {}))
        mock-create-draft-email (fn [_session _email]
                                  (res/success 1234))
        mock-submit-email (fn [_session _email-id _identity_id]
                            (res/success mock-submitted-email-id))]

    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments
                  fm/create-draft-email mock-create-draft-email
                  fm/submit-email mock-submit-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 8 / " mock-error-message) (:error result)))))))

(t/deftest process-email-move-to-processed-fail
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id 1234
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-blobs {}
        mock-submitted-email-id 5678
        mock-error-message "move-email-to-mailbox failed / mock error"
        call-count (atom 0)
        success-result (res/success mock-email)
        failure-result (res/failure mock-error-message)
        mock-move-email-to-mailbox
        (fn [_session _email-id _mailbox-id]
          (swap! call-count inc)
          (if (= @call-count 3)
            failure-result
            success-result))

        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/success {}))
        mock-create-draft-email (fn [_session _email]
                                  (res/success 1234))
        mock-submit-email (fn [_session _email-id _identity_id]
                            (res/success mock-submitted-email-id))]

    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments
                  fm/create-draft-email mock-create-draft-email
                  fm/submit-email mock-submit-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/failure result))
        (t/is (= (str "process-email failed Step 9 / " mock-error-message) (:error result)))))))

(t/deftest process-email-success
  (let [config (cfg/load-config)
        mock-session {}
        mock-sender-id "sender-id"
        mock-mailbox-info {}
        mock-email-id 12
        mock-email {:to [{:email "attachify+riwepo@fastmail.com"}]}
        mock-blobs {}
        mock-moved-email-id 34
        mock-submitted-email-id 56
        mock-move-email-to-mailbox (fn [_session _email-id _mailbox-id]
                                     (res/success mock-moved-email-id))
        mock-fetch-email (fn [_session _email-id]
                           (res/success mock-email))
        mock-download-blobs (fn [_session _email-id]
                              (res/success mock-blobs))
        mock-upload-attachments (fn [_session _email-id]
                                  (res/success {}))
        mock-create-draft-email (fn [_session _email]
                                  (res/success 1234))
        mock-submit-email (fn [_session _email-id _identity_id]
                            (res/success mock-submitted-email-id))]

    (with-redefs [fm/move-email-to-mailbox mock-move-email-to-mailbox
                  fm/fetch-email mock-fetch-email
                  fm/download-blobs mock-download-blobs
                  fm/upload-attachments mock-upload-attachments
                  fm/create-draft-email mock-create-draft-email
                  fm/submit-email mock-submit-email]
      (let [result (c/process-email config mock-session mock-sender-id mock-mailbox-info mock-email-id)]
        (t/is (res/success result))))))


(t/deftest process-emails-fetch-session-fail
  (let [mock-error-message "fetch-session failed / mock error"
        mock-fetch-session (fn [_config]
                             (res/failure mock-error-message))]
    (with-redefs [fm/fetch-session mock-fetch-session]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 1 / " mock-error-message) (:error result)))))))

(t/deftest process-emails-fetch-identity-info-fail
  (let [mock-error-message "fetch-identity-info failed / mock error"
        mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock_fetch-identity-info (fn [_session]
                                   (res/failure mock-error-message))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 2 / " mock-error-message) (:error result)))))))

(t/deftest process-emails-fetch-mailbox-info-fail
  (let [mock-error-message "fetch-mailbox-info failed / mock error"
        mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock-identity-info {}
        mock_fetch-identity-info (fn [_session]
                                   (res/success mock-identity-info))
        mock_fetch-mailbox-info (fn [_session]
                                  (res/failure mock-error-message))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info
                  fm/fetch-mailbox-info mock_fetch-mailbox-info]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 3 / " mock-error-message) (:error result)))))))

(t/deftest process-emails-fetch-processing-email-ids-fail
  (let [mock-error-message "fetch-email-ids failed / mock error"
        mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock-identity-info {}
        mock_fetch-identity-info (fn [_session]
                                   (res/success mock-identity-info))
        mock-mailbox-info {}
        mock_fetch-mailbox-info (fn [_session]
                                  (res/success mock-mailbox-info))
        mock_fetch-email-ids (fn [_session _mailbox-id]
                               (res/failure mock-error-message))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info
                  fm/fetch-mailbox-info mock_fetch-mailbox-info
                  fm/fetch-email-ids mock_fetch-email-ids]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 4 / " mock-error-message) (:error result)))))))

(t/deftest process-emails-fetch-inbox-email-ids-fail
  (let [mock-error-message "fetch-email-ids failed / mock error"
        mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock-identity-info {}
        mock_fetch-identity-info (fn [_session]
                                   (res/success mock-identity-info))
        mock-mailbox-info {}
        mock_fetch-mailbox-info (fn [_session]
                                  (res/success mock-mailbox-info))
        mock-email-ids [1 2 3 4]
        call-count (atom 0)
        success-result (res/success mock-email-ids)
        failure-result (res/failure mock-error-message)
        mock_fetch-email-ids
        (fn [_session _mailbox-id]
          (swap! call-count inc)
          (if (= @call-count 2)
            failure-result
            success-result))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info
                  fm/fetch-mailbox-info mock_fetch-mailbox-info
                  fm/fetch-email-ids mock_fetch-email-ids]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 5 / " mock-error-message) (:error result)))))))

(t/deftest process-emails-process-email-fail
  (let [mock-error-message "process-email failed / mock error"
        mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock-identity-info {}
        mock_fetch-identity-info (fn [_session]
                                   (res/success mock-identity-info))
        mock-mailbox-info {}
        mock_fetch-mailbox-info (fn [_session]
                                  (res/success mock-mailbox-info))
        mock-email-ids [1 2 3 4]
        mock_fetch-email-ids
        (fn [_session _mailbox-id]
          (res/success mock-email-ids))
        mock-process-email (fn [_config _session _sender_id _mailbox-info _email-id]
                             (res/failure mock-error-message))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info
                  fm/fetch-mailbox-info mock_fetch-mailbox-info
                  fm/fetch-email-ids mock_fetch-email-ids
                  c/process-email mock-process-email]
      (let [result (c/process-emails)]
        (t/is (res/failure result))
        (t/is (= (str "process-emails failed Step 6 / '1' / " mock-error-message) (:error result)))))))

(t/deftest process-emails-success
  (let [mock-session {}
        mock-fetch-session (fn [_config]
                             (res/success mock-session))
        mock-identity-info {}
        mock_fetch-identity-info (fn [_session]
                                   (res/success mock-identity-info))
        mock-mailbox-info {}
        mock_fetch-mailbox-info (fn [_session]
                                  (res/success mock-mailbox-info))
        mock-email-ids [1 2 3 4]
        mock_fetch-email-ids (fn [_session _mailbox-id]
                               (res/success mock-email-ids))
        mock-process-email (fn [_config _session _sender-id _mailbox-info _email-id]
                             (res/success nil))]
    (with-redefs [fm/fetch-session mock-fetch-session
                  fm/fetch-identity-info mock_fetch-identity-info
                  fm/fetch-mailbox-info mock_fetch-mailbox-info
                  fm/fetch-email-ids mock_fetch-email-ids
                  c/process-email mock-process-email]
      (let [result (c/process-emails)]
        (t/is (res/success result))))))

















