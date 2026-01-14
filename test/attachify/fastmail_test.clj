(ns attachify.fastmail-test
  (:require [clojure.test :as t]
            [cheshire.core :as json]
            [attachify.result :as res]
            [attachify.config :as cfg]
            [attachify.http :as http]
            [attachify.fastmail :as fm]))

(t/deftest fetch-session-get2-fail-test
  (let [error-message "get2 failed"
        config (cfg/load-config)
        mock-get2 (fn [_url _api-token]
                    (res/failure error-message))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-session failed " error-message) (:error result)))))))

(t/deftest fetch-session-format-fail-test
  (let [error-message "Invalid session format"
        config (cfg/load-config)
        mock-get2 (fn [_url _api-token]
                    (res/success "some dodgy value"))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-session failed " error-message) (:error result)))))))

(t/deftest fetch-session-success-test
  (let [
        config (cfg/load-config)
        mock-session {:apiToken (:email-api-token config)}
        mock-get2 (fn [_url _api-token]
                    (res/success mock-session))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/success? result))
        (t/is (= mock-session (:value result)))))))

(t/deftest fetch-identity-info-post2-fail-test
  (let [error-message "post2 fail"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-identity-info failed " error-message) (:error result)))))))

(t/deftest fetch-identity-info-format-fail-test
  (let [error-message "Neither Identity/get nor error response found"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-identity-info failed " error-message) (:error result)))))))

(t/deftest fetch-identity-info-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-id 123
        mock-response-body {:methodResponses [["Identity/get" {:list [{:id mock-id}]}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/success? result))
        (t/is (= mock-id (:value result)))))))

(t/deftest fetch-mailbox-info-post2-fail-test
  (let [error-message "post2 fail"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-mailbox-info failed " error-message) (:error result)))))))

(t/deftest fetch-mailbox-info-format-fail-test
  (let [error-message "Neither Mailbox/get nor error response found"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content-type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-mailbox-info failed " error-message) (:error result)))))))

(t/deftest fetch-mailbox-info-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-mailbox-info [{:role "inbox" :name "Inbox" :id "P-Y"}]
        mock-response-body {:methodResponses [["Mailbox/get" {:list mock-mailbox-info}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/success? result))
        (t/is (= mock-mailbox-info (:value result)))))))

(t/deftest fetch-email-ids-post2-fail-test
  (let [error-message "post2 fail"
        mock-mailbox-id "P-Y"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email-ids mock-session mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-email-ids failed " error-message) (:error result)))))))

(t/deftest fetch-email-ids-format-fail-test
  (let [error-message "Neither Email/query nor error response found"
        mock-mailbox-id "P-Y"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email-ids mock-session mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-email-ids failed " error-message) (:error result)))))))

(t/deftest fetch-email-ids-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-mailbox-id "P-F"
        mock-email-ids ["1" "2" "3"]
        mock-response-body {:methodResponses [["Email/query" {:ids mock-email-ids}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email-ids mock-session mock-mailbox-id)]
        (t/is (res/success? result))
        (t/is (= mock-email-ids (:value result)))))))

(t/deftest fetch-email-post2-fail-test
  (let [error-message "post2 fail"
        mock-email-id "email-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email mock-session mock-email-id)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-email failed " error-message) (:error result)))))))

(t/deftest fetch-email-format-fail-test
  (let [error-message "Neither Email/get nor error response found"
        mock-email-id "email-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email mock-session mock-email-id)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-email failed " error-message) (:error result)))))))

(t/deftest fetch-email-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-email-id "email-id"
        mock-email {:to "fred" :from "nerk"}
        mock-response-body {:methodResponses [["Email/query" {:list [mock-email]}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email mock-session mock-email-id)]
        (t/is (res/success? result))
        (t/is (= mock-email (:value result)))))))

(t/deftest move-email-to-mailbox-post2-fail-test
  (let [error-message "post2 fail"
        mock-email-id "email-id"
        mock-mailbox-id "mailbox-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/move-email-to-mailbox mock-session mock-email-id mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= (str "move-email-to-mailbox failed " error-message) (:error result)))))))

(t/deftest move-email-to-mailbox-format-fail-test
  (let [error-message "No Email/set response found"
        mock-email-id "email-id"
        mock-mailbox-id "mailbox-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/move-email-to-mailbox mock-session mock-email-id mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= (str "move-email-to-mailbox failed " error-message) (:error result)))))))

(t/deftest move-email-to-mailbox-not-created-fail-test
  (let [error-message "not created response received"
        mock-email-id "email-id"
        mock-mailbox-id "mailbox-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-response-body {:methodResponses [["Email/set" {:notCreated ["email-id"]}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/move-email-to-mailbox mock-session mock-email-id mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= (str "move-email-to-mailbox failed " error-message) (:error result)))))))

(t/deftest move-email-to-mailbox-success-test
  (let [mock-email-id "email-id"
        mock-mailbox-id "mailbox-id"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-response-body {:methodResponses [["Email/set"]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/move-email-to-mailbox mock-session mock-email-id mock-mailbox-id)]
        (t/is (res/success? result))
        (t/is (nil? (:value result)))))))

(t/deftest get-file-extension-fail-test
  (let [mock-mime-type "dodgy-mime-type"
        result (fm/get-file-extension mock-mime-type)]
    (t/is (res/failure? result))
    (t/is (= "get-file-extension failed unexpected mime type dodgy-mime-type" (:error result)))))

(t/deftest get-file-extension-success-test
  (let [mock-mime-type "image/jpeg"
        result (fm/get-file-extension mock-mime-type)]
    (t/is (res/success? result))
    (t/is (= ".jpg" (:value result)))))

(t/deftest create-download-url-fail-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-blob-info {:type "dodgy-mime-type"}
        mock-filename "filename"
        result (fm/create-download-url mock-session mock-blob-info mock-filename)]
    (t/is (res/failure? result))
    (t/is (= "create-download-url failed get-file-extension failed unexpected mime type dodgy-mime-type" (:error result)))))

(t/deftest create-download-url-success-test
  (let [mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :downloadUrl     "{accountId}/{blobId}/{name}/{type}"}
        mock-blob-info {:blobId "blobId" :type "image/jpeg"}
        mock-filename "filename"
        result (fm/create-download-url mock-session mock-blob-info mock-filename)]
    (t/is (res/success? result))
    (t/is (= "account-name/blobId/filename.jpg/image%2Fjpeg" (:value result)))))

(t/deftest download-blob-create-download-url-fail-test
  (let [error-message "create-download-url failed"
        mock-session {}
        mock-blob-info {}
        mock-filename "filename"
        mock-create-download-url (fn [_session _blob-info _filename]
                                   (res/failure error-message))]
    (with-redefs [fm/create-download-url mock-create-download-url]
      (let [result (fm/download-blob mock-session mock-blob-info mock-filename)]
        (t/is (res/failure? result))
        (t/is (= (str "download-blob failed " error-message) (:error result)))))))

(t/deftest download-blob-get2-fail-test
  (let [error-message "get2 failed"
        mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :downloadUrl     "{accountId}/{blobId}/{name}/{type}"}
        mock-blob-info {:blobId "blobId" :type "image/jpeg"}
        mock-filename "filename"
        mock-get2 (fn [_url _api-token]
                    (res/failure error-message))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/download-blob mock-session mock-blob-info mock-filename)]
        (t/is (res/failure? result))
        (t/is (= (str "download-blob failed " error-message) (:error result)))))))

(t/deftest download-blob-success-test
  (let [mock-session {}
        mock-blob-info {}
        mock-filename "filename"
        mock-blob-content "mock blob content"
        mock-create-download-url (fn [_session _blob-info _filename]
                                   (res/success "download-url"))
        mock-get2 (fn [_url _api-token]
                    (res/success mock-blob-content))]
    (with-redefs [fm/create-download-url mock-create-download-url http/get2 mock-get2]
      (let [result (fm/download-blob mock-session mock-blob-info mock-filename)]
        (t/is (res/success? result))
        (t/is (= mock-blob-content (:value result)))))))

(t/deftest download-blobs-download-blob-fail-test
  (let [mock-error-message "download-blob failed"
        mock-session {}
        mock-blob-infos [{:role :mockRole}]
        mock-download_blob (fn [_session _blob-info _filename]
                             (res/failure mock-error-message))]
    (with-redefs [fm/download-blob mock-download_blob]
      (let [result (fm/download-blobs mock-session mock-blob-infos)]
        (t/is (res/failure? result))
        (t/is (= (str "download-blobs failed " mock-error-message) (:error result)))))))

(t/deftest download-blobs-success-test
  (let [mock-session {}
        mock-blob-content "mock-blob-content"
        mock-blob-infos [{:role :mockRole}]
        mock-download_blob (fn [_session _blob-info _filename]
                             (res/success mock-blob-content))]
    (with-redefs [fm/download-blob mock-download_blob]
      (let [result (fm/download-blobs mock-session mock-blob-infos)]
        (t/is (res/success result))
        (t/is (= [{:role :mockRole :value mock-blob-content}] (:value result)))))))

(t/deftest upload-blob-post2-fail-test
  (let [error-message "post2 failed"
        mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :uploadUrl       "{accountId}/{blobId}/{name}/{type}"}
        mock-blob {:blobId "blobId" :type "image/jpeg"}
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/upload-blob mock-session mock-blob)]
        (t/is (res/failure? result))
        (t/is (= (str "upload-blob failed " error-message) (:error result)))))))

(t/deftest upload-blob-success-test
  (let [mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :uploadUrl       "{accountId}/{blobId}/{name}/{type}"}
        mock-blob {:blobId "blobId" :type "image/jpeg"}
        mock-blob-id 1234
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success {:status 200 :headers {"Content-Type" "application/json"} :body (json/encode {:blobId mock-blob-id})}))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/upload-blob mock-session mock-blob)]
        (t/is (res/success? result))
        (t/is (= mock-blob-id (:value result)))))))

(t/deftest get-blobs-info-test
  (let [mock-email {:textBody    [{:blobId 1 :type "text-type"}]
                    :htmlBody    [{:blobId 2 :type "html-type"}]
                    :attachments [{:blobId 3 :type "attachment"}]}
        result (fm/get-blobs-info mock-email)]
    (t/is (= result [{:blobId 1
                      :role   :textBody
                      :type   "text-type"}
                     {:blobId 2
                      :role   :htmlBody
                      :type   "html-type"}
                     {:blobId 3
                      :name   nil
                      :role   :attachment
                      :type   "attachment"}]))))

(t/deftest build-draft-email-test
  (let [mock-blobs [{:role :textBody :value "some text"}
                    {:role :htmlBody :value "some html"}
                    {:role :attachment :value "attachment 1" :blobId 1 :name "name 1" :type ".jpg"}
                    {:role :attachment :value "attachment 2" :blobId 2 :name "name 2" :type ".png"}]
        mock-attachment-info []
        mock-from "sender"
        mock-to "receiver"
        mock-drafts-id "draftsId"
        mock-subject "the subject"
        result (fm/build-draft-email
                 mock-blobs
                 mock-attachment-info
                 mock-from
                 mock-to
                 mock-drafts-id
                 mock-subject)]
    (t/is (= {:from        [{:email mock-from}]
              :to          [{:email mock-to}]
              :subject     mock-subject
              :bodyValues  {"html" {:charset "utf-8"
                                    :value   "some html"}
                            "text" {:charset "utf-8"
                                    :value   "some text"}}
              :textBody    [{:partId "text"}]
              :htmlBody    [{:partId "html"}]
              :mailboxIds  {mock-drafts-id true}
              :attachments [{:blobId      nil
                             :disposition "attachment"
                             :name        "name 1"
                             :type        ".jpg"}
                            {:blobId      nil
                             :disposition "attachment"
                             :name        "name 2"
                             :type        ".png"}]} result))))









