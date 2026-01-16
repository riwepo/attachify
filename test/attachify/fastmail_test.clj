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
  (let [error-message "post2 failed"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= (str "fetch-identity-info failed " error-message) (:error result)))))))

(t/deftest fetch-identity-info-API-fail-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-error-type "the type"
        mock-error-arguments "the arguments"
        mock-response-body {:methodResponses [["error" {:arguments mock-error-arguments :type mock-error-type}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= "fetch-identity-info failed post response failed with API error: type the type, arguments: the arguments" (:error result)))))))

(t/deftest fetch-identity-info-unknown-fail-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= "fetch-identity-info failed post response failed with unknown error" (:error result)))))))

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

(t/deftest fetch-mailbox-info-API-fail-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-error-type "the type"
        mock-error-arguments "the arguments"
        mock-response-body {:methodResponses [["error" {:arguments mock-error-arguments :type mock-error-type}]]}
        mock-post2 (fn [_url _api-token _request_body _content-type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= "fetch-mailbox-info failed post response failed with API error: type the type, arguments: the arguments" (:error result)))))))

(t/deftest fetch-mailbox-info-unknown-fail-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content-type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= "fetch-mailbox-info failed post response failed with unknown error" (:error result)))))))


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

(t/deftest fetch-email-ids-API-fail-test
  (let [mock-mailbox-id "P-Y"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-error-type "the type"
        mock-error-arguments "the arguments"
        mock-response-body {:methodResponses [["error" {:arguments mock-error-arguments :type mock-error-type}]]}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email-ids mock-session mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= "fetch-email-ids failed post response failed with API error: type the type, arguments: the arguments" (:error result)))))))

(t/deftest fetch-email-ids-unknown-fail-test
  (let [mock-mailbox-id "P-Y"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body _content_type]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-email-ids mock-session mock-mailbox-id)]
        (t/is (res/failure? result))
        (t/is (= "fetch-email-ids failed post response failed with unknown error" (:error result)))))))


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

(t/deftest get-blobs-info-success-test
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

(t/deftest build-draft-email-success-test
  (let [mock-blobs [{:role :textBody :value "some text"}
                    {:role :htmlBody :value "some html"}
                    {:role :attachment :value "attachment 1" :blobId 1 :name "name 1" :type ".jpg"}
                    {:role :attachment :value "attachment 2" :blobId 2 :name "name 2" :type ".png"}]
        mock-attachment-info [{:role :attachment :type "image/jpg" :blobId {:old 1 :new 3}}
                              {:role :attachment :type "image/png" :blobId {:old 2 :new 4}}]
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
              :attachments [{:blobId      3
                             :disposition "attachment"
                             :name        "name 1"
                             :type        ".jpg"}
                            {:blobId      4
                             :disposition "attachment"
                             :name        "name 2"
                             :type        ".png"}]} result))))

(t/deftest create-draft-email-post2-fail-test
  (let [error-message "post2 failed"
        mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :uploadUrl       "{accountId}/{blobId}/{name}/{type}"}
        mock-email {:blobId "blobId" :type "image/jpeg"}
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/create-draft-email mock-session mock-email)]
        (t/is (res/failure? result))
        (t/is (= (str "create-draft-email failed " error-message) (:error result)))))))

(t/deftest create-draft-email-API-error-fail-test
  (let [mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-post-response-body {:methodResponses [["error" ""]]}
        mock-email "mock email"
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success mock-post-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/create-draft-email mock-session mock-email)]
        (t/is (res/failure? result))
        (t/is (= "create-draft-email failed API error: , arguments: " (:error result)))))))

(t/deftest create-draft-email-unexpected-error-fail-test
  (let [mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-post-response-body "some dodgy response"
        mock-email "mock email"
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success mock-post-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/create-draft-email mock-session mock-email)]
        (t/is (res/failure? result))
        (t/is (= "create-draft-email failed Unknown error creating draft email" (:error result)))))))

(t/deftest create-draft-email-success-test
  (let [mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-created-email-id 666
        mock-post-response-body {:methodResponses [["Email/set" {:created {:draft_message {:id mock-created-email-id}}}]]}
        mock-email "mock email"
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success mock-post-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/create-draft-email mock-session mock-email)]
        (t/is (res/success? result))
        (t/is (= mock-created-email-id (:value result)))))))

(t/deftest upload-attachments-blob-not-attachment-fail-test
  (let [mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-blobs [1 2 3]
        mock-upload-blob (fn [_session _blob]
                           (res/success nil))]
    (with-redefs [fm/upload-blob mock-upload-blob]
      (let [result (fm/upload-attachments mock-session mock-blobs)]
        (t/is (res/failure? result))
        (t/is (= (str "upload-attachments failed Blob role is not :attachment") (:error result)))))))

(t/deftest upload-attachments-upload-blob-fail-test
  (let [mock-error-message "upload-blob failed"
        mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-blobs [{:role :attachment}]
        mock-upload-blob (fn [_session _blob]
                           (res/failure mock-error-message))]
    (with-redefs [fm/upload-blob mock-upload-blob]
      (let [result (fm/upload-attachments mock-session mock-blobs)]
        (t/is (res/failure? result))
        (t/is (= (str "upload-attachments failed " mock-error-message) (:error result)))))))

(t/deftest upload-attachments-success-test
  (let [mock-session {:apiUrl   "url"
                      :apiToken "token"}
        mock-blobs [{:role :attachment :blobId 1 :type "image/jpeg"}
                    {:role :attachment :blobId 2 :type "image/png"}]
        mock-upload-blob (fn [_session blob]
                           (res/success (+ 10 (:blobId blob))))]
    (with-redefs [fm/upload-blob mock-upload-blob]
      (let [result (fm/upload-attachments mock-session mock-blobs)]
        (t/is (res/success? result))
        (t/is (= [{:blobId {:new 11
                            :old 1}
                   :role   :attachment
                   :type   "image/jpeg"}
                  {:blobId {:new 12
                            :old 2}
                   :role   :attachment
                   :type   "image/png"}] (:value result)))))))

(t/deftest submit-email-post2-fail-test
  (let [mock-error-message "post2 failed"
        mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}
                      :uploadUrl       "{accountId}/{blobId}/{name}/{type}"}
        mock-email-id 1234
        mock-identity-id "identity"
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/failure mock-error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/submit-email mock-session mock-email-id mock-identity-id)]
        (t/is (res/failure? result))
        (t/is (= (str "submit-email failed " mock-error-message) (:error result)))))))

(t/deftest submit-email-API-error-fail-test
  (let [mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}}
        mock-email-id 1234
        mock-identity-id "identity"
        mock-error-type "the type"
        mock-error-arguments "the arguments"
        mock-response-body {:methodResponses [["error" {:arguments mock-error-arguments :type mock-error-type}]]}
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/submit-email mock-session mock-email-id mock-identity-id)]
        (t/is (res/failure? result))
        (t/is (= (str "submit-email failed API error: type: " mock-error-type ", arguments: " mock-error-arguments) (:error result)))))))

(t/deftest submit-email-unexpected-error-fail-test
  (let [mock-session {:apiUrl          "url"
                      :apiToken        "token"
                      :primaryAccounts {:urn:ietf:params:jmap:mail "account-name"}}
        mock-email-id 1234
        mock-identity-id "identity"
        mock-response-body "some-dodgy-response"
        mock-post2 (fn [_url _api-token _content _content_type]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/submit-email mock-session mock-email-id mock-identity-id)]
        (t/is (res/failure? result))
        (t/is (= "submit-email failed with unexpected error" (:error result)))))))












