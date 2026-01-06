(ns attachify.fastmail-test
  (:require [clojure.test :as t]
            [attachify.result :as res]
            [attachify.config :as cfg]
            [attachify.http :as http]
            [attachify.fastmail :as fm]))

(t/deftest fetch-session-get2-fail-test
  (let [error-message "get2 fail"
        config (cfg/load-config)
        mock-get2 (fn [_url _api-token]
                    (res/failure error-message))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-session-format-fail-test
  (let [error-message "Invalid session format"
        config (cfg/load-config)
        mock-get2 (fn [_url _api-token]
                    (res/success "some dodgy value"))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-session-success-test
  (let [
        config (cfg/load-config)
        mock-session {:api-token (:email-api-token config)}
        mock-get2 (fn [_url _api-token]
                    (res/success mock-session))]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (res/success? result))
        (t/is (= mock-session (:value result)))))))

(t/deftest fetch-identity-info-post2-fail-test
  (let [error-message "post2 fail"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-identity-info-format-fail-test
  (let [error-message "Neither Identity/get nor error response found"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-identity-info-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-id 123
        mock-response-body {:methodResponses [["Identity/get" {:list [{:id mock-id}]}]]}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-identity-info mock-session)]
        (t/is (res/success? result))
        (t/is (= mock-id (:value result)))))))

(t/deftest fetch-mailbox-info-post2-fail-test
  (let [error-message "post2 fail"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/failure error-message))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-mailbox-info-format-fail-test
  (let [error-message "Neither Mailbox/get nor error response found"
        mock-session {:apiUrl "url" :apiToken "token"}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/success "some dodgy response"))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/failure? result))
        (t/is (= error-message (:error result)))))))

(t/deftest fetch-mailbox-info-success-test
  (let [mock-session {:apiUrl "url" :apiToken "token"}
        mock-id 123
        mock-response-body {:methodResponses [["Mailbox/get" {:list [{:id mock-id}]}]]}
        mock-post2 (fn [_url _api-token _request_body]
                     (res/success mock-response-body))]
    (with-redefs [http/post2 mock-post2]
      (let [result (fm/fetch-mailbox-info mock-session)]
        (t/is (res/success? result))
        (t/is (= mock-id (:value result)))))))





