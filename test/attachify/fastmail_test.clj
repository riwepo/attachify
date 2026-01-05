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




