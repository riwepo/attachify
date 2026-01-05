(ns attachify.fastmail-test
  (:require [clojure.test :as t]
            [attachify.config :as cfg]
            [attachify.http :as http]
            [attachify.fastmail :as fm]))

(t/deftest fetch-session-get2-fail-test
  (let [error-message "get2 fail"
        config (cfg/load-config)
        mock-get2 (fn [_url _api-token]
                    {:error true :error-message error-message})]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (= true (:error result)))
        (t/is (= error-message (:error-message result)))))))

(t/deftest fetch-session-success-test
  (let [
        config (cfg/load-config)
        mock-session {:api-token (:email-api-token config)}
        mock-get2 (fn [_url _api-token]
                    {:success true :value mock-session})]
    (with-redefs [http/get2 mock-get2]
      (let [result (fm/fetch-session config)]
        (t/is (= true (:success result)))
        (t/is (= mock-session (:value result)))))))




