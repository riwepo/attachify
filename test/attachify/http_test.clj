(ns attachify.http-test
  (:require [clojure.test :as t]
            [clj-http.client :as http]
            [attachify.result :refer [success? failure?]]
            [attachify.config :refer :all]
            [attachify.http :refer :all]))

(t/deftest get2-bad-url-test
  (let [bad-url "some-dodgy-url"
        config (load-config)
        token (:email-api-token config)
        mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 bad-url token)]
        (t/is (failure? result))
        (t/is (= "get2 failed some-dodgy-url 404" (:error result)))))))

(t/deftest get2-bad-token-test
  (let [config (load-config)
        url (get-email-auth-url config)
        token "some-dodgy-token"
        mock-get (fn [_url _opts]
                   {:status 401
                    :body "Not Authorized"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 url token)]
        (t/is (failure? result))
        (t/is (= "get2 failed https://api.fastmail.com/.well-known/jmap 401" (:error result)))))))

(t/deftest get2-success-test
  (let [config (load-config)
        url (get-email-auth-url config)
        token (:email-api-token config)
        mock-get (fn [_url _opts]
                   {:status 200
                    :body "the body"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 url token)]
        (t/is (success? result))
        (t/is (= "the body" (:value result)))))))

