(ns attachify.http-test
  (:require [clojure.test :refer :all]
            [clj-http.client :as http]
            [attachify.config :refer :all]
            [attachify.http :refer :all]))

(deftest get2-bad-url-test
  (let [bad-url "some-dodgy-url"
        config (load-config)
        token (:email-api-token config)
        mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 bad-url token)]
        (println result)
        (is (= true (:error result)))
        (is (= "Error 404 http/get URL some-dodgy-url" (:error-message result)))))))

(deftest get2-bad-token-test
  (let [config (load-config)
        url (get-email-auth-url config)
        token "some-dodgy-token"
        mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 url token)]
        (println result)
        (is (= true (:error result)))
        (is (= "Error 404 http/get URL https://api.fastmail.com/.well-known/jmap" (:error-message result)))))))