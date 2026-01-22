(ns attachify.http-test
  (:require [clojure.test :as t]
            [clj-http.client :as http]
            [attachify.result :refer [success? failure?]]
            [attachify.config :refer :all]
            [attachify.http :refer :all]))

(t/deftest get2-fail-bad-url
  (let [bad-url "some-dodgy-url"
        config (load-config)
        token (:email-api-token config)
        mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 bad-url token :text)]
        (t/is (failure? result))
        (t/is (= "get2 failed / some-dodgy-url / 404" (:error result)))))))

(t/deftest get2-fail-bad-token
  (let [config (load-config)
        url (get-email-auth-url config)
        token "some-dodgy-token"
        mock-get (fn [_url _opts]
                   {:status 401
                    :body "Not Authorized"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 url token :text)]
        (t/is (failure? result))
        (t/is (= "get2 failed / https://api.fastmail.com/.well-known/jmap / 401" (:error result)))))))

(t/deftest get2-success
  (let [config (load-config)
        url (get-email-auth-url config)
        token (:email-api-token config)
        mock-get (fn [_url _opts]
                   {:status 200
                    :body "the body"})]
    (with-redefs [http/get mock-get]
      (let [result (get2 url token :text)]
        (t/is (success? result))
        (t/is (= "the body" (:value result)))))))

(t/deftest post2-bad-url
  (let [bad-url "some-dodgy-url"
        config (load-config)
        token (:email-api-token config)
        content "content"
        content-type "text/plain"
        mock-post (fn [_url _opts]
                    {:status 404
                     :body "Not Found"})]
    (with-redefs [http/post mock-post]
      (let [result (post2 bad-url token content content-type)]
        (t/is (failure? result))
        (t/is (= "post2 failed / 404 / some-dodgy-url" (:error result)))))))

(def api-url "https://api.fastmail.com/jmap/api/")

(t/deftest post2-bad-token
  (let [url api-url
        token "some-dodgy-token"
        content "content"
        content-type "text/plain"
        mock-post (fn [_url _opts]
                    {:status 401
                     :body "Not Authorized"})]
    (with-redefs [http/post mock-post]
      (let [result (post2 url token content content-type)]
        (t/is (failure? result))
        (t/is (= (str "post2 failed / 401 / " api-url) (:error result)))))))

(t/deftest post2-bad-request
  (let [config (load-config)
        url api-url
        token (:email-api-token config)
        content nil
        content-type "text/plain"
        mock-post (fn [_url _opts]
                    {:status 400
                     :body "Bad Request"})]
    (with-redefs [http/post mock-post]
      (let [result (post2 url token content content-type)]
        (t/is (failure? result))
        (t/is (= (str "post2 failed / 400 / " api-url) (:error result)))))))

(t/deftest post2-success
  (let [config (load-config)
        url api-url
        token (:email-api-token config)
        content "content"
        content-type "text/plain"
        mock-post (fn [_url _opts]
                    {:status 200
                     :body "the body"})]
    (with-redefs [http/post mock-post]
      (let [result (post2 url token content content-type)]
        (t/is (success? result))
        (t/is (= "the body" (:value result)))))))



