(ns attachify.fastmail-test
  (:require [clojure.test :refer :all]
            [clj-http.client :as http]
            [attachify.fastmail :refer :all]))

(deftest fetch-session-bad-url-test
  (let [mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})
        config {:email-api-token "dummy-token"}
        ;; mock get-email-auth-url to return a dummy URL
        mock-get-email-auth-url (fn [_config] "http://bad-url")]
    (with-redefs [http/get mock-get
                  get-email-auth-url mock-get-email-auth-url]
      (let [result (fetch-session config)]
        (is (= {:error "Failed to fetch session, status: 404"} result))))))