(ns attachify.fastmail-test
  (:require [clojure.test :refer :all]
            [clj-http.client :as http]
            [attachify.http :refer :all]
            [attachify.config :refer :all]))

(deftest get2-bad-url-test
  (let [config (load-config)
        mock-get (fn [_url _opts]
                   {:status 404
                    :body "Not Found"})
        bad-url "some-dodgy-url"]
    (with-redefs [http/get mock-get]
      (let [result (get2 url)]
        (is (= {:error "Failed to fetch session, status: 404"} result))))))