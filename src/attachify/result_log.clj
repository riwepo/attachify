(ns attachify.result-log
  (:require
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [attachify.result :refer [success failure]]))

(tel/set-min-level! :debug)

(defn log-and-failure [& msgs]
  (let [result (apply failure msgs)]
    (tel/log! {:level :error, :data result})
    result))

(defn log-and-success
  [value & msgs]
  (let [combined-message (str/join " " msgs)
        result (success value)]
    (tel/log! {:level :debug, :data result} combined-message)
    result))

(comment
  nil)





