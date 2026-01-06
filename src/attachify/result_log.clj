(ns attachify.result-log
  (:require
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [attachify.result :refer [success failure]]))

(tel/set-min-level! :debug)

(defn log-and-failure [& msgs]
  (let [combined-msg (str/join " " msgs)
        result (failure combined-msg)]
    (tel/log! {:level :error, :data result})
    result))

(defn log-and-success
  ([message]
   (let [result (success)]
     (tel/log! {:level :debug, :data result} message)
     result))
  ([message object]
   (let [result (success object)]
     (tel/log! {:level :debug, :data result} message)
     result)))

(comment

  nil)





