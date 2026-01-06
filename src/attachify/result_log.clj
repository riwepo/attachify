(ns attachify.result-log
  (:require
    [taoensso.telemere :as tel]
    [attachify.result :refer [success failure]]))

(tel/set-min-level! :debug)

(defn log-and-failure [msg]
  (let [result (failure msg)]
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





