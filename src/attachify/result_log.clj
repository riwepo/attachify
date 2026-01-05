(ns attachify.result-log
  (:require
    [taoensso.telemere :as tel]
    [attachify.result :refer [success failure]]))

(defn log-and-failure [msg]
  (let [result (failure msg)]
    (tel/log! {:level :error, :result result})
    result))

(defn log-and-success
  ([]
   (let [result (success)]
     (tel/log! {:level :debug, :result result})
     result))
  ([object]
   (let [result (success object)]
     (tel/log! {:level :debug, :result result})
     result)))





