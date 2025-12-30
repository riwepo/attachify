(ns attachify.result-log
  (:require
    [taoensso.timbre :refer [debug error]]
    [attachify.result :refer [success failure]]))

(defn log-and-failure [msg]
  (error msg)
  (failure msg))

(defn log-and-success
  ([]
   (debug "Success")
   (success))
  ([object]
   (debug "Success" object)
   (success object)))



