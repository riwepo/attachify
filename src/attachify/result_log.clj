(ns attachify.result-log
  (:require
    [taoensso.telemere :as tel]
    [attachify.result :refer [success failure]]))

(defn log-and-failure [msg]
  (tel/log! {:level :error, :success false :msg msg})
  (failure msg))

(defn log-and-success
  ([]
   (tel/log! {:level :debug, :success true})
   (success))
  ([object]
   (tel/log! {:level :debug, :success true :value object})
   (success object)))



