(ns attachify.result
  (:require [taoensso.timbre :as timbre]))

(defn failure [msg]
  (timbre/error msg)
  {:success       false
   :error         true
   :error-message msg
   :value         nil})

(defn success [value msg]
  (timbre/debug msg)
  {:success       true
   :error         false
   :error-message nil
   :value         value})
