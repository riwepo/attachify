(ns attachify.result)

(defn failure [msg]
  {:success       false
   :error         true
   :error-message msg
   :value         nil})

(defn success
  ([]
   {:success       true
    :error         false
    :error-message nil
    :value         true})
  ([value]
   {:success       true
    :error         false
    :error-message nil
    :value         value}))

