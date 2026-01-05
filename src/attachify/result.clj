(ns attachify.result)

(defn failure [msg]
  {:status :error
   :error  msg})

(defn success
  ([]
   {:status :ok})
  ([value]
   {:status :ok
    :value  value}))

(defn success?
  [result]
  (= :ok (:status result)))

(defn failure?
  [result]
  (not= :ok (:status result)))

