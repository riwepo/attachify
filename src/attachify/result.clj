(ns attachify.result
  (:require
    [clojure.string :as str]))

(defn failure [& msgs]
  (let [combined-message (str/join " " msgs)]
    (println combined-message)
    {:status :error
     :error  combined-message}))

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

(comment
  (failure "1" "2")

  nil)

