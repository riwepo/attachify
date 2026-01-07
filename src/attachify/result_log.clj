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
  [& args]
  (println args)
  (let [[object msgs] (if (and (seq args) (not (string? (first args))))
                        ;; first arg is object, rest are messages
                        [(first args) (rest args)]
                        ;; no object, all args are messages
                        [nil args])
        combined-message (str/join " " msgs)
        result (if object
                 (success object)
                 (success))]
    (tel/log! {:level :debug, :data result} combined-message)
    result))

(comment

  nil)





