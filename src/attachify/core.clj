(ns attachify.core
  (:require [config.core :refer [env]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))



(def myEnv (edn/read-string (slurp "resources/config.edn")))

(comment
  (io/resource "resources/config.edn")
  (slurp (io/resource "config.edn"))
  (slurp  "resources/config.edn")
  (println myEnv)
  (println "API Token:" (:fastmail_api_token myEnv))
  nil)


