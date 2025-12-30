(ns attachify.config
  (:require [clojure.edn :as edn]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))
