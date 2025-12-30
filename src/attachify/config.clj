(ns attachify.config
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn load-config []
  (-> "resources/config.edn"
      slurp
      edn/read-string))

(defn get-email-auth-url
  [config]
  (let [hostname (:email-hostname config)
        auth-url-template (:email-auth-url config)]
    (str/replace auth-url-template "{hostname}" hostname)))
