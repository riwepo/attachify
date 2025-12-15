(ns attachify.core
  (:require [yogthos.config :as config]))

(def app-config (config/load-config))

(println "API Token:" (:api-token app-config))