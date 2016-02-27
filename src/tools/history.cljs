(ns tools.history
  (:require [goog.events :as events]
            [goog.history.EventType :as EventType])
  (:import goog.History))

(defonce HISTORY (History.))

(defn listen:navigate [f]
  (goog.events/listen HISTORY EventType/NAVIGATE f)
  (doto HISTORY (.setEnabled true)))


(defn token []
  (.getToken HISTORY))

(defn set-token! [t]
  (.setToken HISTORY t))
