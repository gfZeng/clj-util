(ns tools.ring.helper
  (:refer-clojure :exclude [swap! get get-in assoc!])
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [tools.util :as util]
            [ring.util.response :refer (content-type)])

  (:import [clojure.lang ExceptionInfo]))


(defn json->clj [m]
  (util/clojurize m {:key-fn (comp keyword util/underscore->dash)}))

(defn- edn-request? [req]
  (re-find #"edn" (clojure.core/get-in req [:headers "content-type"] "")))

(defn wrap-edn-params [handler]
  (fn [req]
    (let [req (if (edn-request? req)
                (let [body (edn/read-string (slurp (:body req)))]
                  (cond-> req
                    true (assoc :body body)
                    (map? body) (update :params merge body)))
                req)]
      (handler req))))

(defn- as-edn [x]
  (if (coll? x) (pr-str x) x))

(defn edn-response [res]
  (let [res (update res :body as-edn)]
    (if (contains? (:headers res) "Content-Type")
      res
      (content-type res "application/edn; charset=utf-8"))))

(defn translate-msg [key]
  (string/replace (name key) #"_|-" " "))

(defn fail [msg]
  (let [msg (if (string? msg)
              msg
              (translate-msg msg))]
    (throw (ex-info msg {:error {:danger msg}}))))

(defn wrap-error-response [handler]
  (fn [req]
    (try
      (handler req)
      (catch ExceptionInfo e
        {:status 200
         :body (ex-data e)}))))

(defn wrap-clear-thread-local [handler]
  (fn [req]
    (let [res (handler req)]
      (clear-thread-local!)
      res)))

(defn wrap-debug [handler & [debug-keys]]
  (fn [req]
    (doto (handler req)
      (println ">>>>>>>>>>>>>>>"))))


;;; context globally
(def ^:dynamic *ctxt* (atom nil))

(defn get
  ([key]
   (get key nil))
  ([key not-found]
   (clojure.core/get @*ctxt* key not-found)))

(defn get-in
  ([keys]
   (get-in keys nil))
  ([keys not-found]
   (clojure.core/get-in @*ctxt* keys not-found)))

(defn swap! [f & args]
  (apply clojure.core/swap! *ctxt* f args))

(defn assoc! [key val]
  (swap! assoc key val))

(defn assoc-in! [keys val]
  (swap! assoc-in keys val))

(def put! assoc!)

(defn update! [key f & args]
  (apply swap! update key f args))

(defn update-in! [keys f & args]
  (apply swap! update-in keys f args))

(defn wrap-context [handler]
  (fn [req]
    (binding [*ctxt* (atom {})]
      (handler req))))
