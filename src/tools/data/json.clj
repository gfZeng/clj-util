(ns tools.data.json
  (:refer-clojure :exclude [load])
  (:require
   [clojure.string :as string]
   [clj-time.core :as t]
   [clj-time.coerce :as cc]
   [clojure.data.json :as json :refer [JSONWriter -write]]

   [tools.util :refer (as-joda-time)]))


(extend-protocol JSONWriter
  java.sql.Timestamp
  (-write [obj out] (-write (java.util.Date. (.getTime obj)) out))
  java.util.Date
  (-write [obj out] (-write (-> (cc/from-date obj)
                                as-joda-time
                                str)
                            out))
  org.joda.time.DateTime
  (-write [obj out] (-write (str obj) out))
  java.net.Inet4Address
  (-write [obj out] (-write (str obj) out)))

(defn as-clj-keyword
  [x]
  (if (string? x)
    (keyword (string/replace x #"_" "-"))
    (as-clj-keyword (name x))))

(defn as-json-key
  [x]
  (if (string? x)
    (string/replace x #"-" "_")
    (as-json-key (name x))))

(defn load
  [reader & opts]
  (apply json/read reader (concat opts [:key-fn as-clj-keyword])))

(defn load-str
  [s & opts]
  (apply load (java.io.StringReader. s) opts))

(defn dump
  [x writer & opts]
  (apply json/write x writer (concat opts [:key-fn as-json-key
                                           :escape-unicode false
                                           :escape-js-separators false
                                           :escape-slash false])))


(defn dump-str
  [s & opts]
  (let [writer (java.io.StringWriter.)]
    (apply dump s writer opts)
    (.toString writer)))
