(ns tools.net.http
  (:require
   [clojure.string :as string]
   #?(:clj
      [ring.util.codec :as codec]))
  (#?(:clj :require :cljs :require-macros)
   [clojure.core.strint :refer (<<)]))


#?(:clj
   (do
     (defn url-encode [s] (java.net.URLEncoder/encode s "UTF-8"))
     (defn url-decode [s] (java.net.URLDecoder/decode s "UTF-8")))
   :cljs
   (do
     (defn url-encode [s] (js/encodeURIComponent s))
     (defn url-decode [s] (js/decodeURIComponent s))))

(defn get-domain
  [url]
  (nth (string/split url #"/") 2))

(defn flatten-params
  ([params]
   (apply conj {}
          (for [[k v] params]
            (flatten-params (name k) v))))
  ([prefix params]
   (if (map? params)
     (apply conj {}
            (for [[k v] params]
              (flatten-params (<< "~{prefix}[~(name k)]") v)))
     {prefix params})))

#?(:clj
   (defn rebuild-url
     [url query-params]
     (let [[uri query-str] (string/split url #"\?")
           params (when query-str (codec/form-decode query-str))
           params (merge params (flatten-params query-params))]
       (str uri "?" (string/join "&" (for [[k v] params]
                                       (str (url-encode k) "=" (url-encode ((if (keyword? v) name str) v)))))))))

(defn satisfy-uri
  [base-uri uri]
  (cond
    (empty? base-uri) uri
    (zero? (.indexOf uri "http")) uri
    (zero? (.indexOf uri base-uri)) uri
    :else   (str base-uri uri)))
