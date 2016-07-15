(ns tools.mem
  (:require
   [taoensso.carmine :as car]
   [taoensso.nippy :refer (Freezable write-id id-byte-as-long id-short-as-long id-int-as-long)]
   [tools.util :as util :refer (when-let* name-with-attributes)])

  (:import [java.io DataOutput]))


(extend-type Integer ; Optimized common-case type
  Freezable
  (freeze-to-out* [x ^DataOutput out]
    (cond
     (<= Byte/MIN_VALUE x Byte/MAX_VALUE)
     (do (write-id out id-byte-as-long) (.writeByte out x))

     (<= Short/MIN_VALUE x Short/MAX_VALUE)
     (do (write-id out id-short-as-long) (.writeShort out x))

     :else (do (write-id out id-int-as-long) (.writeInt out x)))))

(defprotocol ICacheStore
  (read-cache [_ key])
  (write-cache! [_ key val])
  (delete-cache! [_ key])
  (clear-cache! [_]))

(defrecord RedisCacheStore [conn expires-in]
  ICacheStore
  (read-cache [_ key]
    (car/wcar conn (car/get key)))
  (write-cache! [_ key val]
    (if expires-in
      (car/wcar conn (car/setex key expires-in val))
      (car/wcar conn (car/set key val))))
  (delete-cache! [_ key]
    (car/wcar conn (car/del key))))

(defn redis-cache-store [redis-spec & {:keys [expires-in]}]
  "warnning: redis-cache-store will cache integer as string"
  (RedisCacheStore. redis-spec expires-in))

(defn cache
  [f & {:as opts
        :keys [box-name store cached-key]
        :or {cached-key cons}}]
  (let [fm (meta f)
        box-name (or box-name (util/box-name f))]
    (-> (fn [& args]
          (let [key (cached-key box-name args)]
            (if-let [val (read-cache store key)]
              val
              (let [val (apply f args)]
                (write-cache! store key val)
                val))))
        (vary-meta merge (merge opts {:raw-fn f})))))

(defmacro def-cache-fn
  [name & fdecl]
  (let [[name fdecl] (name-with-attributes name fdecl)]
    `(let [var# (defn ~name ~@fdecl)
           box-name# (util/box-name var#)]
       (when-let* [{store# :store cached-key# :cached-key} (meta var#)
                   meta# {:store store# :box-name box-name#
                          :cached-key cached-key# :raw-fn (var-get var#)}]
         (alter-var-root var# #(vary-meta (apply cache % (apply concat meta#))
                                          merge meta#)))
       var#)))

(defn raw-call
  [f & args]
  (apply (-> f meta :raw-fn) args))

(defn invalid-cache!
  [f & args]
  (let [{:keys [store box-name cached-key]
         :or {cached-key cons}} (meta f)]
    (delete-cache! store (cached-key box-name args))))

(defmacro def-uncache-fn
  [name & fdecl]
  (let [[name fdecl] (name-with-attributes name fdecl)
        {:keys [cached-fn cached-args]
         :or {cached-args identity}} (meta name)]
    `(doto (defn ~name ~@fdecl)
       (alter-var-root (fn [f#]
                         (fn [& args#]
                           (try
                             (apply f# args#)
                             (finally (apply invalid-cache! ~cached-fn (~cached-args args#))))))))))
