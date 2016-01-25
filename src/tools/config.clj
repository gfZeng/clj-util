(ns tools.config
  (:refer-clojure :exclude [read set])
  (:require
   [taoensso.carmine :as car]))


(defprotocol IStore
  (read [_ key])
  (write [_ key val]))

(defn set [key val] val)

(defmacro config!
  [store key & body]
  `(let [new-val# (->
                   (read ~store ~key)
                   ~@body)]
     (write ~store ~key new-val#)
     new-val#))

(defrecord RedisStore [spec]
    IStore
  (read [_ key]
    (car/wcar spec (if (vector? key)
                     (apply car/hget key)
                     (car/get key))))

  (write [_ key val]
    (car/wcar spec (if (vector? key)
                     (car/hset (first key) (second key) val)
                     (car/set key val)))))

(defn redis-store
  [spec]
  (RedisStore. spec))

;; TODO
(defrecord PostgresStore [db-spec table]
  )


(defmacro defconfig [store]
  `(list
    (defn ~'read' [key#]
      (read ~store key#))
    (defmacro ~'config'! [key# & body#]
      `(config! ~'~store ~key# ~@body#))))
