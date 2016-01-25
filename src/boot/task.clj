(ns boot.task
  (:require [boot.core :refer (deftask with-pre-wrap)]))

(deftask server [m method METHOD sym "Launch method"
                 j join      bool "Server thread join?"
                 p port PORT int  "Server listening port number"]
  (with-pre-wrap fileset
    (when-let [n (symbol (namespace method))]
      (require n)
      ((resolve method)
       {:port port
        :join? join}))
    fileset))

(deftask extend-core []
  (with-pre-wrap fileset
    (load "/core_ext")
    fileset))
