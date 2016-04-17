(ns tools.compojure
  (:require [tools.util :as util :refer (name-with-attributes)]
            [tools.def :refer (defmacro:def)]
            [ring.util.response :as resp]))


(defonce middlewares (atom {}))

(defmacro clear-handle
  ([anchor]
   `(swap! middlewares dissoc-in [~(.toString *ns*) ~anchor]))
  ([anchor func]
   `(swap! middlewares update-in [~(.toString *ns*) ~anchor]
           util/drop-val ~func))
  ([anchor by-key key]
   `(swap! middlewares update-in [~(.toString *ns*) ~anchor]
           #(remove (comp #{~key} :key meta) %))))

(defmacro handle [anchor handler & {:as opts}]
  `(do (clear-handle ~anchor :by-key ~(:key (meta handler)))
       (swap! middlewares update-in [~(.toString *ns*) ~anchor]
              conj (vary-meta ~handler assoc :scope '~opts))))

(defn middlewares-for [ns anchor fname]
  (->> (get-in @middlewares [ns anchor])
       (remove #(-> % meta :scope :exclude (get fname)))
       (filter #(-> % meta :scope (:only #{fname}) fname))))


(defn- params-destruct [keys params]
  (loop [bindings         []
         [x y z :as keys] keys]
    (cond
      (nil? x) bindings
      (= :as x)
        (recur (conj bindings y params)
               (nnext keys))
      (= :<< y)
        (recur (conj bindings x `(~z (~(keyword x) ~params)))
               (drop 3 keys))
      (map? x)
        (recur (conj bindings x `(~(keyword (:as x)) ~params))
               (next keys))
      :else (recur (conj bindings x `(~(keyword x) ~params))
                   (next keys)))))

(defmacro:def defhandler* [fname args & fdecl]
  (let [fdecl (if-let [pre-chckers (:pre (first fdecl))]
                (list `(cond
                         ~@pre-chckers
                         :else (do ~@(rest fdecl))))
                fdecl)]
    `(defn ~fname [~'*req*]
       (let [~'*params* (:params ~'*req*)
             ~@(params-destruct args '*params*)]
         ~@fdecl))))


(defmacro defhandler [fname & fdecl]
  (let [[fname [args & fdecl]] (name-with-attributes fname fdecl)
        fdecl (if-let [pre-chckers (:pre (first fdecl))]
                `(cond
                   ~@pre-chckers
                   :else (do ~@(rest fdecl)))
                `(do ~@fdecl))]
    `(defn ~fname [~'*req*]
       (doseq [h# (middlewares-for ~(.toString *ns*) :pre '~fname)]
         (h# ~'*req*))
       (let [~'*params* (:params
                         ((apply comp (middlewares-for ~(.toString *ns*) :prepare '~fname))
                          ~'*req*))
             ~@(params-destruct args '*params*)
             ret# ~fdecl]
         (doseq [h# (middlewares-for ~(.toString *ns*) :post '~fname)]
           (h# ~'*req*))
         ((apply comp (middlewares-for ~(.toString *ns*) :transform '~fname)) ret#)))))

(defmacro defapi [& fdecl]
  `(doto (defhandler ~@fdecl)
     (alter-var-root
      (fn [f#]
        (fn [req#]
          (let [res# (f# req#)]
            (if-not (coll? res#)
              res#
              (cond
                (::body (meta res#))      (resp/response res#)
                (sequential? res#)        {:body res#}
                (some (partial contains? res#)
                      [:headers :status]) res#
                :else                     (resp/response res#)))))))))

(defmacro dispatch [handler & {:as bindings}]
  `(~handler (update ~'*req* :params merge ~bindings)))
