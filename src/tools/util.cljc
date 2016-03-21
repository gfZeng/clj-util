(ns tools.util
  #?@(:cljs [(:require-macros
              [tools.def :refer (when-let* defalias)])])
  #?(:clj (:require
           [tools.def :refer :all
                      :rename {name-with-attributes name-with-attributes'}]))
  (:require
   [clojure.walk :as walk]
   [clojure.string :as string]
   [clojure.data :as data]
   #?@(:cljs [[cljs.reader :as reader]
              [goog.string :as gstring :refer (format)]
              [goog.string.format]]))

  #?(:clj (:import javax.xml.bind.DatatypeConverter)))


;;; Both
;;; ====

(defn str->num
  [s]
  (let [s (.trim s)]
    (cond
      (empty? s) 0
      (re-find #"\.\d" s) (#?(:clj Double/parseDouble  :cljs js/parseFloat) s)
      :else (#?(:clj Long/parseLong :cljs js/parseInt) s))))

(defprotocol Numberable
  (as-num [_]))

(extend-protocol Numberable
  nil
  (as-num [_] 0)
  #?(:clj Number :cljs number)
  (as-num [n] n)
  #?(:clj String :cljs string)
  (as-num [s] (str->num s)))

(defn keep-precision
  [precision number]
  (let [multiplier (Math/pow 10 precision)]
    (-> number (* multiplier) Math/round (/ multiplier))))

(defn range-fix
  [x range]
  ;; (second (apply sort x range))  --- simple but inefficient
  (let [[min-num max-num] range]
    (-> x (max min-num) (min max-num))))

(defn move
  [xs from-pos to-pos]
  (let [acc (empty xs)
        x (nth xs from-pos)
        xs (concat (take from-pos xs) (drop (inc from-pos) xs))]
    (into acc (into acc (concat (take to-pos xs)
                                (conj (drop to-pos xs) x))))))

(defn drop-nth
  [xs n]
  (into (empty xs)
        (into (empty xs) (concat (take n xs) (drop (inc n) xs)))))

(defn drop-val [xs v]
  (remove #{v} xs))

(def distinct-conj (comp distinct conj))

(defn insert-nth
  [xs n x]
  (into (empty xs)
        (into (empty xs) (concat (take n xs) [x] (drop n xs)))))

(defn underscore->dash
  [v]
  (cond
    (string? v) (string/replace v #"_" "-")
    (keyword? v) (keyword (underscore->dash (name v)))
    (symbol? v) (symbol (underscore->dash (str v)))
    :else v))

(defn dash->underscore
  [v]
  (cond
    (string? v) (string/replace v #"-" "_")
    (keyword? v) (keyword (dash->underscore (name v)))
    (symbol? v) (symbol (dash->underscore (str v)))
    :else v))

(defn clojurize
  [m & [{:keys [key-fn skip-item skip-key]
         :or {key-fn underscore->dash
              skip-item #(:keep-raw (meta %))
              skip-key not}}]]
  (let [ent-fn (fn [[k v]] [(if (skip-key k) k (key-fn k)) v])
        item-fn (fn [x] (if (and (not (skip-item x))
                                 (map? x))
                          (into {} (map ent-fn x))
                          x))]
    (walk/postwalk item-fn m)))

(defn- first-by-compare [comp xs]
  (first (sort-by second comp (map-indexed #(do [%1 %2]) xs))))

(defn merge-sorted
  ([xss]
   (merge-sorted compare xss))
  ([comp xss]
   (let [xss (remove empty? xss)]
     (when (seq xss)
       (let [[pos fx] (first-by-compare comp (map first xss))]
         (lazy-seq
          (cons fx (merge-sorted comp (map-indexed #(if (= pos %1) (rest %2) %2) xss)))))))))

(defn merge-sorted-by
  ([key-fn xss]
   (merge-sorted-by key-fn compare xss))
  ([key-fn comp xss]
   (merge-sorted #(. comp (compare (key-fn %1) (key-fn %2))) xss)))

(defn distinct-by [key-fn xs]
  (map first (vals (group-by key-fn xs))))

(defn random-string
  [n]
  (let [chars (map char (concat (range (int \0) (inc (int \9)))
                                (range (int \A) (inc (int \Z)))
                                (range (int \a) (inc (int \z)))))]
    (apply str (repeatedly n #(rand-nth chars)))))

(defn not-blank
  [s]
  (when-not (string/blank? s)
    (string/trim s)))

(defn random-numeric-string
  [n]
  (apply str (repeatedly n #(rand-nth "0123456789"))))

(defn random-alphabetic-string
  [n]
  (apply str (repeatedly n #(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))

(defmacro timef
  "Evaluates expr and prints the time it took.  Returns the value of expr."
  [fmt expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (format ~fmt (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))
     ret#))

(defn var-name
  [v]
  (when-let* [m (meta v)
              ns (:ns m)
              name (:name m)]
    (str ns \/ name)))

(defn box-name [v]
  (or (:box-name (meta v))
      (var-name v)
      (first (.split (str v) "@"))))

(defn var->sym
  [v]
  (symbol (var-name v)))

(defn take-some [pred xs]
  (some #(when (pred %) %) xs))

(defn upper-case [x]
  (cond
    (nil? x)     nil
    (symbol? x)  (symbol (upper-case (name x)))
    (keyword? x) (keyword (upper-case (name x)))
    (string? x)  (string/upper-case x)
    :else        x))

(defn lower-case [x]
  (cond
    (nil? x)     nil
    (symbol? x)  (symbol (lower-case (name x)))
    (keyword? x) (keyword (lower-case (name x)))
    (string? x)  (string/lower-case x)
    :else        x))

(defn str-before [s sr]
  (.substring s 0 (.indexOf s sr)))

(defn str-after [s sl]
  (.substring s (+ (.indexOf s sl) (count sl))))

(defn str-between [s sl sr]
  (-> s (str-after sl) (str-before sr)))

(defmacro map-> [xs & body]
  `(mapv #(-> % ~@body) ~xs))

(defmacro map->> [xs & body]
  `(mapv #(->> % ~@body) ~xs))

(defmacro map-as-> [expr xs & body]
  `(mapv #(as-> % expr body) ~xs))

(defn camel-case [x]
  (cond
    (symbol? x) (symbol (camel-case (name x)))
    (keyword? x) (keyword (camel-case (name x)))
    :else (string/replace x #"[_-](\w)" #(.toUpperCase (%1 1)))))

(defn keys<< [key-step & ks]
  (let [key-step (if (vector? key-step)
                   key-step [key-step])]
    (apply conj key-step ks)))

(defn- unquote-form
  "Inside form, unquoting (~) allows for arbitrary evaluation."
  [args]
  (walk/walk (fn [item]
               (cond (and (seq? item) (= `unquote (first item))) (second item)
                 ;; needed if we want fn literals preserved
                 (or (seq? item) (symbol? item)) (list 'quote item)
                 :else (let [result (unquote-form item)]
                         ;; clojure.walk strips metadata
                         (if-let [m (meta item)]
                           (with-meta result m)
                           result))))
             identity
             args))


;;; ClojureScript
;;; =============

#?(:cljs
   (do
     (defn eval-with-env [env js-code]
       (let [js-code (reduce #(string/replace
                               % (re-pattern %2) (str "this['" %2 "']"))
                             js-code
                             (map name (keys env)))]
         (try
           (.call (fn [] (js/eval js-code)) (clj->js env))
           (catch js/Error e))))
     (defn pr-money [x]
       (str "￥" (format "%.2f" (or x 0))))
     (defn q [selector]
       (js/document.querySelector (name selector)))
     (defn q* [selector]
       (js/document.querySelectorAll (name selector)))
     (defn data
       ([node] (.-dataset node))
       ([node dname] (-> (data node)
                         (aget (camel-case (name dname))))))

     (defn refresh-page []
       (js/window.location.reload))

     (let [format-fns
           {:edn  {:body> pr-str
                   :body< reader/read-string
                   :headers {"Content-Type" "application/edn; charset=UTF-8"}}
            :json {:body> (comp js/JSON.stringify clj->js)
                   :body< #(-> % js/JSON.parse
                               (js->clj :keywordize-keys true))
                   :headers {"Content-Type" "application/json; charset=UTF-8"}}}]
       (defn fetch [{:as settings
                     :keys [url format body> body<]
                     :or {format :edn}}]
         (let [body< (or body< (-> format-fns format :body<))
               body> (or body> (-> format-fns format :body>))
               headers (merge (-> format-fns format :headers)
                              (:headers settings))]
           (-> (js/fetch url (cljs.core/clj->js
                              (cond-> settings
                                true (dissoc :url :format :body< :body>)
                                true (assoc :headers headers)
                                (:body settings) (update :body body>))))
               (.then #(.text %))
               (.then #(js/Promise.
                        (fn [rs rj]
                          (rs (body< %)))))))))
     (def ^:dynamic *form-opts* nil)

     (defn form-flush-in
       ([form-opts]
        (run! #(form-flush-in form-opts %) (:>fields form-opts)))
       ([form-opts f]
        (swap! (:for form-opts) assoc-in (:for f)
               ((or (:< f) identity)
                (-> (:id f)
                    js/document.getElementById
                    .-value)))))

     (let [counter (atom -1)
           field-id (fn []
                      (str "field-" (swap! counter inc)))]
       (defn field
         ([opts]
          (field opts nil))
         ([opts attrs]
          (field opts attrs nil))
         ([opts attrs option-values]
          (let [opts (if (map? opts) opts {:for opts})]
            (let [data (:for @*form-opts*)
                  auto-flush-in? (:auto-flush-in? opts (:auto-flush-in? @*form-opts*))
                  attrs (-> attrs
                            (assoc
                             :id  (or (:id attrs) (field-id))
                             :value (get-in @data (:for opts))
                             :on-change
                             (fn [e]
                               (when auto-flush-in?
                                 (swap! data assoc-in (:for opts)
                                        ((or (:< opts)
                                             identity)
                                         (.. e -target -value))))
                               (when-let [f (:on-change attrs)]
                                 (f e)))))
                  attrs (cond-> attrs
                          option-values (dissoc :value))]
              (swap! *form-opts* update :>fields conj
                     (assoc opts :id (:id attrs)))
              (if option-values
                [:select attrs
                 (for [[v repr] option-values]
                   [:option {:value v} repr])]
                [:input attrs]))))))
     )
   )


;;; Clojure
;;; =======

#?(:clj
   (load "util_clj"))
