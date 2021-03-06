(ns tools.util
  #?@(:cljs [(:require-macros
              [tools.util :refer (when-let* defalias)])]) 
  (:require
   [clojure.walk :as walk]
   [clojure.string :as string]
   [clojure.data :as data]
   #?@(:cljs [[cljs.reader :as reader]
              [goog.string :as gstring :refer (format)]
              [goog.string.format]]))

  #?(:clj (:import javax.xml.bind.DatatypeConverter)))


#?(:clj
 (do
   (defmacro when-let*
     "simply implementation:
  (if-let [[sym expr & more-bindings] (seq bindings)]
    `(when-let [~sym ~expr]
       (when-let* ~more-bindings ~@body))
    `(do ~@body))

  the form:
  (when-let* [a 3 b 4] (+ a b))
  will expanded as:
  (when-let [a 3]
    (when-let [b 4]
      (+ a b)"
     [bindings & body]
     (letfn [(expand [bindings]
               (if-let [[sym expr & more-bindings] (seq bindings)]
                 `(when-let [~sym ~expr]
                    ~(expand more-bindings))
                 `(do ~@body)))]
       (expand bindings)))

   (defmacro if-let*
     ([bindings then]
      `(when-let* ~bindings ~then))
     ([bindings then else]
      (letfn [(expand [bindings]
                (if-let [[sym expr & more-bindings] (seq bindings)]
                  `(if-let [~sym ~expr]
                     ~(expand more-bindings)
                     ~else)
                  then))]
        (expand bindings))))

   (comment
     (defmacro if-let*
       ([bindings then]
        `(when-let* ~bindings ~then))
       ([bindings then else]
        (let [leted (vec (take-nth 2 bindings))
              vals (take-nth 2 (rest bindings))
              syms (vec (repeatedly (count leted) gensym))]
          `(if-let [~leted (when-let* [~@(interleave syms vals)] ~syms)]
             ~then
             ~else)))))


   (defmacro if-binding
     ([binding then]
      `(if-binding ~binding ~then nil))
     ([binding then else]
      `(binding ~binding
         (if ~(first binding)
           ~then
           ~else))))

   (defmacro when-binding*
     [bindings then]
     `(binding ~bindings
        (when (and ~@(map first bindings))
          ~then)))

   (defmacro let-cond
     [& clauses]
     (when clauses
       (list (if (vector? (first clauses))
               'if-let
               'if) (first clauses)
             (if (next clauses)
               (second clauses)
               (throw (IllegalArgumentException.
                       "cond requires an even number of forms")))
             (cons `cond* (next (next clauses))))))

   (defmacro doto->>
     [expr & body]
     `(let [ret# ~expr]
        (->> ret# ~@body)
        ret#))

   (defmacro doas->
     [expr sym & body]
     `(let [ret# ~expr]
        (as-> ret# ~sym
          ~@body)
        ret#))

   (defmacro do-let
     [binding & body]
     `(let ~binding
        ~@body
        ~(first binding)))

   (defmacro defalias
     "Defines an alias for a var, preserving metadata. Adapted from
  clojure.contrib/def.clj, Ref. http://goo.gl/xpjeH"
     ([target] `(defalias ~(symbol (name target)) ~target nil))
     ([name target] `(defalias ~name ~target nil))
     ([name target doc]
      `(let [^clojure.lang.Var v# (var ~target)]
         (alter-meta!
          (def ~name (.getRawRoot v#))
          #(merge % (dissoc (meta v#) :column :line :file :test :name)
                  (when-let [doc# ~doc] {:doc doc#})))
         (var ~name))))


   (defn name-with-attributes
     "To be used in macro definitions.
  Handles optional docstrings and attribute maps for a name to be defined
  in a list of macro arguments. If the first macro argument is a string,
  it is added as a docstring to name and removed from the macro argument
  list. If afterwards the first macro argument is a map, its entries are
  added to the name's metadata map and the map is removed from the
  macro argument list. The return value is a vector containing the name
  with its extended metadata map and the list of unprocessed macro
  arguments."
     [name macro-args]
     (let [[docstring macro-args] (if (string? (first macro-args))
                                    [(first macro-args) (next macro-args)]
                                    [nil macro-args])
           [attr macro-args]          (if (and (nil? docstring) (map? (first macro-args)))
                                        [(first macro-args) (next macro-args)]
                                        [{} macro-args])
           attr                       (if docstring
                                        (assoc attr :doc docstring)
                                        attr)
           attr                       (if (meta name)
                                        (conj (meta name) attr)
                                        attr)]
       [(with-meta name attr) macro-args]))

   (defmacro defmacro:def [define [fname & fdecl] & body]
     `(defmacro ~define [~fname ~@fdecl]
        (let [[~fname [~@fdecl]] (name-with-attributes
                                  ~fname
                                  (list* ~@(remove #{'&} fdecl)))]
          ~@body)))

   (defmacro:def defn:opts [fname args & fdecl]
     (let [opts (last args)
           fdecl (if (not-any? #{'&} args)
                   fdecl
                   [`(let [~opts (if (map? (first ~opts))
                                   (first ~opts)
                                   (apply array-map ~opts))]
                       ~@fdecl)])]
       `(defn ~fname ~args
          ~@fdecl)))
   ))


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

(defn call [f x]
  (f x))

(defn mk-naming-style:x->y [f]
  (fn [x]
    (condp call x
      string? (f x)
      keyword? (-> x name f keyword)
      symbol? (-> x name f symbol)
      x)))

(def underscore->dash
  (mk-naming-style:x->y #(string/replace % #"_" "-")))

(def dash->underscore
  (mk-naming-style:x->y #(string/replace % #"-" "_")))

(def camelcasize
  (mk-naming-style:x->y (fn [x]
               (string/replace x #"[_-](\w)"
                               #(.toUpperCase (%1 1))))))

(def dashize
  (mk-naming-style:x->y
   (fn [x]
     (string/replace x #"_|[A-Z]"
                     #(str "-"
                           (when-not (= "_" %)
                             (string/lower-case %)))))))

(def underscorize
  (mk-naming-style:x->y
   (fn [x]
     (string/replace x #"-|[A-Z]"
                     #(str "_"
                           (when-not (= "-" %)
                             (string/lower-case %)))))))


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

(defn keys<< [key-step & ks]
  (let [key-step (if (vector? key-step)
                   key-step [key-step])]
    (apply conj key-step ks)))

(defn unquote-form
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

(defn if-pred [pred? v]
  (when (pred? v) v))

(def if-seq (partial if-pred seq?))

;;; ClojureScript
;;; =============

#?(:cljs
   (do
     (defn clear-timeout [x]
       (js/clearTimeout x))

     (defn clear-interval [x]
       (js/clearInterval x))

     (defn pr-money [x]
       (str "￥" (format "%.2f" (or x 0))))
     (defn q [selector]
       (js/document.querySelector (name selector)))
     (defn q* [selector]
       (js/document.querySelectorAll (name selector)))
     (defn data
       ([node] (.-dataset node))
       ([node dname] (-> (data node)
                         (aget (camelcasize (name dname))))))

     (defn refresh-page []
       (js/window.location.reload))



     (defn fetch* [settings]
       (js/fetch (:url settings)
                 (clj->js (dissoc settings :url))))

     (let [format-fns
           {:edn  {:body>   pr-str
                   :body<   reader/read-string
                   :headers {"Content-Type" "application/edn; charset=UTF-8"}}
            :json {:body>   (comp js/JSON.stringify clj->js)
                   :body<   #(-> % js/JSON.parse
                                 (js->clj :keywordize-keys true))
                   :headers {"Content-Type" "application/json; charset=UTF-8"}}}]
       (defn fetch [{:as   settings
                     :keys [format body> body<]
                     :or   {format :edn}}]
         (let [body<   (or body< (-> format-fns format :body<))
               body>   (or body> (-> format-fns format :body>))
               headers (merge (-> format-fns format :headers)
                              (:headers settings))]
           (-> (fetch* (cond-> settings
                         true             (dissoc :format :body< :body>)
                         true             (assoc :headers headers)
                         (:body settings) (update :body body>)))
               (.then #(.text %))
               (.then body<)))))
     (def ^:dynamic *form-opts* nil)

     (defn form-flush-in
       ([form-opts]
        (run! #(form-flush-in form-opts %) (:>fields form-opts)))
       ([form-opts f]
        (swap! (:for form-opts) assoc-in (:for f)
               ((or (:<< f) identity)
                (-> (:id f)
                    js/document.getElementById
                    .-value)))))

     (let [counter  (atom -1)
           field-id (fn []
                      (str "field-" (swap! counter inc)))]
       (defn field
         ([opts]
          (field opts nil))
         ([opts attrs]
          (field opts attrs nil))
         ([opts attrs option-values]
          (let [opts (if (map? opts) opts {:for opts})]
            (let [data           (:for @*form-opts*)
                  auto-flush-in? (:auto-flush-in? opts (:auto-flush-in? @*form-opts*))
                  value          (get-in @data (:for opts))
                  attrs          (-> attrs
                                     (assoc
                                      :id  (or (:id attrs) (field-id))
                                      :on-change
                                      (fn [e]
                                        (when auto-flush-in?
                                          (swap! data assoc-in (:for opts)
                                                 ((or (:<< opts)
                                                      identity)
                                                  (.. e -target -value))))
                                        (when-let [f (:on-change attrs)]
                                          (f e)))))]
              (swap! *form-opts* update :>fields conj
                     (assoc opts :id (:id attrs)))
              (if option-values
                [:select attrs
                 (for [[v repr] option-values]
                   [:option {:value v
                             :selected (when (= value v)
                                         "selected")}
                    repr])]
                [:input (assoc attrs :value value)]))))))
     )
   )


;;; Clojure
;;; =======

#?(:clj
   (load "util_clj"))
