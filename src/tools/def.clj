(ns tools.def)

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

(defmacro cond*
  [& clauses]
  (when clauses
    (list (if (:let (meta (first clauses)))
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
