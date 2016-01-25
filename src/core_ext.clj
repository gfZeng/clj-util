(in-ns 'clojure.core)

(defn dissoc-in [m keys]
  (update-in m (butlast keys) dissoc (last keys)))

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


(def THREAD-LOCAL-CACHE (atom #{}))

(defmacro alter! [v-sym f & args]
  `(do
     (when-not (thread-bound? (var ~v-sym))
       (push-thread-bindings {(var ~v-sym) nil}))
     (when (thread-bound? (var ~v-sym))
       (swap! THREAD-LOCAL-CACHE conj (var ~v-sym))
       (set! ~v-sym (~f ~v-sym ~@args)))))

(defn clear-thread-local! []
  (doseq [_ @THREAD-LOCAL-CACHE]
    (pop-thread-bindings))
  (reset! THREAD-LOCAL-CACHE #{}))


(defn- protocol-sym [x]
  (let [x (str x)
        x (if (.contains x "/")
            (.replace x "/" "/__")
            (str "__" x))]
    (symbol x)))

(defmacro defprotocol* [name & opts+sigs]
  (let [var-args-fns (filter (fn [sig]
                               (when (list? sig)
                                 (->> sig second (some #{'&}))))
                             opts+sigs)
        protocol-opts+sigs (map (fn [sig]
                                  (if (list? sig)
                                    (let [[name argv & doc] sig]
                                      (list* (protocol-sym name) (vec (remove #{'&} argv)) doc))
                                    sig))
                                opts+sigs)]
    `(do
       (defprotocol ~name ~@protocol-opts+sigs)
       ~@(for [[name argv] var-args-fns]
           `(defn ~name ~argv
              (~(symbol (str "__" name)) ~@(remove #{'&} argv)))))))


(defmacro defrecord* [name & opts+sigs]
  (let [opts+sigs (for [sig opts+sigs]
                    (if (and (list? sig)
                             (some #{'&} (second sig)))
                      (list* (protocol-sym (first sig))
                             (vec (remove #{'&} (second sig)))
                             (nnext sig))
                      sig))]
    `(defrecord ~name ~@opts+sigs)))

(defn rcomp [& fs]
  (apply comp (reverse fs)))

(defn rpartial [f & args]
  (fn [largs]
    (apply f (concat largs args))))

(require '[clojure.core.strint :refer (<<)])

(defn wrap-reader [rdr f]
  ^{:raw-reader rdr}
  (reify clojure.lang.IFn
    (invoke [this r ch opts pending-forms]
      (f (.invoke rdr r ch opts pending-forms)))))

(let [macros (-> (.getDeclaredField clojure.lang.LispReader "macros")
                 (doto (.setAccessible true))
                 (.get nil))
      dispatch-macros (-> (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                          (doto (.setAccessible true))
                          (.get nil))]

  (defn set-character-macro [^Character c f]
    (aset macros (int c) f))

  (defn get-character-macro [^Character c]
    (aget macros (int c)))

  (defn alter-character-macro! [^Character c wrapper & args]
    (set-character-macro
     c (apply wrapper (get-character-macro c) args)))

  (defn set-dispatch-macro [^Character c f]
    (aset dispatch-macros (int c) f)))

(set-dispatch-macro
 \<
 (let [raw-reader (clojure.lang.HereDocReader.)]
   (fn [rdr c opts pending-forms]
     (list 'clojure.core.strint/<<
           (.invoke raw-reader rdr c opts pending-forms)))))

;; (alter-charactor-macro! \" rcomp #(list 'clojure.core.strint/<< %))
;; do not use this extention
;; "(+ 42 1) = ~(+ 42 1)" eval=> "(+ 42 1) = 43", this is really cool
;; but..., you know.

#<JAVACODE
package clojure.lang;

import java.io.Reader;

/**
 * Created by isaac on 12/4/15.
 */
public class HereDocReader extends AFn {

    @Override
    public Object invoke(Object reader, Object charactor, Object opts, Object pendingForms) {
        Reader r = (Reader) reader;
        StringBuilder sb = new StringBuilder();

        char[] cache = null;
        int cacheLen = 0;
        for (int ch, cachedLen = 0; (ch = LispReader.read1(r)) != -1; ) {
            char c = (char) ch;
            if (cache == null) {
                sb.append(c);
                if (Character.isWhitespace(c)) {
                    cache = new char[cacheLen];
                } else {
                    cacheLen++;
                }
                continue;
            }

            if (c == sb.charAt(cachedLen)) {
                cache[cachedLen] = c;
                cachedLen++;
                if (cachedLen == cacheLen) { break; }
                continue;
            }

            sb.append(cache, 0, cachedLen);
            cachedLen = 0;
            sb.append(c);
        }

        return sb.delete(0, cacheLen).toString();
    }
}
JAVACODE
