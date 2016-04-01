(in-ns 'tools.util)

(defmacro ^:private compile-if
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`.
  (compile-if (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (do-cool-stuff-with-fork-join)
    (fall-back-to-executor-services))"
  ([exp then]
   `(compile-if ~exp ~then nil))
  ([exp then else]
   (if (try (eval exp)
            (catch Throwable _ false))
     then
     else)))


(defn base64-encode
  [x & {:keys [encoding]
        :or {encoding "UTF-8"}}]
  (if (string? x)
    (base64-encode (.getBytes x encoding))
    (DatatypeConverter/printBase64Binary x)))

(defn base64-decode
  [x & {:keys [encoding]
        :or {encoding "UTF-8"}}]
  (String. (DatatypeConverter/parseBase64Binary x) encoding))

(defn urlsafe-b64encode
  [x & args]
  (-> (apply base64-encode x args)
      (string/replace "+" "-")
      (string/replace "/" "_")))

(defn hmac-sha1-bytes
  [key input]
  ;; https://gist.github.com/visibletrap/4571244#file-hmac-sha1-clj
  (let [secret (javax.crypto.spec.SecretKeySpec. (. key getBytes "UTF-8") "HmacSHA1")
        hmac-sha1 (doto (javax.crypto.Mac/getInstance "HmacSHA1") (.init secret))
        bytes (. hmac-sha1 doFinal (. input getBytes "UTF-8"))]
    bytes))

(defn hmac-sha1
  [key input]
  (let [bytes (hmac-sha1-bytes key input)]
    (apply str (map (partial format "%02x") bytes))))

(defalias name-with-attributes name-with-attributes')

(compile-if (require 'clj-time.core)
            (load "util_time"))

;;;; define for cljs
(defmacro on-ready [& body]
  `(js/$ (fn [] ~@body)))

(defmacro eval-with [window bindings & body]
  `(do
     (set! (.-export js/window) [~@(vals bindings)])
     (.call (.eval ~window
                   (str "(" (fn [w#]
                              (let [[~@(keys bindings)] (.-export w#)]
                                ~@body)) ")"))
            ~window
            js/window)))

(defmacro when-$ [[sym sel] & body]
  `(let [~sym (js/$ ~sel)]
     (when (aget ~sym 0)
       ~@body)))

(defn body-syntax-parse [body]
  (let [[body finally] (if (some-> (last body) first
                                   (= '.finally))
                         [(butlast body) (let [[_ err-body] (last body)]
                                           `(.finally (fn [] ~@err-body)))]
                         [body nil])
        [body catch]   (if (some-> (last body) first
                                   (= '.catch))
                         [(butlast body) (let [[_ err & err-body] (last body)]
                                           `(.catch (fn ['~err] ~@err-body)))]
                         [body nil])]
    [body (when catch [catch]) (when finally [finally])]))

(defmacro let-promise [bindings & body]
  (let [[body catch] (body-syntax-parse body)]
    `(->
      (js/Promise.all
       (into-array ~(vec (take-nth 2 (rest bindings)))))
      (.then (fn [[~@(take-nth 2 bindings)]]
               ~@body))
      ~@catch
      ~@finally)))

(defmacro let-promise-> [bindings & body]
  (let [[body catch] (body-syntax-parse body)
        expand (fn expand [[sym expr & rest-bindings]]
                 `(-> ~expr
                      ~catch
                      (.then (fn [~sym]
                               ~@(if rest-bindings
                                   [(expand rest-bindings)]
                                   body)))))]
    (expand bindings)))

(defmacro let-fetch [bindings & body]
  (let [syms (take-nth 2 bindings)
        exprs (for [expr (take-nth 2 (rest bindings))]
                `(fetch ~expr))]
    `(let-promise ~(vec (interleave syms exprs))
       ~@body)))

(defmacro let-fetch-> [bindings & body]
  (let [syms (take-nth 2 bindings)
        exprs (for [expr (take-nth 2 (rest bindings))]
                `(fetch ~expr))]
    `(let-promise-> ~(vec (interleave syms exprs))
       ~@body)))

(defmacro form [opts & fields]
       `(binding [*form-opts* (atom
                               (let [opts# ~opts]
                                 (if (instance? ~'cljs.core/Atom opts#)
                                   {:for opts#}
                                   opts#)))]
          (let [form-opts# *form-opts*
                ~'*form-data* (:for @form-opts#)
                ~'*flush-in* (fn
                               ([] (form-flush-in @form-opts#))
                               ([f#] (form-flush-in @form-opts# f#)))]
            (add-watch ~'*form-data* :update-fields-values
                       (fn [r# k# ov# nv#]
                         (let [diff-v# (first (data/diff nv# ov#))]
                           (doseq [f# (:>fields @form-opts#)]
                             (when-let* [value# (get-in diff-v# (:for f#))
                                         ele# (js/document.getElementById (:id f#))]
                               (-> ele# .-value
                                   (set! ((or (:>> f#) identity) value#))))))))
            [:form ~@fields])))
