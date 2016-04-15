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

(def thread-local-utc-date-format
  (memoize
   (fn thisfn
     ([] (thisfn "GMT"))
     ([time-zone]
      (proxy ThreadLocal []
        (initialValue []
          (let [time-zone (java.util.TimeZone/getTimeZone time-zone)
                time-zone-id (-> (subs (.getID time-zone) 3)
                                 not-empty
                                 (or "-00:00"))]
            (doto (java.text.SimpleDateFormat.
                   (str "yyyy-MM-dd'T'HH:mm:ss.SSS" time-zone-id))
              ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
              (.setTimeZone time-zone)))))))))

(defn pr-utc
  ([^java.util.Date d]
   (pr-utc d "GMT"))
  ([^java.util.Date d time-zone]
   (-> time-zone
       thread-local-utc-date-format
       .get
       (.format d))))

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

(defn parse-fetch-body [body]
  (let [[body finally] (if (some->> (last body)
                                    (if-pred seq?)
                                    first
                                    (= '.finally))
                         [(butlast body) (let [[_ & err-body] (last body)]
                                           `(.finally (fn [] ~@err-body)))]
                         [body nil])
        [body catch]   (if (some->> (last body)
                                    (if-pred seq?)
                                    first
                                    (= '.catch))
                         [(butlast body) (let [[_ err & err-body] (last body)]
                                           `(.catch (fn [~err] ~@err-body)))]
                         [body nil])]
    [body (when catch [catch]) (when finally [finally])]))

(defmacro let-promise [bindings & body]
  (let [[body catch finally] (parse-fetch-body body)
        [pre-checker body]   (let [[f & r :as body] body]
                               (cond
                                 (not r) [nil body]
                                 (:pre f) [(:pre f) r]
                                 :else [nil body]))]
    `(->
      (js/Promise.all
       (into-array ~(vec (take-nth 2 (rest bindings)))))
      (.then (fn [[~@(take-nth 2 bindings)]]
               (cond
                 ~@pre-checker
                 :else (do ~@body))))
      ~@catch
      ~@finally)))

(defmacro let-promise-> [bindings & body]
  (let [[body catch finally] (parse-fetch-body body)
        expand (fn expand [[sym expr & rest-bindings]]
                 `(-> ~expr
                      (.then (fn [~sym]
                               ~@(if rest-bindings
                                   [(expand rest-bindings)]
                                   body)))
                      ~@catch
                      ~@finally))]
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

(defmacro timeout [delay & body]
  `(let [timeout# (atom nil)]
     (reset! timeout#
             (js/setTimeout
              (fn []
                (let [~'*timeout* @timeout#]
                  ~@body))
              ~delay))))

(defmacro interval [delay & body]
  `(let [interval# (atom nil)]
     (reset! interval#
             (js/setInterval
              (fn []
                (let [~'*interval* @interval#]
                  ~@body))
              ~delay))))

(defmacro def-timeout-fn [name delay args & body]
  `(let [timeout# (atom nil)]
     (defn ~name ~args
       (when-let [t# @timeout#]
         (js/clearTimeout t#))
       (reset! timeout# (js/setTimeout (fn [] ~@body) ~delay)))))
