(in-ns 'tools.util)

(import 'org.joda.time.format.DateTimeFormatter)

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


(defn set-default-time-zone
  [x]
  (if (number? x)
    (set-default-time-zone (t/time-zone-for-offset x))
    (org.joda.time.DateTimeZone/setDefault x)))

(defn as-joda-time
  ([x] (as-joda-time x (t/default-time-zone)))
  ([x time-zone]
   (let [time-zone (if (number? time-zone)
                     (t/time-zone-for-offset time-zone)
                     time-zone)]
     (t/to-time-zone (tc/to-date-time x) time-zone))))

(def as-time-formatter
  (memoize
   (fn [formatter]
     (if (instance? DateTimeFormatter formatter)
       formatter
       (tf/formatter formatter (t/time-zone-for-offset +8))))))

(defn now
  []
  (as-joda-time (t/now)))

(defn time-parse
  ([dt-str]      (as-joda-time dt-str))
  ([fmt dt-str] (tf/parse (as-time-formatter fmt) dt-str)))

(defn time-unparse
  ([dt]      (time-unparse "yyyy-MM-dd HH:mm:ss" dt))
  ([fmt dt] (tf/unparse (as-time-formatter fmt) dt)))

(defn the-hour
  ([]   (the-hour (now)))
  ([dt] (time-parse "yyyy-MM-dd HH" (time-unparse "yyyy-MM-dd HH" dt))))

(defn the-month
  ([] (the-month (now)))
  ([dt] (time-parse "yyyy-MM" (time-unparse "yyyy-MM" dt))))

(defn the-day
  ([]   (the-day (now)))
  ([dt] (time-parse "yyyy-MM-dd" (time-unparse "yyyy-MM-dd" dt))))

(defn today
  []
  (the-day))

(defn the-week
  ([]   (the-week (today)))
  ([dt] (time-parse "yyyy; w" (time-unparse "yyyy; w" dt))))

(defn hour-seq
  ([n] (take n (hour-seq)))
  ([]  (iterate #(t/minus % (-> 1 t/hours)) (the-hour))))

(defn day-seq
  ([n] (take n (day-seq)))
  ([]  (iterate #(t/minus % (-> 1 t/days)) (today))))

(defn week-seq
  ([n] (take n (week-seq)))
  ([]  (iterate #(t/minus % (-> 1 t/weeks)) (the-week))))

(defn month-seq
  ([n] (take n (month-seq)))
  ([]  (iterate #(t/minus % (-> 1 t/months)) (the-month))))

(defn period-seq
  ([type n] (take n (period-seq type)))
  ([type]   (condp = type
              :hour  (hour-seq)
              :day   (day-seq)
              :month (month-seq)
              :week  (week-seq)
              (throw (ex-info "error type of period" {:type type})))))

(defalias name-with-attributes name-with-attributes')

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
  (if (some-> (last body) first
              (= '.catch))
    [(butlast body) (let [[_ err & err-body] (last body)]
                      `(.catch (fn [~err] ~@err-body)))]
    [body `identity]))

(defmacro let-promise [bindings & body]
  (let [[body catch] (body-syntax-parse body)]
    `(->
      (js/Promise.all
       (into-array ~(vec (take-nth 2 (rest bindings)))))
      ~catch
      (.then (fn [[~@(take-nth 2 bindings)]]
               ~@body)))))

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
        exprs [(for [expr (take-nth 2 (rest bindings))]
                 `(compile-fetch expr))]]
    `(let-promise ~(vec (interleave syms exprs)))))

(defmacro let-fetch-> [bindings & body]
  (let [syms (take-nth 2 bindings)
        exprs [(for [expr (take-nth 2 (rest bindings))]
                 `(compile-fetch expr))]]
    `(let-promise-> ~(vec (interleave syms exprs)))))

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
                             (when-let [value# (get-in diff-v# (:for f#))]
                               (-> (js/document.getElementById (:id f#))
                                   .-value
                                   (set! ((or (:> f#) identity) value#))))))))
            [:form ~@fields])))
