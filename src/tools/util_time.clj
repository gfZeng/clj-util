(in-ns 'tools.util)

(require '[clj-time.core :as t]
         '[clj-time.coerce :as tc]
         '[clj-time.format :as tf])

(import 'org.joda.time.format.DateTimeFormatter)


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
