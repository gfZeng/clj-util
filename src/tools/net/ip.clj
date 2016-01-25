(ns tools.net.ip
  (:import [java.net Inet4Address NetworkInterface]))

(defn local-ip?
  [addr]
  (if (string? addr)
    (local-ip? (Inet4Address/getByName addr))
    (NetworkInterface/getByInetAddress addr)))
