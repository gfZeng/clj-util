(ns tools.history)


(defmacro on-navigate [& body]
  `(tools.history/listen:history
    (fn [] ~@body)))
