(ns tools.history)


(defmacro on-navigate [& body]
  `(tools.history/listen:navigate
    (fn [] ~@body)))
