(ns rhododindon.helpers)
;  #?(:cljs (:require [goog.string :as gstring]
;                     [goog.string.format])))

(comment (defn format
  "Formats a string"
  [fmt & args]
;  #?(:cljs (apply gstring/format fmt args)
  apply clojure.core/format fmt args))

(defn exception
  "Sens a formatted exception"
  [fmt & args]
  (let [s (apply format fmt args)]
    (throw
     (Exception. s))))
;        :cljs (js/Error. s)))))