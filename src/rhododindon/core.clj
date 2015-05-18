(ns rhododindon.core
  (:require [rhododindon.pdf :as pdf]
            [rhododindon.helpers :as h])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (not= 1 (count args))
    (println (format "Please give a file name as argument (%s given)"
                       (count args)))
    (with-open [r (java.io.PushbackReader. (clojure.java.io/reader (first args)))]
      (-> (read r)
          eval
          pdf/parse-map
          println))))
