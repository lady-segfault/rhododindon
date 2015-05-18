(ns rhododindon.pdf
  (:require [clojure.core.typed :as t]
            [rhododindon.helpers :as h]))

(defn gen-mapping
  "Creates a mapping between ids and labels"
  [data]
  (into {}
        (map (fn [k e] {k e})
             (->> (range (dec (count data)))
                  (map #(+ % 2))
                  (shuffle)
                  (cons 1))
             (map first data))))

(defn index->label
  "Given an index, returns the label given the mapping"
  [index mapping]
  (mapping index))

(defn label->index
  "Given a label, returns the index according to the mapping"
  [label mapping]
  (first (filter #(= label (mapping %)) (keys mapping))))


(defn parse-content
  "Parse the content of an element"
                                        ;TODO: use Markdown
  [elem]
  (if-let [content (:content elem)]
    content
    (do (println "Error: content not found!!! Elem:")
        (println elem))))

(defn parse-next-map
  "Parse the map containing next possible entries"
  [next mapping]
  (reduce str
          (map (fn [[k v]]
                 (format "%s -> %s\n"
                         v
                         (label->index k mapping)))
               next)))

(declare parse-rhodo)

(defn parse-if
  "Parse if special form"
  [args mapping]
  (if (= 3 (count args))
    (format "Si %s : %s\nSinon : %s\n"
            (parse-rhodo (first args) mapping)
            (parse-rhodo (second args) mapping)
            (parse-rhodo (last args) mapping))
    (h/exception "Error parsing rhodocode: if must take exactly three arguments, not this: %s " args)))
                            
(defn parse-operator
  [comparator args mapping]
  (if (not= 2 (count args))
    (h/exception "Parse error in rhodocode: operators must take exactly two arguments, %s given " (count args))
    (format "%s %s %s"
            (first args)
            (condp = comparator
              '+ "plus"
              '- "moins"
              '< "est inférieur à"
              '> "est supérieur à"
              '= "vaut exactement"
              (h/exception "Parse error in rhodocode: unknown comparator %s"
                         comparator))
            (second args))))

(defn parse-d
  "Parse the result of a dice. Quite limited since static output."
  [args mapping]
  (if (= 1 (count args))
    (format "le résultat d'un dé à %s faces" (first args))
    (h/exception "Parse error in rhodocode: d must take exactly one argument, %s given" (count args))))

(defn parse-inc-dec
  "Parse inc and dec calls"
  [call args mapping]
  (if (not= 2 (count args))
    (h/exception "Parse error in rhodocode: %s must take exactly 2 arguments, %s given"
                 (str call)
                 (count args))
    (format "%s %s %s"
            (first args)
            (condp = call
              'inc "augmente de"
              'dec "diminue de"
              (h/exception "Parse error in rhodocode: unknown call"))
            (second args))))
              


(defn parse-fncall
  "Parse fn call. Since we only generate PDF it is a bit limited"
  [elem mapping]
  (if (empty? elem)
    (h/exception "Error parsing rhodocode: empty call wtf")
    (if (= 'quote (first elem))
      (parse-rhodo (first (rest elem)) mapping)
    (let [call (first elem)
          args (map #(parse-rhodo % mapping) (rest elem))]
    (cond 
     (= call 'if) (parse-if args mapping)
     (#{'< '> '= '+ '-} call) (parse-operator call args mapping)
     (#{'inc 'dec}call) (parse-inc-dec call args mapping)
     (= 'd call) (parse-d args mapping)
     :else (str "Pas du tout encore implémenté" elem))))))
           

(defn parse-rhodo
  "Parse Rhododindon's '''code''' called rhodocode"
  [elem mapping]
  (cond
   (number? elem) (str elem)
   (string? elem) elem
   (seq? elem) (parse-fncall elem mapping)
   (symbol? elem) (str elem) ;TODO: really parse
   (map? elem) (parse-next-map elem mapping)
   :else (h/exception "Parse error: unrecognized rhodocode %s"
                      elem)))
            
  

(defn parse-next
  "Parse the next part of an element"
  [elem mapping]
  (let [next (:next elem)]
    (cond
     (nil? next) "THE END"
     :else (parse-rhodo next mapping))))
     
(defn parse-actions
 "Parse the actions associated to an element"
 [elem mapping]
 (if-let [actions (:actions elem)]
   (reduce str ""
           (for [action actions]
             (str (parse-rhodo action mapping)
                  "\n")))
   ""))
  
(defn parse-elem
  "Parse the xth element of the gamebook data and returns a string"
  [x data mapping]
  (let [elem (data (index->label x mapping))]
    (format "### %d ###\n%s\n\n%s%s\n\n"
            x
            (parse-content elem)
            (parse-actions elem mapping)
            (parse-next elem mapping))))
           

  
(defn parse-map
  "Parse a map containing the gamebook data, into a string"
  [data]
  (let [mapping (gen-mapping data)
        n (count mapping)]
    (loop [x 1
           s ""]
      (if (> x n)
        s
        (recur (inc x)
               (str s
                    (parse-elem x
                                data
                                mapping)))))))
           
    

