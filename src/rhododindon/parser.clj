(ns rhododindon.parser
  (:require [rhododindon.helpers :as h]
            [clojure.string :refer [trim split-lines split]]))

(declare parse)

(defn rhodocode?
  "Checks for beginning of rhodocode"
  [{[head & _] :remaining}]
  (.contains head "@("))

(defn insert-exp
  "Insert a rhodocode expression at the appropriate place"
  [{cursor :cursor :as context} exp]
  (let [exp (read-string exp)]
    (cond
     (empty? cursor) (assoc-in context [:init]
                               (conj (context :init) exp))
     (= 2 (count cursor)) (let [cursor (conj cursor :actions)
                                actions (get-in context cursor)
                                actions (if (nil? actions)
                                          []
                                          actions)]
                            (assoc-in context cursor
                                      (conj actions exp)))
     :else (assoc-in context cursor exp))))


(defn parse-rhodocode
  "Parse a rhodocode"
  [{[head & tail :as lines] :remaining
    :as context}]
  (let [head (trim head)
        i (.indexOf head "@(")]
    (if (not= i 0)
      (let [new-head (subs head 0 i)
            old-head (subs head i)]
        (assoc context :remaining
               (into [new-head old-head] tail)))
      (do (println head)
          (println (.endsWith head ")@"))
          (loop [head head
                 tail tail]
            (if (empty? head)
              (if (empty? tail)
                (throw (Exception. "Parse error of rhododown: mismatched '@)'"))
                (recur (first tail) (rest tail)))
              (let [head (trim head)]
                (cond
                 (.endsWith head ")@") (let [exp (subs head 1 (dec (count head)))]
                                          (-> context
                                             (assoc :remaining tail)
                                             (insert-exp exp)))
                 (.contains head ")@") (let [i (.lastIndexOf head ")@")
                                             old-head (subs head (+ i 2))
                                             head (subs head 1 (inc i))]
                                         (-> context
                                             (assoc :remaining
                                               (into [old-head] tail))
                                             (insert-exp head)))
                 :else (if (empty? tail)
                         (recur nil nil)
                         (recur (str head " " (first tail))
                                (rest tail)))))))))))

                  
(defn next?
  "Checks if line is an indication to next element"
  [{[head & _] :remaining}]
  (.startsWith (trim head) "*"))

(defn parse-next
  "Parse indication to next element"
  [{[head & tail] :remaining
    cursor :cursor
    :as context}]
  (if (or (not= 2 (count cursor))
          (not= (first cursor) :data))
    (throw (Exception. "Parse error: next reference not in data element"))
    (loop [head head
           [next & tail :as full-tail] tail]
      (let [head (trim head)]
        (if (or (empty? next)
                (.startsWith (trim next) "*"))
          (let [[label content] (split (subs head 1) #":" 2)]
            (if (or (empty? label)
                    (empty? content))
              (let [context (assoc context :remaining (into [label] full-tail))]
                (if (rhodocode? context)
                  (-> context
                      (assoc :cursor (conj cursor :next))
                      (parse-rhodocode)
                      (assoc :cursor cursor))
                  (throw (Exception. (format "%s ;\n%s" "Parse error in rhododown: malformed next element, must be 'label: content' or rhodocode"
                                             head)))))
              (let [label (trim label)
                    content (trim content)
                    cursor (conj cursor :next label)]
              (-> context
                  (assoc :remaining full-tail)
                  (assoc-in cursor content)))))
          (recur (str head " " next) tail))))))
                 
      

(defn section-title?
  "Checks for section title"
  [{[_ next & _] :remaining}]
  (if next
    (.startsWith next "----")
    false))

(defn parse-section-title
  "Parse a section title"
  [{[head _ & tail] :remaining
    :as context}]
  (let [label (clojure.string/trim head)]
    (-> context
        (assoc-in [:data label] {})
        (assoc :cursor [:data label])
        (assoc :remaining tail))))

(defn big-title?
  "Checks for main title"
  [{[_ next & tail] :remaining}]
  (if next
    (if (string? next)
      (.startsWith next "====")
      (println tail))
    false))
    
(defn parse-big-title
  "Parse the main title"
  [{[head _ & tail] :remaining
    :as context}]
  (-> context
      (assoc :title (clojure.string/trim head))
      (assoc :remaining tail)))

(defn parse-content
  "Parse what is supposed to be a line of text"
  [{[head & tail] :remaining
    cursor :cursor
    :as context}]
  (if (empty? head)
    (assoc context :remaining tail)
    (let [cursor (if (empty? cursor)
                   [:intro]
                   (if (and (= 2 (count cursor))
                            (= :data (first cursor))
                            (string? (second cursor)))
                     (conj cursor :content)
                     (h/exception "Parse error in rhododown: wrong cursor to insert content")))
          s (get-in context cursor)]
      (-> context
          (assoc-in cursor (str s (trim head) "\n\n"))
          (assoc :remaining tail)))))
        
  

(defn parse
  "The function that is called recursively"
  [{lines :remaining
    :as context}]
  (cond
   (empty? lines) context ;(dissoc context :remaining :cursor)
   (big-title? context) (recur (parse-big-title context))
   (section-title? context) (recur (parse-section-title context))
   (rhodocode? context) (recur (parse-rhodocode context))
   (next? context) (recur (parse-next context))
   :else (recur (parse-content context))))
                                        ;(h/exception "Parse error loading rhododown file")))
     

  
(defn parse-rhododown
  "Parse a mix of markdown and rhodocode and returns (hopefully)
   some internal data structure"
  [s]
  (let [lines (clojure.string/split-lines s)]
    (parse {:remaining lines
            :cursor nil
            :init []
            :data {}})))
    