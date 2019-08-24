(ns f4clojure.core
  (:gen-class))

(defn last-element-in-sequence
  [sequence] 
 (loop [f (first sequence)
         r (rest sequence)]
    (if (empty? r)
      f
      (recur (first r) (rest r)))))

(defn penultimate
  [elems]
  (nth (reverse elems) 1 nil))

(defn nth-elem
  [elems n]
  (loop [elems elems n n]
    (if (= n 0)
      (first elems)
      (recur (rest elems) (dec n)))))
