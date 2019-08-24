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

(defn my-count
  [elems]
  (loop [elems elems counter 0]
    (if (empty? elems)
      counter
      (recur (rest elems) (inc counter)))))

(defn reverse-seq
  [s]
  (loop [s s rev []]
    (if (empty? s)
      rev
      (recur (rest s) (cons (first s) rev)))))
