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

(defn sum-up
  [s]
  (loop [s s total 0]
    (if (empty? s)
      total
      (recur (rest s) (+ (first s) total)))))

(defn flip-order
  [f]
  (fn [& args]
    (apply f (reverse args))))

(defn dispatch-rotate
  [_ times]
  (cond
    (> times 0) :counter-clock-wise
    (< times 0) :clock-wise))

(defmulti rotation dispatch-rotate)

(defmethod rotation :counter-clock-wise
  [s times]
  (loop [s s times times]
    (if (= 0 times)
      s
      (recur
       (conj (vec (rest s)) (first s))
       (dec times)))))

(defmethod rotation :clock-wise
  [s times]
  (loop [s s times times]
    (if (= 0 times)
      s
      (recur
       (cons (last s) (butlast s))
       (inc times)))))

(defn reverse-interleave
  [items num-chunks]
  ;; Start with num-chunks empty vectors, which will hold the items later
  (loop [items items
         chunks (repeat num-chunks [])]
    (if (empty? items)
      chunks
      ;; split-at lets us pick off the items which need to be pushed into chunks
      (let [[first-n rest-n] (split-at num-chunks items)]
        (recur
         rest-n
         ;; map-indexed will map over a collection with index
         ;; we are basically taking the nth item from first-n
         ;; and pushing it to the nth chunk
         (map-indexed
          (fn [index item]
            (let [to-conj (nth first-n index nil)]
              (conj item to-conj))) chunks))))))

(defn split-by-type
  [items]
  (loop [items items type-map {}]
    (if (empty? items)
      (vals type-map)
      (let [f (first items)
            r (rest items)
            t (type f)
            items-for-type (or (type-map t) [])]
        (recur
         r
         (assoc type-map t (conj items-for-type f)))))))

(defn count-items
  [items]
  (loop [items items freq {}]
    (if (empty? items)
      freq
      (let [f (first items)
            r (rest items)
            current-count (or (freq f) 0)]
        (recur r (assoc freq f (inc current-count)))))))

(defn remove-dups
  [items]
  (loop [items items uniques []]
    (if (empty? items)
      uniques
      (let [f (first items)
            r (rest items)
            f-in-uniques (some #(= f %) uniques)] ; contains? works with indexes, not what I expected!
        (if f-in-uniques
          (recur r uniques)
          (recur r (conj uniques f)))))))
