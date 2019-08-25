(ns f4clojure.core
  (:gen-class))

;; http://www.4clojure.com/problem/19
(defn last-element-in-sequence
  [sequence] 
 (loop [f (first sequence)
         r (rest sequence)]
    (if (empty? r)
      f
      (recur (first r) (rest r)))))

;; http://www.4clojure.com/problem/20
(defn penultimate
  [elems]
  (nth (reverse elems) 1 nil))

;; http://www.4clojure.com/problem/21
(defn nth-elem
  [elems n]
  (loop [elems elems n n]
    (if (= n 0)
      (first elems)
      (recur (rest elems) (dec n)))))

;; http://www.4clojure.com/problem/22
(defn my-count
  [elems]
  (loop [elems elems counter 0]
    (if (empty? elems)
      counter
      (recur (rest elems) (inc counter)))))

;; http://www.4clojure.com/problem/23
(defn reverse-seq
  [s]
  (loop [s s rev []]
    (if (empty? s)
      rev
      (recur (rest s) (cons (first s) rev)))))

;; http://www.4clojure.com/problem/24
(defn sum-up
  [s]
  (loop [s s total 0]
    (if (empty? s)
      total
      (recur (rest s) (+ (first s) total)))))

;; http://www.4clojure.com/problem/46
(defn flip-order
  [f]
  (fn [& args]
    (apply f (reverse args))))

(defn dispatch-rotate
  [_ times]
  (cond
    (> times 0) :counter-clock-wise
    (< times 0) :clock-wise))

;; http://www.4clojure.com/problem/44
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

;; http://www.4clojure.com/problem/43
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

;; http://www.4clojure.com/problem/50
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

;; http://www.4clojure.com/problem/55
(defn count-items
  [items]
  (loop [items items freq {}]
    (if (empty? items)
      freq
      (let [f (first items)
            r (rest items)
            current-count (or (freq f) 0)]
        (recur r (assoc freq f (inc current-count)))))))

;; http://www.4clojure.com/problem/56
(defn remove-dups
  [items]
  (loop [items items uniques []]
    (if (empty? items)
      uniques
      (let [f (first items)
            f-in-uniques (some #(= f %) uniques) ; contains? works with indexes, not what I expected!
            new-uniques (if f-in-uniques uniques (conj uniques f))]
        (recur (rest items) new-uniques)))))

;; http://www.4clojure.com/problem/58
(defn composer
  [& funcs]
  (loop [composed (last funcs)
         funcs (butlast funcs)]
    (if (empty? funcs)
      composed
      (recur
       (fn [& args]
         ((last funcs) (apply composed args)))
       (butlast funcs)))))

;; http://www.4clojure.com/problem/54
(defn part-seq
  [size items]
  (loop [items items
         chunks []]
    (if (< (count items) size)
      chunks
      (let [[a b] (split-at size items)]
        (recur b (conj chunks a))))))

;; http://www.4clojure.com/problem/59
(defn juxtapose
  [& functions]
  (fn [& args]
    (map #(apply % args) functions)))

;; http://www.4clojure.com/problem/70
(defn split-sentence-and-sort
  [sentence]
  (sort-by #(.toLowerCase %) (re-seq #"[a-zA-Z0-9]+" sentence))) ; not sure if using regexes is allowed in problems


(defn prime?
  [num]
  (let [limit (inc (quot num 2))]
  (nil?
   (some #(= (rem num %) 0)
         (range 2 limit)))))

;; http://www.4clojure.com/problem/67
(defn n-primes
  [n]
  (loop [n n current 2 primes []]
    (if (= 2 n) ; because we start at 2, we should end early
      primes
      (recur (dec n) (inc current)
             (if (prime? current) (conj primes current) primes)))))

(defn is-map?
  "Tries to infer if the argument is a map by using conj with a single element
  which should fail with exception"
  [arg]
  (try
    (conj arg 0)
    false ; if conj worked, its not a map
    (catch IllegalArgumentException e true))) ; it is a map

(defn is-set?
  "Tries to infer if the arg is a set by adding the first element again
  and confirming that the size does not change"
  [arg]
  (let [f (first arg)
        size (count arg)]
    (= (count (conj arg f)) size)))

(defn is-vec?
  "Tries to infer if we have a vec by checking that conj appends
  this can also succeed for set randomly but we do the test in sequence
  inserts two random numbers so we do not fail on an empty list becoming a single elem list where last == first"
  [arg]
  (let [a (rand-int 10000)
        b (rand-int 10000)
        c (conj arg a b)]
    (= (last c) b)))

(defn is-list?
  [arg]
  (let [a (rand-int 10000)
        b (rand-int 10000)
        c (conj arg a b)]
    (= (first c) b)))

;; http://www.4clojure.com/problem/65
(defn black-box-testing
  [arg]
  (cond
    (is-map? arg) :map
    (is-set? arg) :set
    (is-vec? arg) :vec
    (is-list? arg) :list))
