(ns f4clojure.core
  (:require [clojure.string :as s])
  (:gen-class))

;; http://www.4clojure.com/problem/19
(defn last-element-in-sequence
  [sequence] 
  (reduce #(identity %2) nil sequence))

;; http://www.4clojure.com/problem/20
(defn penultimate
  [elems]
  (nth (reverse elems) 1 nil))

;; http://www.4clojure.com/problem/12
(defn nth-elem
  [elems n]
  (if (< (count elems) n)
    nil
    (last (take (inc n) elems))))

;; http://www.4clojure.com/problem/22
(defn my-count
  [elems]
  (reduce (fn [a b] (inc a)) 0 elems))

;; http://www.4clojure.com/problem/23
(defn reverse-seq
  [s]
  (reduce #(cons %2 %1) [] s))

;; http://www.4clojure.com/problem/24
(defn sum-up [s] (apply + s))

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
  (flatten (conj
            (vec (drop times s))
            (take times s))))

(defmethod rotation :clock-wise
  [s times]
  (let [times (- times)]
    (flatten (cons
              (take-last times s)
              (drop-last times s)))))

;; http://www.4clojure.com/problem/43
(defn reverse-interleave
  "First we create a list of sub-lists, such that each sub-list starts at
  further and further indices. Then we select those sublists that can give
  us the right items (the first chunk-size).

  Finally we call take-nth on each of these sub-lists to give the result.
  "
  [items chunk-size]
  (let [items-size (count items)
        sub-lists (map #(drop % items) (range items-size))
        sub-lists-large-enough (take chunk-size sub-lists)]
    (map #(take-nth chunk-size %) sub-lists-large-enough)))

;; http://www.4clojure.com/problem/50
(defn split-by-type
  [items]
  (vals (group-by type items)))

;; http://www.4clojure.com/problem/55
(defn count-items
  [items]
  (->>
   items
   (group-by identity)
   (#(zipmap (keys %) (map count (vals %))))))

;; http://www.4clojure.com/problem/56
(defn remove-dups
  [items]
  (reduce
   (fn [uniques item]
     (if (some #(= item %) uniques)
       uniques
       (conj uniques item)))
   []
   items))

;; http://www.4clojure.com/problem/58
(defn composer
  [& funcs]
  (let [funcs (reverse funcs)]
    (reduce (fn [composed f]
              (fn [& args] (f (apply composed args))))
            (first funcs)
            (rest funcs))))

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

;; http://www.4clojure.com/problem/65
(defn black-box-testing
  [arg]
  (letfn [(is-list? [arg]
            (let [a (rand-int 10000) b (rand-int 10000) c (conj arg a b)]
              (= (first c) b)))
          (is-vec? [arg]
            (let [a (rand-int 10000) b (rand-int 10000) c (conj arg a b)]
              (= (last c) b)))
          (is-set? [arg]
            (let [f (first arg) size (count arg)]
              (= (count (conj arg f)) size)))
          (is-map? [arg]
            (try
              (conj arg 0)
              false ; if conj worked, its not a map
              (catch IllegalArgumentException e true)))]
    (cond
      (is-map? arg) :map
      (is-set? arg) :set
      (is-vec? arg) :vec
      (is-list? arg) :list)))

;; http://www.4clojure.com/problem/74
(defn filter-squares
  [s]
  (letfn [(square? [n] (some #(= (* % %) n) (range (quot n 2))))]
    (->>
     s
     (#(s/split % #","))
     (map #(Integer/parseInt %))
     (filter square?)
     (interpose ",")
     (apply str))))

;; http://www.4clojure.com/problem/76
(defn my-trampoline
  [f & args]
  (let [a (apply f args)]
    (some
     #(if (not (fn? %)) %)
     (iterate #(if (fn? %) (%) %) a))))

;; http://www.4clojure.com/problem/77
(defn anagram-finder
  [words]
  (set (->>
        words
        (group-by set)
        vals
        (map set)
        (filter #(> (count %) 1)))))

;; http://www.4clojure.com/problem/60
(defn seq-reduce
  "Only got this to work with lazy sequences after looking at code for reductions function"
  ([f args]
   (seq-reduce f (first args) (rest args)))
  ([f elem args]
   (lazy-seq (cons elem
                   (when-let [s (seq args)]
                     (seq-reduce f
                                 (f elem (first s))
                                 (rest s)))))))

(defn sum-of-divisors [n]
  (let [bounds (range 1 (inc (quot n 2)))]
    (apply + (reduce (fn [divisors x] (if (= 0 (rem n x)) (conj divisors x) divisors))
                     #{}
                     bounds))))

;; http://www.4clojure.com/problem/80
(defn perfect? [n]
  (= (sum-of-divisors n) n))

;; http://www.4clojure.com/problem/69
(defn merge-maps [f & maps]
  (reduce
   (fn [merged-map m]
     (let [common-keys (clojure.set/intersection
                        (set (keys merged-map))
                        (set (keys m)))
           pre-merge (conj merged-map m)]	;; start with a naive merge of all keys
       (reduce
        (fn [final-map key]			;; for all common keys rewrite with call to f
          (assoc final-map key (f (merged-map key) (m key))))
        pre-merge
        common-keys)))
   {}
   maps))

;; http://www.4clojure.com/problem/102
(defn to-camel-case [s]
  (let [words (->>
               s
               (partition-by #(= \- %))
               (filter #(not= '(\-) %))
               (map #(apply str %)))]
    (str (first words)
         (apply str (map s/capitalize (rest words))))))

(defn gcd [a b]
  (if (= 0 a)
    b
    (recur (mod b a) a)))

(defn co-prime? [a b]
  (= 1 (gcd a b)))

;; http://www.4clojure.com/problem/75
(defn euler-totient [n]
  (if (= 1 n)
    1
    (count (filter #(co-prime? n %) (range 1 n)))))

;; http://www.4clojure.com/problem/86
(defn happy? [n]
  (letfn [(split-num [n]
            (->>
             n
             (iterate #(quot % 10))
             (take-while pos?)
             (map #(mod % 10))
             reverse))]
    (loop [n n cache #{}]
      (cond
        (= n 1) true
        (contains? cache n) false
        :else (recur (apply +
                      (map #(* % %)
                           (split-num n))) (conj cache n))))))
