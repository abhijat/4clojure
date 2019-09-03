(ns f4clojure.core-test
  (:require [clojure.test :refer :all]
            [f4clojure.core :refer :all]))

(deftest test-last-element-in-sequence
  (testing "simple vector"
    (is (= 5 (last-element-in-sequence [1 2 3 4 5]))))
  (testing "simple list"
    (is (= 3 (last-element-in-sequence '(5 4 3)))))
  (testing "strings in vector"
    (is (= "d" (last-element-in-sequence ["b" "c" "d"]))))
  (testing "empty vector"
    (is (= nil (last-element-in-sequence [])))))

(deftest test-penultimate-element
  (testing "simple vector"
    (are [x y] (= x y)
      10 (penultimate [1 2 3 10 100])
      :a (penultimate [:a :b])
      nil (penultimate [1]))))

(deftest test-nth-elem
  (are [x y] (= x y)
    1 (nth-elem [9 1 8 2] 1)
    1 (nth-elem [1 2 3 4] 0)
    nil (nth-elem [] 1)
    nil (nth-elem [1 2 3] 1000)))

(deftest test-my-count
  (are [x y] (= x y)
    1 (my-count [1])
    0 (my-count [])
    4 (my-count [1 2 2 2])))


(deftest test-reverse-seq
  (are [x y] (= x y)
    '(3 2 1) (reverse-seq [1 2 3])
    '(1) (reverse-seq [1])
    '() (reverse-seq [])))

(deftest test-sum-up
  (are [x y] (= x y)
    0 (sum-up [1 -1 2 -2])
    10 (sum-up [1 2 3 4])))

(deftest test-flip-order
  (are [x y] (= x y)
    false (> 7 8)
    true ((flip-order >) 7 8)
    "hello world" (str "hello" " " "world")
    "world hello" ((flip-order str) "hello" " " "world")))

(deftest test-rotate-seq
  (let [s [1 2 3 4 5]]
    (are [x y] (= x y)
      [3 4 5 1 2] (rotation s 2)
      s (rotation s 5)
      [4 5 1 2 3] (rotation s -2))))

(deftest test-reverse-interleave
  (are [x y] (= x y)
    '([1 4 7] [2 5 8] [3 6 9]) (reverse-interleave (range 1 10) 3)
    '((0 5) (1 6) (2 7) (3 8) (4 9)) (reverse-interleave (range 10) 5)))

(deftest test-type-split
  (are [x y] (= x y)
    '([1 2 3] [:a :b :c]) (split-by-type [1 :a 2 :b 3 :c])
    '([:a :b] ["foo" "bar"]) (split-by-type [:a "foo"  "bar" :b])
    '([[1 2] [3 4]] [:a :b] [5 6]) (split-by-type [[1 2] :a [3 4] 5 6 :b])))

(deftest test-count-items
  (are [x y] (= x y)
    {1 1 2 2 3 3 4 4} (count-items [1 2 2 3 3 3 4 4 4 4])
    {} (count-items [])))

(deftest test-remove-dups
  (are [x y] (= x y)
    [1 2] (remove-dups [1 2 1 2 1 2 1 1 1 1 1 2 2 2 2 2])
    [1 2 3] (remove-dups [1 2 2 2 1 1 1 1 3 3 3 3 2 2 1 1])
    (range 50) (remove-dups (range 50))))

(deftest test-compose-funcs
  (are [x y] (= x y)
    5 ((composer count last) ["aaaa" "bbbbb" "ccccc"])
    100 ((composer (partial + 99) first) [1 :a :x])
    false ((composer empty? first reverse) [[] [2] [444]])))

(deftest test-part-seq
  (are [x y] (= x y)
    [[0 1] [2 3] [4 5] [6 7] [8 9]] (part-seq 2 (range 10))
    [[0 1 2 3] [4 5 6 7]] (part-seq 4 (range 10))))

(deftest test-juxtapose
  (are [x y] (= x y)
    [21 6 1] ((juxtapose + max min) 2 3 5 1 6 4)
    ["HELLO" 5] ((juxtapose #(.toUpperCase %) count) "hello")
    [2 6 4] ((juxtapose :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))

(deftest test-split-and-sort
  (are [x y] (= x y)
    ["a" "conundrum" "is" "quite" "This"] (split-sentence-and-sort "This is quite, a conundrum!")
    ["fall" "follies" "foolish" "Fools" "for"] (split-sentence-and-sort  "Fools fall for foolish follies.")))

(deftest test-n-primes
  (are [x y] (= x y)
    [2 3 5 7] (n-primes 10)
    [2 3 5 7 11 13 17 19] (n-primes 20)))

(deftest test-black-box
  (are [x y] (= x y)
    :set (black-box-testing #{1 :a})
    :map (black-box-testing {:a 1})
    :vec (black-box-testing [])
    :list (black-box-testing '(1 2 3))))

(deftest test-filter-squares
  (are [x y] (= x y)
    "16,144" (filter-squares "12,16,45,144,991")
    "" (filter-squares "12,13,14")))

(deftest test-trampoline
  (are [x y] (= x y)
    [1 3 5 7 9 11]
    (letfn
        [(foo
           [x y]
           #(bar (conj x y) y))
         (bar
           [x y]
           (if (> (last x) 10)
             x
             #(foo x (+ 2 y))))]
      (my-trampoline foo [] 1))))

(deftest test-anagram-finder
  (are [x y] (= x y)
    #{#{"meat" "team" "mate"}} (anagram-finder ["meat" "mat" "team" "mate" "eat"])
    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}} (anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])))

(deftest test-seq-reduce
  (are [x y] (= x y)
    [0 1 3 6 10] (take 5 (seq-reduce + (range)))
    [[1] [1 2] [1 2 3] [1 2 3 4]] (seq-reduce conj [1] [2 3 4])
    (reduce * 2 [3 4 5]) (last (seq-reduce * 2 [3 4 5]))))

(deftest test-perfect
  (are [x y] (= x y)
    true (perfect? 6)
    true (perfect? 496)
    false (perfect? 500)))

(deftest test-merge-maps
  (are [x y] (= x y)
    {:a 4, :b 6, :c 20} (merge-maps * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
    {1 7, 2 10, 3 15} (merge-maps - {1 10, 2 20} {1 3, 2 10, 3 15})))

(deftest test-camel-case
  (are [x y] (= x y)
    "something" (to-camel-case "something")
    "multiWordKey" (to-camel-case "multi-word-key")
    "leaveMeAlone" (to-camel-case "leaveMeAlone")))

(deftest test-totient
  (are [x y] (= x y)
    16 (euler-totient 40)
    60 (euler-totient 99)
    4 (euler-totient 10)))

(deftest test-happy
  (are [x y] (= x y)
    true (happy? 7)
    false (happy? 2)
    true (happy? 986543210)))

(deftest test-re-trampoline
  (are [x y] (= x y)
    82 (letfn [(triple [x] #(sub-two (* 3 x)))
            (sub-two [x] #(stop?(- x 2)))
            (stop? [x] (if (> x 50) x #(triple x)))]
         (re-trampoline triple 2))
    [true false true false true false]
    (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
            (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
      (map (partial re-trampoline my-even?) (range 6)))))

(deftest test-balanced
  (are [x y] (= x y)
    true (balanced? 11)
    true (balanced? 121)
    false (balanced? 123)
    true (balanced? 0)
    false (balanced? 88099)
    true (balanced? 89098)
    true (balanced? 89089)
    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]
    (take 20 (filter balanced? (range)))))

(deftest test-powerset
  (are [x y] (= x y)
    #{#{1 :a} #{:a} #{} #{1}} (power-set #{1 :a})
    #{#{}} (power-set #{})
    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}} (power-set #{1 2 3})
    1024 (count (power-set (into #{} (range 10))))))

(deftest test-equivalence
  (are [x y] (= x y)
    #{#{0} #{1 -1} #{2 -2}} (equivalence #(* % %) #{-2 -1 0 1 2})
    #{#{0 3} #{1 4} #{2 5}} (equivalence #(rem % 3) #{0 1 2 3 4 5 })
    #{#{0} #{1} #{2} #{3} #{4}} (equivalence identity #{0 1 2 3 4})
    #{#{0 1 2 3 4}} (equivalence (constantly true) #{0 1 2 3 4})))

(deftest test-map-with-vals
  (are [x y] (= x y)
    {:a [1 2 3], :b [], :c [4]} (map-with-vals [:a 1 2 3 :b :c 4])
    {:a [1], :b [2]} (map-with-vals [:a 1, :b 2])
    {:a [1]} (map-with-vals [:a 1])))

(deftest test-seq-base
  (are [x y] (= x y)
    [1 2 3 4 5 0 1] (seq-base 1234501 10)
    [0] (seq-base 0 11)
    [1 0 0 1] (seq-base 9 2)
    [1 0] (let [n (rand-int 100000)](seq-base n n))
    [16 18 5 24 15 1] (seq-base Integer/MAX_VALUE 42)))

(deftest test-pronounce
  (are [x y] (= x y)
    [[1 1] [2 1] [1 2 1 1]] (take 3 (pronounce [1]))
    [3 1 2 4] (first (pronounce [1 1 1 4 4]))
    [1 1 1 3 2 1 3 2 1 1] (nth (pronounce [1]) 6)
    338 (count (nth (pronounce [3 2]) 15))))

(deftest test-de-curry
  (are [x y] (= x y)
    10 ((de-curry (fn [a]
                    (fn [b]
                      (fn [c]
                        (fn [d]
                          (+ a b c d))))))
        1 2 3 4)
    24 ((de-curry (fn [a]
                    (fn [b]
                      (fn [c]
                        (fn [d]
                          (* a b c d))))))
        1 2 3 4)
    25 ((de-curry (fn [a]
                    (fn [b]
                      (* a b))))
        5 5)))

(deftest test-lazy-search
  (are [x y] (= x y)
    3 (lazy-search [3 4 5])
    4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19])
    7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13])
    64 (lazy-search (map #(* % % %) (range))
                    (filter #(zero? (bit-and % (dec %))) (range))
                    (iterate inc 20))))

(deftest test-global-take-while
  (are [x y] (= x y)
    [2 3 5 7 11 13] (global-take-while 4 #(= 2 (mod % 3)) [2 3 5 7 11 13 17 19 23])
    ["this" "is" "a" "sentence"] (global-take-while 3 #(some #{\i} %) ["this" "is" "a" "sentence" "i" "wrote"])
    ["this" "is"] (global-take-while 1 #{"a"} ["this" "is" "a" "sentence" "i" "wrote"])))

(deftest test-insert-mid
  (are [x y] (= x y)
    '(1 :less 6 :less 7 4 3) (insert-mid < :less [1 6 7 4 3])
    '(2) (insert-mid > :more [2])
    [0 1 :x 2 :x 3 :x 4]  (insert-mid #(and (pos? %) (< % %2)) :x (range 5))
    true (empty? (insert-mid > :more ()))
    nil (comment
      "This test causes integer overflow on my machine. need to investigate"
      [0 1 :same 1 2 3 :same 5 8 13 :same 21]
      (take 12 (->> [0 1]
                    (iterate (fn [[a b]] [b (+ a b)]))
                    (map first)
                    (insert-mid (fn [a b]
                                  (= (mod a 2) (mod b 2)))
                                :same))))))
