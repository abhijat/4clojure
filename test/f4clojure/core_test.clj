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
