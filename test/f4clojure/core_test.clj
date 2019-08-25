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
    4 (my-count [1 2 2 2])
    0 (my-count nil)))


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
    '([1 4 7] [2 5 8] [3 6 nil]) (reverse-interleave [1 2 3 4 5 6 7 8] 3)
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
