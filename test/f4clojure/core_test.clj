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
