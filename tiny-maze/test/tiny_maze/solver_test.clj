(ns tiny-maze.solver-test
  (:require [clojure.test :refer :all]
            [tiny-maze.solver :refer :all]))

;; Hint: Start with a smaller maze
(deftest test-solve-maze
  (testing "can find way to exit with 3x3 maze"
    (let [maze [[:S 0 1]
                [1  0 1]
                [1  0 :E]]
          sol [[:x :x 1]
               [1  :x 1]
               [1  :x :x]]]
      (is (= sol (solve-maze maze)))))

  #_(testing "can find way to exit with 4x4 maze"
    (let [maze [[:S 0  0 1]
                [1  1  0 0]
                [1  0  0 1]
                [1  1  0 :E]]
          sol [[:x :x :x 1]
                [1  1 :x 0]
                [1  0 :x 1]
                [1  1 :x :x]]]
     (is (= sol (solve-maze maze))))))

(deftest test-find-start
  (testing "can find start"
    (let [maze [[:S 0]
                [1  :E]]]
      (is (= {:x 0, :y 0} (locate maze :S)))
      (is (= {:x 1, :y 0} (locate [[0 :S] [:E 1]] :S))))))

(deftest test-find-end
  (testing "can find end"
    (let [maze [[:S 0]
                [1  :E]]]
      (is (= {:x 1, :y 1} (locate maze :E)))
      (is (= {:x 0, :y 1} (locate [[0 :S] [:E 1]] :E))))))

(deftest test-distance
  (testing "calculates manhattan metric"
    (let [maze [[:A :B :C :D :E :F]
                [:G :H :I :J :K :L]
                [:M :N :O :P :Q :R]
                [:S :T :U :V :W :X]
                [:Y :Z :a :b :c :d]
                [:e :f :g :h :i :j]
                [:k :l :m :n :o :p]]]
      (is (= 2 (distance maze :A :H)))
      (is (= 2 (distance maze :H :A)))
      (is (= 4 (distance maze :I :b)))
      (is (= 11 (distance maze :k :F))))))

(deftest test-neighbour-positions
  (testing "finds all neighbour positions"
    (let [maze [[:A :B :C :D :E :F]
                [:G :H :I :J :K :L]
                [:M :N :O :P :Q :R]
                [:S :T :U :V :W :X]
                [:Y :Z :a :b :c :d]
                [:e :f :g :h :i :j]
                [:k :l :m :n :o :p]]]
    (is (= [{:x 0, :y 0} {:x 1, :y 1}, {:x 0, :y 2}]
           (neighbour-positions maze :G)))
    (is (= [{:x 5, :y 5} {:x 4, :y 6}]
           (neighbour-positions maze :p)))
    (is (= [{:x 2, :y 2} {:x 3, :y 3} {:x 2, :y 4} {:x 1, :y 3}]
           (neighbour-positions maze :U)))
    )))

(deftest test-update-maze
  (testing "place symbol at position and return new maze"
    (let [maze [[0 0 0]
                [0 0 0]
                [0 0 0]]]
      (is (= [[1 0 0]
              [0 0 0]
              [0 0 0]]
             (update-maze maze {:x 0, :y 0} 1))))))

(deftest test-get-symbol
  (testing "finds the symbol of a given position"
    (let [maze [[:A :B :C :D :E :F]
                [:G :H :I :J :K :L]
                [:M :N :O :P :Q :R]
                [:S :T :U :V :W :X]
                [:Y :Z :a :b :c :d]
                [:e :f :g :h :i :j]
                [:k :l :m :n :o :p]]]
      (is (= :T (get-symbol maze {:x 1, :y 3})))
      )))
