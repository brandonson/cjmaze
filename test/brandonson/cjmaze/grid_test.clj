(ns brandonson.cjmaze.grid-test
  (:require [clojure.test :refer :all]
            [brandonson.cjmaze.core :refer :all]
            [brandonson.cjmaze.grid :refer :all]))

(deftest grid-size-test
  (let [grid (maze-to-grid (gen-maze 2 3))]
    (do
      ;size is 2*side + 1
      (is (= 5 (:width grid)))
      (is (= 7 (:height grid))))))

(deftest grid-corner-walls
  (let [grid (maze-to-grid (gen-maze 2 2))]
    (do
      (is (= 'wall (at-grid-xy grid 0 0)))
      (is (= 'wall (at-grid-xy grid 0 4)))
      (is (= 'wall (at-grid-xy grid 4 0)))
      (is (= 'wall (at-grid-xy grid 4 4))))))

