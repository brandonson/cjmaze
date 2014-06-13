(ns brandonson.cjmaze.core-test
  (:require [clojure.test :refer :all]
            [brandonson.cjmaze.core :refer :all]))

;test generation of a 2x2 maze.  Should always have one wall
(deftest simple-2-2-map-test
  (is (= 1 (count (:walls (gen-maze 2 2))))))

;test that width and height of mazes is approprate

(deftest maze-size-test
  (let [maze (gen-maze 5 2)]  
    (do 
      (is (= 5 (:width maze)))
      (is (= 2 (:height maze))))))
