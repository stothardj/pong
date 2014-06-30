(ns twod.core-test
  (:require [clojure.test :refer :all]
            [twod.core :refer :all]
            [quil.core :refer [atan2 abs]]))

(defn float=
  "Test if two floats are approx equals"
  ([expected actual] (float= actual expected 0.0001))
  ([expected actual epsilon]
     (< (abs (- expected actual)) epsilon)))

(def all-true? (partial every? identity))

(deftest test-bounds
  (testing "x"
    (is (= [10 40] (bounds-x {:x 10 :width 30}))))
  (testing "y"
    (is (= [12 22] (bounds-y {:y 12 :height 10})))))

(deftest test-center
  (is (== 60 (center-y {:y 10 :height 100}))))

(deftest test-update-loc
  (is (= {:x 12 :y 17} (update-loc {:x 3 :y 9} [9 8]))))

(deftest test-normalise
  (is (= {:x 3 :y 12} (normalise [3 10] [7 14] {:x -5 :y 12})))
  (is (= {:x 1 :y 9} (normalise [0 9] {:x 1 :y 13}))))

(deftest test-charcode-to-keyword
  (is (= :122 (charcode-to-keyword 122))))

(deftest test-calc-delta
  (is (all-true? (map float= [4 -3] (calc-delta 5 (atan2 3 4))))))

(deftest test-calc-ball-delta
  (is (all-true? (map float= [4 -3]
                      (calc-ball-delta {:speed 5 :angle (atan2 3 4)})))))

(deftest test-move
  (is (-> (merge-with float=
                      {:x 5 :y -1 :speed 5 :angle (atan2 3 4)}
                      (move {:x 1 :y 2 :speed 5 :angle (atan2 3 4)}))
          vals
          all-true?)))

(deftest test-bounce-wall
  (testing "Doesnt bounce on screen"
    (is (= 0.1 (:angle (bounce-wall {:x 5 :y 7 :angle 0.1 :speed 1})))))
  (testing "Bounces off top"
    (is (= -0.1 (:angle (bounce-wall {:x 5 :y -1 :angle 0.1 :speed 1})))))
  (testing "Bounces off bottom"
    (is (= 0.1 (:angle (bounce-wall {:x 5 :y (inc (second screen-bounds-y))
                                     :angle -0.1 :speed 1})))))
  (testing "Doesnt reverse bounce at top"
    (is (= -0.1 (:angle (bounce-wall {:x 5 :y -1 :angle -0.1 :speed 1})))))
  (testing "Doesnt reverse bounce at bottom"
    (is (= 0.1 (:angle (bounce-wall {:x 5 :y (inc (second screen-bounds-y))
                                     :angle 0.1 :speed 1}))))))

(deftest test-overlap-ranges
  (is (overlap-ranges? [1 3] [2 4]))
  (is (overlap-ranges? [1 4] [2 3]))
  (is (not (overlap-ranges? [1 2] [3 4])))
  (is (not (overlap-ranges? [3 4] [1 2]))))

(deftest test-overlap-rects
  (is (overlap-rects? {:x 3 :y 7 :width 10 :height 12}
                      {:x 7 :y 18 :width 5 :height 3}))
  (is (not (overlap-rects? {:x 3 :y 7 :width 10 :height 12}
                           {:x 7 :y 20 :width 5 :height 3}))))

(deftest test-maybe-reset-ball
  (is (= 3 (:x (maybe-reset-ball {:x 3}))))
  (is (not (= -1 (:x (maybe-reset-ball {:x -1})))))
  (is (not (= (inc (second screen-bounds-x))
              (:x (maybe-reset-ball {:x (inc (second screen-bounds-x))}))))))
