(ns twod.core
  (:require [quil.core :refer :all])
  (:import java.awt.event.KeyEvent
           (java.util.concurrent Executors TimeUnit)))

(def params {
             :screen-dimensions [400 400]
             :background-colour 0
             :foreground-colour 255
             :paddle-height 50
             :paddle-width 5
             :ball-radius 4})

(def screen-bounds-x [0 (first (:screen-dimensions params))])
(def screen-bounds-y [0 (second (:screen-dimensions params))])

(def left-paddle (atom {:x 20
                        :y 190
                        :width (:paddle-width params)
                        :height (:paddle-height params)}))
(def right-paddle (atom {:x (- (first (:screen-dimensions params)) 20)
                         :y 190
                         :width (:paddle-width params)
                         :height (:paddle-height params)}))

(def loc (juxt :x :y))
(defn set-loc [m loc]
  "Set the x and y values in map m to the location"
  (apply (partial assoc m) (interleave [:x :y] loc)))
(defn update-loc [m delta]
  "Update the x and y values in map m by adding delta"
  (->> (interleave [:x :y] delta)
       (partition 2)
       (reduce #(update-in % [(first %2)] + (second %2)) m)))

(def left-score (atom 0))
(def right-score (atom 0))

(def paddles
  {:left left-paddle
   :right right-paddle})

(defn rand-angle []
  (rand (* 2 Math/PI)))

(defn new-ball []
  {:box {:x 200 :y 200 :width (:ball-radius params) :height (:ball-radius params)}
   :speed 2
   :angle (rand-angle)})

(def ball (atom (new-ball)))

(defn normalise
  ([bounds position] (normalise bounds bounds position))
  ([x-bounds y-bounds position]
     (let [[min-x max-x] x-bounds
           [min-y max-y] y-bounds
           [x y] position
           bound (fn [a-min a-max value]
                   (max a-min (min a-max value)))
           new-x (bound min-x max-x x)
           new-y (bound min-y max-y y)]
       (vector new-x new-y))))

(defn setup []
  (smooth)
  (no-stroke)
  (frame-rate 30))

(defn draw
  []
  (background-float (params :background-colour))
  (fill (params :foreground-colour))
  (doseq [paddle (vals paddles)]
    (->> @paddle
         ((juxt :x :y :width :height))
         (apply rect)))
  (->> (:box @ball)
       ((juxt :x :y :width :height))
       (apply rect))
  (text "Use WS and arrow keys to move" 10 390)
  (text (str @left-score) 20 20)
  (text (str @right-score) (- (second screen-bounds-x) 20) 20))

(def valid-keys-left {\w :up
                      \s :down})

(def valid-keys-right {KeyEvent/VK_UP :up
                       KeyEvent/VK_DOWN :down})

(def valid-keys
  {:left valid-keys-left
   :right valid-keys-right})

(def moves {:up [0 -10]
            :down [0 10]
            :left [-10 0]
            :right [10 0]
            :still [0 0]})

(defn charcode-to-keyword [c] (->> c str keyword))

(defn calc-delta [speed angle]
  (->> angle
       ((juxt cos #(- (sin %))))
       (map (partial * speed))))

(defn calc-ball-delta [m] (apply calc-delta ((juxt :speed :angle) m)))

(defn maybe-move [paddle-side the-key-pressed]
  (let [paddle (paddles paddle-side)
        valid-key (valid-keys paddle-side)
        move (moves (get valid-key the-key-pressed :still))]
    (swap! paddle #(update-loc % move))
    (swap! paddle #(set-loc %
                            (normalise
                             (map - screen-bounds-x [0 (:width %)])
                             (map - screen-bounds-y [0 (:height %)])
                             (loc %))))))

(defn key-press []
  (let [raw-key (raw-key)
        the-key-code (key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    (doseq [side [:left :right]] (maybe-move side the-key-pressed))))

(defn move [m]
  "Changes location based on speed and angle."
  (update-in m [:box] #(update-loc % (calc-ball-delta m))))

(defn bounce-wall [m]
  "Changes direction based on wall collided with."
  (let [[_ dy] (calc-ball-delta m)
        y (get-in m [:box :y])
        [mn mx] screen-bounds-y]
    (if
        (or (and (< y mn) (< dy 0)) (and (> y mx) (> dy 0)))
      (update-in m [:angle] -)
      m)))

(defn overlap-ranges?
  "Return true if two ranges overlap at all.
   range is [start end]"
  [a b]
  (let [[as ae] a
        [bs be] b]
    (not (or (< ae bs) (< be as)))))

(defn overlap-rects?
  "Return true if two rects overlap at all.
   rect is [left top width height]"
  [a b]
  (let [[al at aw ah] a
        [bl bt bw bh] b]
    (and (overlap-ranges? [al (+ al aw)] [bl (+ bl bw)])
         (overlap-ranges? [at (+ at ah)] [bt (+ bt bh)]))))

(defn bounce-paddle [m]
  "Changes direction based on paddle collided with."
  (let [[dx] (calc-ball-delta m)
        ball-rect ((juxt :x :y :width :height) (:box @ball))
        paddle-left-rect ((juxt :x :y :width :height) @left-paddle)
        paddle-right-rect ((juxt :x :y :width :height) @right-paddle)]
    (if (or (and (< dx 0) (overlap-rects? ball-rect paddle-left-rect))
            (and (> dx 0) (overlap-rects? ball-rect paddle-right-rect)))
      (update-in m [:angle] #(- (+ % Math/PI)))
      m)))

(defn maybe-reset-ball [m]
  "Put the ball back in the center if it's offscreen."
  (let [x (get-in m [:box :x])
        [mn mx] screen-bounds-x]
    (cond (< x mn) (do (swap! right-score inc) (new-ball))
          (> x mx) (do (swap! left-score inc) (new-ball))
          :else m)))

(defn game-loop []
  (swap! ball move)
  (swap! ball bounce-wall)
  (swap! ball bounce-paddle)
  (swap! ball maybe-reset-ball))

(defn -main [& args]
  (let [executor (Executors/newScheduledThreadPool 1)
        handle (.scheduleAtFixedRate executor
                                     #'game-loop
                                     20
                                     20
                                     TimeUnit/MILLISECONDS)]
    (defsketch key-listener
      :title "Keyboard arrow keys demo"
      :size (params :screen-dimensions)
      :setup setup
      :draw draw
      :key-pressed #'key-press
      :on-close (fn []
                  (.cancel handle true)
                  (.shutdownNow executor)))))
