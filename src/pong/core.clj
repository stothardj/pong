(ns pong.core
  (:require [quil.core :refer :all]
            [pong.menu :as menu])
  (:import java.awt.event.KeyEvent
           (java.util.concurrent Executors TimeUnit)))

(def params {:ai-speed 8
             :screen-width 800
             :screen-height 600
             :background-colour 0
             :foreground-colour 255
             :max-points 3
             :paddle-height 50
             :paddle-width 5
             :ball-radius 8
             :start-speed 4
             :speed-bump 0.5})

(def screen-bounds-x [0 (:screen-width params)])
(def screen-bounds-y [0 (:screen-height params)])

(def left-paddle (atom {:x 20
                        :y (-> params :screen-height (/ 2))
                        :width (:paddle-width params)
                        :height (:paddle-height params)}))
(def right-paddle (atom {:x (-> params :screen-width (- 20))
                         :y (-> params :screen-height (/ 2))
                         :width (:paddle-width params)
                         :height (:paddle-height params)}))

(def start-menu
  (atom (menu/menu
         :one-player "One Player"
         :two-player "Two Player"
         :quit "Quit")))

(defn bounds-x
  "Return [left right] of a {:x :width} map"
  [m]
  (let [{:keys [x width]} m] [x (+ x width)]))

(defn bounds-y 
  "Return [top bottom] of a {:y :height} map"
  [m]
  (let [{:keys [y height]} m] [y (+ y height)]))

(defn center-x
  "Return the x coordinate of the center of a rectangle"
  [rect]
  (->> rect bounds-x (reduce +) (* 0.5)))

(defn center-y
  "Return the y coordinate of the center of a rectangle"
  [rect]
  (->> rect bounds-y (reduce +) (* 0.5)))

(defn update-loc
  "Update the x and y values in map m by adding delta"
  [m delta]
  (merge-with + m (zipmap [:x :y] delta)))

(def game-state (atom :start-screen))
(def left-score (atom 0))
(def right-score (atom 0))

(def paddles
  {:left left-paddle
   :right right-paddle})

(defn- rand-angle []
  (rand (* 2 Math/PI)))

(defn start-angle
  "Random angle which will atleat have some horizontal movement."
  []
  (->> rand-angle repeatedly (filter #(< 0.3 (abs (cos %)))) first))

(defn new-ball []
  {:x (-> params :screen-width (/ 2))
   :y (-> params :screen-height (/ 2))
   :width (:ball-radius params)
   :height (:ball-radius params)
   :speed (:start-speed params)
   :angle (start-angle)})

(def ball (atom (new-ball)))

(defn- bound
  "Restrict value to be between min and max."
  [a-min a-max value]
  (max a-min (min a-max value)))

(defn normalise
  ([bounds position] (normalise bounds bounds position))
  ([x-bounds y-bounds position]
     (let [[min-x max-x] x-bounds
           [min-y max-y] y-bounds
           {x :x y :y} position
           new-x (bound min-x max-x x)
           new-y (bound min-y max-y y)]
       (assoc position :x new-x :y new-y))))

(defn setup []
  (smooth)
  (no-stroke)
  (frame-rate 30))

(defn draw-rect [m]
  (->> m ((juxt :x :y :width :height)) (apply rect)))

(defn draw-game []
  (background-float (params :background-colour))
  (fill (params :foreground-colour))
  (doseq [paddle (vals paddles)]
    (draw-rect @paddle))
  (draw-rect @ball)
  (text "Use WS and arrow keys to move" 10 (-> params :screen-height (- 10)))
  (text (str @left-score) 20 20)
  (text (str @right-score) (-> params :screen-width (- 20)) 20))

(defn draw-start []
  (background-float (params :background-colour))
  (fill (params :foreground-colour))
  (text "Pong" 20 40)
  (doseq [[idx option] (menu/options-indexed @start-menu)]
    (let [selected (-> option meta :selected)]
      (when selected (fill 255 0 0))
      (text (second option) 20 (+ 60 (* 20 idx)))
      (when selected (fill (params :foreground-colour))))))

(defn draw []
  (if (= :start-screen @game-state) (draw-start) (draw-game)))

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

(defn calc-delta
  "The amount of movement in [dx dy]"
  [speed angle]
  (->> angle
       ((juxt cos #(- (sin %))))
       (map (partial * speed))))

(defn calc-ball-delta [m] (apply calc-delta ((juxt :speed :angle) m)))

(defn move-paddle [paddle move]
  (swap! paddle #(update-loc % move))
  (swap! paddle #(normalise
                  (map - screen-bounds-x [0 (:width %)])
                  (map - screen-bounds-y [0 (:height %)])
                  %)))

(defn human-move [paddle-side the-key-pressed]
  (let [paddle (paddles paddle-side)
        valid-key (valid-keys paddle-side)
        move (moves (get valid-key the-key-pressed :still))]
    (move-paddle paddle move)))

(defn ai-move [paddle-side]
  (let [paddle (paddles paddle-side)
        ball-x (center-x @ball)
        ball-y (center-y @ball)
        paddle-x (center-x @paddle)
        paddle-y (center-y @paddle)
        move [0 (bound (- (:ai-speed params)) (:ai-speed params) (- ball-y paddle-y))]]
    (when (< (abs (- paddle-x ball-x)) (-> params :screen-width (/ 2)))
      (move-paddle paddle move))))

(def handle (atom nil))

(declare game-loop)
(defn start-game
  "Start a new pong game."
  [executor mode]
  (reset! game-state mode)
  (reset! left-score 0)
  (reset! right-score 0)
  (reset! ball (new-ball))
  (reset! left-paddle {:x 20
                       :y (-> params :screen-height (/ 2))
                       :width (:paddle-width params)
                       :height (:paddle-height params)})
  (reset! right-paddle {:x (-> params :screen-width (- 20))
                        :y (-> params :screen-height (/ 2))
                        :width (:paddle-width params)
                        :height (:paddle-height params)})
  (compare-and-set! handle nil
                    (.scheduleAtFixedRate executor
                                          game-loop
                                          20
                                          20
                                          TimeUnit/MILLISECONDS)))

(defn stop-game
  "Stop the currently running pong game."
  []
  (when @handle
    (.cancel @handle true)
    (reset! handle nil)))

(defn- get-key
  "Returns the current key-code, or raw-key if that is unavailable."
  []
  (let [raw-key (raw-key)
        the-key-code (key-code)]
    (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)))

(defn select-action [option executor]
  (case (first option)
    :one-player (start-game executor :one-player)
    :two-player (start-game executor :two-player)
    :quit nil))

(defn key-press-start-screen
  [key executor]
  (when-let [action ({\w :up
                      \s :down
                      KeyEvent/VK_UP :up
                      KeyEvent/VK_DOWN :down
                      \newline :select
                      \space :select
                      \a :select} key)]
    (case action
      :up (swap! start-menu menu/select-prev)
      :down (swap! start-menu menu/select-next)
      :select (select-action (menu/selected @start-menu) executor))))

(defn key-press-one-player
  [key]
  (human-move :left key))

(defn key-press-two-player
  [key]
  (doseq [side [:left :right]] (human-move side key)))

(defn key-press [executor]
  (let [key (get-key)]
    (case @game-state
      :start-screen (key-press-start-screen key executor)
      :one-player (key-press-one-player key)
      :two-player (key-press-two-player key))))

(defn move
  "Changes location based on speed and angle."
  [m]
  (update-loc m (calc-ball-delta m)))

(defn bounce-wall
  "Changes direction based on wall collided with."
  [m]
  (let [[_ dy] (calc-ball-delta m)
        y (:y m)
        [mn mx] screen-bounds-y]
    (if (or (and (< y mn) (< dy 0))
            (and (> y mx) (> dy 0)))
      (-> m
       (update-in [:angle] -)
       (update-in [:speed] (partial + (:speed-bump params))))
      m)))

(defn overlap-ranges?
  "Return true if two ranges overlap at all.
   range is [start end]"
  [a b]
  (let [[as ae] a
        [bs be] b]
    (not (or (< ae bs) (< be as)))))

(defn overlap-rects?
  "Return true if two rects overlap at all."
  [a b]
  (and (overlap-ranges? (bounds-x a) (bounds-x b))
       (overlap-ranges? (bounds-y a) (bounds-y b))))

(defn bounce-paddle
  "Changes direction based on paddle collided with."
  [m]
  (let [[dx] (calc-ball-delta m)
        ball-center (center-y m)
        left-diff (- (center-y @left-paddle) ball-center)
        right-diff (- (center-y @right-paddle) ball-center)]
    (cond
     (and (< dx 0) (overlap-rects? @ball @left-paddle))
     (-> m
      (update-in [:angle]
                 #(+ (- (+ % Math/PI)) (/ left-diff (:height @left-paddle))))
      (update-in [:speed] (partial + (:speed-bump params))))
     
     (and (> dx 0) (overlap-rects? @ball @right-paddle))
     (-> m
         (update-in [:angle]
                    #(- (- (+ % Math/PI)) (/ right-diff (:height @right-paddle))))
         (update-in [:speed] (partial + (:speed-bump params))))
     
     :else m)))

(defn start-screen
  "Transition to the start screen."
  []
  (stop-game)
  (reset! game-state :start-screen))

(defn game-over? []
  (let [max-points (:max-points params)]
    (or (= @left-score max-points) (= @right-score max-points))))

(defn maybe-reset-ball
  "Put the ball back in the center if it's offscreen."
  [m]
  (let [x (:x m)
        [mn mx] screen-bounds-x]
    (cond (< x mn) (do (swap! right-score inc) (new-ball))
          (> x mx) (do (swap! left-score inc) (new-ball))
          :else m)))

(defn game-loop []
  (swap! ball move)
  (swap! ball bounce-wall)
  (swap! ball bounce-paddle)
  (swap! ball maybe-reset-ball)
  (when (= :one-player @game-state) (ai-move :right))
  (when (game-over?) (start-screen)))

(defn -main [& args]
  (let [executor (Executors/newScheduledThreadPool 1)]
    (sketch
      :title "Pong"
      :size ((juxt :screen-width :screen-height) params)
      :setup setup
      :draw draw
      :key-pressed #(key-press executor)
      :on-close (fn []
                  (stop-game)
                  (.shutdownNow executor)))))
