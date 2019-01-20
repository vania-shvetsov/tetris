(ns tetris.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil.applet :as applet]))

(def segment-size 20)
(def field-height-px 440)
(def field-width-px 280)
(def info-panel-width 140)
(def max-tick 35)
(def info-figure-position [17 5])

(def figure-I {:matrix [[-1 0] [0 0] [1 0] [2 0]]
               :color [0 255 255]
               :shape :I})

(def figure-J {:matrix [[-1 0] [0 0] [1 0] [1 1]]
               :color [0 11 255]
               :shape :J})

(def figure-L {:matrix [[-1 1] [-1 0] [0 0] [1 0]]
               :color [255 154 5]
               :shape :L})

(def figure-O {:matrix [[-1 0] [0 0] [-1 1] [0 1]]
               :color [255 6 255]
               :shape :O})

(def figure-S {:matrix [[-1 1] [0 1] [0 0] [1 0]]
               :color [9 255 5]
               :shape :S})

(def figure-T {:matrix [[-1 0] [0 0] [0 1] [1 0]]
               :color [160 9 255]
               :shape :T})

(def figure-Z {:matrix [[-1 0] [0 0] [0 1] [1 1]]
               :color [255 0 4]
               :shape :Z})

(def figures [figure-I figure-J figure-L figure-O figure-S figure-T figure-Z])

(defn field-width [] (/ field-width-px segment-size))

(defn field-height [] (/ field-height-px segment-size))

(defn field-center [] (/ (field-width) 2))

(defn in-range? [x a b] (<= a x b))

(def not-in-range? (complement in-range?))

(defn figure-coords [fig]
  (let [[x y] (:position fig)]
    (map (fn [[x-rel y-rel]]
           [(+ x-rel x) (+ y-rel y)]) (:matrix fig))))

(defn to-start-position [fig]
  (assoc-in fig [:position] [(field-center) 0]))

(defn new-figure
  ([]
   (-> (rand-nth figures)
       (to-start-position)))
  ([coord]
   (-> (rand-nth figures)
       (assoc-in [:position] coord))))

(defn collide? [fig glass]
  (let [coords (figure-coords fig)]
    (or (some #(not-in-range? % 0 (dec (field-width))) (map first coords))
        (some #(not-in-range? % 0 (dec (field-height))) (map second coords))
        (some #(some (fn [[y xs]]
                       (and (= (% 1) y) (xs (% 0)))) glass) coords))))

(defn rotate [fig glass]
  (let [fig' (update-in fig [:matrix] #(mapv (fn [[x y]] [(- y) x]) %))]
    (if (collide? fig' glass) fig fig')))

(defn move-left [fig glass]
  (let [fig' (update-in fig [:position] (fn [[x y]] [(dec x) y]))]
    (if (collide? fig' glass) fig fig')))

(defn move-right [fig glass]
  (let [fig' (update-in fig [:position] (fn [[x y]] [(inc x) y]))]
    (if (collide? fig' glass) fig fig')))

(defn move-down* [fig]
  (update-in fig [:position] (fn [[x y]] [x (inc y)])))

(defn move-down [fig glass]
  (let [fig' (move-down* fig)]
    (if (collide? fig' glass) fig fig')))

(defn pick-next-figure [state]
  (-> state
      (assoc-in [:current-figure] (to-start-position (:next-figure state)))
      (assoc-in [:next-figure] (new-figure info-figure-position))))

(defn persist-figure [state]
  (let [{:keys [current-figure glass]} state
        coords (figure-coords current-figure)
        glass' (reduce (fn [m [x y]]
                         (if (contains? m y)
                           (update-in m [y] #(conj % x))
                           (assoc m y #{x})))
                       glass coords)]
    (assoc-in state [:glass] glass')))

(defn remove-filled-rows [state]
  (let [glass (:glass state)
        glass' (into (empty glass)
                     (remove (fn [[_ xs]]
                               (= (field-width) (count xs))) glass))]
    (assoc-in state [:glass] glass')))

(defn squash-rows [state]
  (let [glass (:glass state)
        h (field-height)
        glass' (into (empty glass)
                     (map (fn [new-y [_ xs]] [new-y xs])
                          (range (- h (count glass)) h)
                          glass))]
    (assoc-in state [:glass] glass')))

(defn auto-move-down [state]
  (let [{:keys [current-figure glass]} state
        fig' (move-down* current-figure)
        has-collision (collide? fig' glass)]
    (if has-collision
      (-> state
          (persist-figure)
          (remove-filled-rows)
          (squash-rows)
          (pick-next-figure))
      (-> state
          (assoc-in [:current-figure] fig')))))

(defn draw-segment [x y color]
  (apply q/fill color)
  (q/stroke-weight 2)
  (q/stroke 255)
  (q/rect (* segment-size x)
          (* segment-size y)
          segment-size segment-size))

(defn draw-figure [fig]
  (let [color (:color fig)]
    (doseq [[x y] (figure-coords fig)]
      (draw-segment x y color))))

(defn draw-glass [glass]
  (doseq [[y xs] glass
          x xs]
    (draw-segment x y [150 150 150])))

(defn draw-info-panel []
  (q/stroke-weight 2)
  (q/stroke 200)
  (let [x (+ field-width-px 2)]
    (q/line x 0 x field-height-px)))

(defn draw-background []
  (q/stroke-weight 2)
  (q/stroke 150)
  (q/point segment-size segment-size)
  (doseq [x (range segment-size field-width-px segment-size)
          y (range segment-size field-height-px segment-size)]
    (q/point x y)))

(defn initial-state []
  {:next-figure (new-figure info-figure-position)
   :current-figure (new-figure)
   :speed 1
   :glass (sorted-map)
   :run false
   :tick max-tick})

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (initial-state))

(defn update-state [state]
  (let [{:keys [run tick speed]} state]
    (if run
      (if (<= tick 0)
        (-> state
            (auto-move-down)
            (assoc-in [:tick] max-tick))
        (-> state
            (update-in [:tick] #(- % speed))))
      state)))

(defn on-key-typed [state event]
  (let [{:keys [glass run]} state
        event-key (name (:key event))]
    (if run
      (case event-key
        "w" (update-in state [:current-figure] rotate glass)
        "a" (update-in state [:current-figure] move-left glass)
        "d" (update-in state [:current-figure] move-right glass)
        "s" (update-in state [:current-figure] move-down glass)
        " " (assoc-in state [:run] false)
        state)
      (case event-key
        " " (assoc-in state [:run] true)
        state))))

(defn draw [state]
  (q/background 255)
  (draw-info-panel)
  (let [{:keys [current-figure next-figure glass]} state]
    (draw-background)
    (draw-figure current-figure)
    (draw-figure next-figure)
    (draw-glass glass)))

(q/defsketch tetris
  :title "Tetris"
  :size [(+ field-width-px info-panel-width) field-height-px]
  :setup setup
  :update update-state
  :draw draw
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode]
  :key-typed on-key-typed)

(defn get-state []
  (applet/with-applet tetris
    (q/state)))

(defn inc-speed []
  (applet/with-applet tetris
    (swap! (q/state-atom) update-in [:speed] inc)))

(defn dec-speed []
  (applet/with-applet tetris
    (swap! (q/state-atom) update-in [:speed] dec)))

(def pp clojure.pprint/pprint)

(comment
  (get-state)

  (inc-speed)

  (dec-speed)

  (applet/with-applet tetris
    (q/exit))
  )
