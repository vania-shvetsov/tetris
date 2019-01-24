(ns tetris.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [quil.applet :as applet])
  (:import [java.lang Math]))

(def segment-size 20)
(def field-height-px 340)
(def field-width-px 200)
(def info-panel-width 140)
(def max-tick 35)
(def next-figure-y-position 50)
(def help-text-position [225 238])

(def figures
  {:I {:shape [[-1 0] [0 0] [1 0] [2 0]]
       :color [0 255 255]}
   :J {:shape [[-1 0] [0 0] [1 0] [1 1]]
       :color [0 11 255]}
   :L {:shape [[-1 1] [-1 0] [0 0] [1 0]]
       :color [255 154 5]}
   :O {:shape [[-1 0] [0 0] [-1 1] [0 1]]
       :color [255 6 255]}
   :S {:shape [[-1 1] [0 1] [0 0] [1 0]]
       :color [9 255 5]}
   :T {:shape [[-1 0] [0 0] [0 1] [1 0]]
       :color [160 9 255]}
   :Z {:shape [[-1 0] [0 0] [0 1] [1 1]]
       :color [255 0 4]}})


(defn field-width [] (/ field-width-px segment-size))


(defn field-height [] (/ field-height-px segment-size))


(defn field-center [] (/ (field-width) 2))


(defn in-range? [x a b] (<= a x b))


(def not-in-range? (complement in-range?))


(defn figure-coords [fig]
  (let [[x y] (:position fig)]
    (map (fn [[x-rel y-rel]]
           [(+ x-rel x) (+ y-rel y)]) (:shape fig))))


(defn figure-width [fig]
  (->> (:shape fig)
       (map first)
       (into #{})
       (count)))


(defn min-fig-x [fig]
  (->> (:shape fig)
       (flatten)
       (apply min)))


(defn to-start-position [fig]
  (assoc-in fig [:position] [(field-center) 0]))


(defn new-figure []
  (rand-nth (vals figures)))


(defn collide? [fig glass]
  (let [coords (figure-coords fig)]
    (or (some (fn [[x y]]
                (or (not-in-range? x 0 (dec (field-width)))
                    (not-in-range? y 0 (dec (field-height)))
                    (some (fn [[row xs]]
                            (and (= y row)
                                  (xs x))) glass))) coords))))


(defn rotate [fig glass]
  (let [fig' (update-in fig [:shape] #(mapv (fn [[x y]] [(- y) x]) %))]
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
      (assoc-in [:figure] (to-start-position (:next-figure state)))
      (assoc-in [:next-figure] (new-figure))))


(defn persist-figure [state]
  (let [{:keys [figure glass]} state
        coords (figure-coords figure)
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
  (let [{:keys [figure glass]} state
        fig' (move-down* figure)
        has-collision (collide? fig' glass)]
    (if has-collision
      (-> state
          (persist-figure)
          (remove-filled-rows)
          (squash-rows)
          (pick-next-figure))
      (-> state
          (assoc-in [:figure] fig')))))


(defn draw-segment [x y color]
  (q/rect-mode :corner)
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
  (q/stroke 220)
  (let [border-width 2
        x (+ field-width-px border-width)]
    (q/line x 0 x field-height-px)
    (q/stroke-weight 0)
    (q/fill 235)
    (q/rect (+ border-width x) 0
            (+ border-width x) (q/width))))


(defn draw-next-figure [fig]
  (let [{matrix :shape color :color} fig
        info-panel-mid-px (+ field-width-px (/ info-panel-width 2))
        box-x (- info-panel-mid-px
                 (/ (* segment-size (figure-width fig)) 2))
        box-y next-figure-y-position
        x-inc (Math/abs (min-fig-x fig))
        coords (map (fn [[x-rel y-rel]]
                      [(* segment-size (+ x-rel x-inc))
                       (* segment-size y-rel)]) matrix)]
    (q/rect-mode :corner)
    (apply q/fill color)
    (q/stroke-weight 2)
    (q/stroke 255)
    (q/with-translation [box-x box-y]
      (doseq [[xp yp] coords]
        (q/rect xp yp segment-size segment-size)))))


(defn draw-background []
  (q/stroke-weight 2)
  (q/stroke 200)
  (q/point segment-size segment-size)
  (doseq [x (range segment-size field-width-px segment-size)
          y (range segment-size field-height-px segment-size)]
    (q/point x y)))


(defn draw-help-text []
  (q/fill 140)
  (q/rect-mode :center)
  (q/with-translation help-text-position
    (q/text "W - rotate" 0 0)
    (q/text "A - left" 0 20)
    (q/text "D - right" 0 40)
    (q/text "S - down" 0 60)
    (q/text "Space - pause" 0 80)))


(defn initial-state []
  {:next-figure (new-figure)
   :figure (to-start-position (new-figure))
   :speed 1
   :glass (sorted-map)
   :run false
   :tick max-tick})


(initial-state)


(defn setup []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (initial-state))


(defn settings []
  (q/pixel-density (q/display-density)))


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
        "w" (update-in state [:figure] rotate glass)
        "a" (update-in state [:figure] move-left glass)
        "d" (update-in state [:figure] move-right glass)
        "s" (update-in state [:figure] move-down glass)
        " " (assoc-in state [:run] false)
        state)
      (case event-key
        " " (assoc-in state [:run] true)
        state))))


(defn draw [state]
  (q/background 255)
  (draw-info-panel)
  (draw-background)
  (draw-help-text)
  (let [{:keys [figure next-figure glass]} state]
    (draw-figure figure)
    (draw-next-figure next-figure)
    (draw-glass glass)))


(q/defsketch tetris
  :title "Tetris"
  :size [(+ field-width-px info-panel-width) field-height-px]
  :setup setup
  :update update-state
  :draw draw
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode]
  :key-typed on-key-typed
  :settings settings)


(defn get-state []
  (applet/with-applet tetris
    (q/state)))


(defn inc-speed []
  (applet/with-applet tetris
    (swap! (q/state-atom) update-in [:speed] inc)))


(defn dec-speed []
  (applet/with-applet tetris
    (swap! (q/state-atom) update-in [:speed] dec)))


(comment
  (get-state)

  (inc-speed)

  (dec-speed)

  (applet/with-applet tetris
    (q/exit))
  )
