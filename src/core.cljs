(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(def field-rect {:x 10 :y 10 :w 790 :h 790})
(def cell-count 45)
(defn create-image []
  (let [img (js/Image.)]
    (set! (.-src img) "ant.png")
    img))
(def ant-img (create-image))
(def step-counter-pos
  (let [{x1 :x y1 :y w :w h :h} field-rect
        x2 (+ x1 w)
        y2 (+ y1 h)]
    {:x (- x2 100) :y (- y2 10)}))

(def colors-map {:off "#fff" :on "#888"})

(defn create-field [size]
  (into [] (for [x (range 0 size)]
             (into [] (for [y (range 0 size)]
                        :off)))))

(defn create-canvas []
  (let [canvas-dom (.getElementById js/document "canvas")]
    (if canvas-dom
      (canvas/init canvas-dom "2d")
      (prn "Failed to create canvas!"))))

(defn cell-rect [x y]
  (let [fx (:x field-rect)
        fy (:y field-rect)
        cell-size (/ (:w field-rect) cell-count)]
    {:x (+ fx (* x cell-size))
     :y (+ fy (* y cell-size))
     :w cell-size
     :h cell-size}))

(defn cell-color  [pos field]
  (colors-map (get-in field pos)))

(defn draw-field [ctx state]
  (canvas/stroke-width ctx 0.5)
  (doseq [y (range 0 cell-count)]
    (doseq [x (range 0 cell-count)]
      (canvas/fill-style ctx (cell-color [x y] (:field @state)))
      (canvas/fill-rect ctx (cell-rect x y))
      (canvas/stroke-rect ctx (cell-rect x y))
      ))
  (let [[x y] (:pos @state)
        {rx :x ry :y w :w h :h :as r} (cell-rect x y)]
    (canvas/save ctx)
    (canvas/translate ctx (+ rx (/ w 2)) (+ ry (/ h 2)))
    (canvas/rotate ctx (/ (* (:rot @state) (.-PI js/Math)) 180))
    (canvas/translate ctx (- 0 (/ w 2)) (- 0 (/ h 2)))
    (canvas/draw-image ctx ant-img {:x 0 :y 0 :w w :h h})
    (canvas/restore ctx))
  (canvas/font-style ctx "50px Arial")
  (canvas/fill-style ctx "rgba(0,0,0,0.7")
  (canvas/text ctx (merge {:text (:step @state)} step-counter-pos)))

(defn flip-color [pos field]
  (update-in field pos #(if (= :on %) :off :on)))

(defn turn-right [angle]
  (mod (+ angle 90) 360))
(defn turn-left [angle]
  (mod (- angle 90) 360))

(def turn-fn-map {:off turn-right :on turn-left})

(defn move-forward [pos rot]
  (cond
   (= rot 0) [(first pos) (dec (second pos))]
   (= rot 90) [(inc (first pos)) (second pos)]
   (= rot 180) [(first pos) (inc (second pos))]
   (= rot 270) [(dec (first pos)) (second pos)]))

(defn step-ant [{:keys [rot field pos step] :as state}]
  (let [rot-fn ((get-in field pos) turn-fn-map)
        new-rot (rot-fn rot)]
    (assoc state
      :field (flip-color pos field)
      :pos (move-forward pos new-rot)
      :rot new-rot
      :step (inc step)))
  )

(defn update-field [state]
  (swap! state step-ant)
  ;(swap! state assoc :rot (+ (:rot @state) 10))
  state
  )

(defn draw-grid [ctx]
  (let [{x1 :x y1 :y w :w h :h} field-rect
        x2 (+ x1 w)
        y2 (+ y1 h)]
    (doseq [x (range x1 x2 (int (/ w cell-count)))]
      (canvas/move-to ctx x y1)
      (canvas/line-to ctx x y2))
    (doseq [y (range y1 y2 (int (/ w cell-count)))]
      (canvas/move-to ctx x1 y)
      (canvas/line-to ctx x2 y))
    (canvas/stroke-width ctx 0.25)
    (canvas/stroke-style ctx "#000")
    (canvas/stroke ctx))

  )

(defn main []
  (let [mc (create-canvas)
        init-pos [(int (/ cell-count 2)) (int (/ cell-count 2))]
        state (atom {:pos init-pos :rot 270 :field (create-field cell-count) :step 1})]
    ;; (canvas/add-entity mc :grid (canvas/entity nil nil draw-grid))
    (canvas/add-entity mc :field
                       (canvas/entity state
                                      update-field
                                      draw-field))
    ))
