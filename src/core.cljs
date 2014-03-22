(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(def field-rect {:x 10 :y 10 :w 500 :h 500})
(def cell-count 55)
(defn create-image []
  (let [img (js/Image.)]
    (set! (.-src img) "ant.png")
    img))
(def ant-img (create-image))

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
    (canvas/restore ctx)))

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

(defn step-ant [{:keys [rot field pos] :as state}]
  (let [rot-fn ((get-in field pos) turn-fn-map)
        new-rot (rot-fn rot)]
    (assoc state
      :field (flip-color pos field)
      :pos (move-forward pos new-rot)
      :rot new-rot))
  )

(defn update-field [state]
  (swap! state step-ant)
  ;(swap! state assoc :rot (+ (:rot @state) 10))
  state
  )

;;; fixme use core.async
(defn update [state]
  (js/setTimeout (fn []
                   (update-field state)
                   (update state))
                 3)
  )
(defn main []
  (let [mc (create-canvas)
        init-pos [(int (/ cell-count 2)) (int (/ cell-count 2))]
        state (atom {:pos init-pos :rot 270 :field (create-field cell-count)})]
    (canvas/add-entity mc :background
                       (canvas/entity field-rect
                                      nil                       ; update function
                                      (fn [ctx val]
                                        (-> ctx
                                            (canvas/fill-style "#ded")
                                            (canvas/fill-rect val)))))
    (canvas/add-entity mc :field
                       (canvas/entity state
                                      nil
                                      draw-field))
    (update state)
    ))
