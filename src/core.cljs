(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(def field-rect {:x 10 :y 10 :w 500 :h 500})
(def cell-count 5)
(defn create-image []
  (let [img (js/Image.)]
    (set! (.-src img) "ant.png")
    img))
(def ant-img (create-image))

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

(defn draw-field [ctx state]
  (canvas/stroke-width ctx 0.5)
  (doseq [y (range 0 cell-count)]
    (doseq [x (range 0 cell-count)]
      (when (= [x y] (:active @state))
        (let [{rx :x ry :y w :w h :h :as r} (cell-rect x y)]
          (canvas/save ctx)
          (canvas/translate ctx (+ rx (/ w 2)) (+ ry (/ h 2)))
          (canvas/rotate ctx (/ (* (:rot @state) (.-PI js/Math)) 180))
          (canvas/translate ctx (- 0 (/ w 2)) (- 0 (/ h 2)))
          (canvas/draw-image ctx ant-img {:x 0 :y 0 :w w :h h})
          (canvas/restore ctx)))
      (canvas/stroke-rect ctx (cell-rect x y)))))

(defn next-cell [[x y]]
  (let [xn (inc x)]
    (if (< xn cell-count)
      [xn y]
      (if (< (inc y) cell-count) [0 (inc y)] [0 0]))))

(defn update-field [state]
  ;(swap! state assoc :active (next-cell (:active @state)))
  (swap! state assoc :rot (+ (:rot @state) 10))
  state
  )

;;; fixme use core.async
(defn update [state]
  (js/setTimeout (fn []
                   (update-field state)
                   (update state))
                 30)
  )
(defn main []
  (let [mc (create-canvas)
        state (atom {:active [2 2] :rot 0.0})]
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
