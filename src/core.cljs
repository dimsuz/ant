(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(def field-rect {:x 10 :y 10 :w 500 :h 500})
(def cell-count 5)

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
        (canvas/fill-style ctx "#0f0")
        (canvas/fill-rect ctx (cell-rect x y)))
      (canvas/stroke-rect ctx (cell-rect x y)))))

(defn next-cell [[x y]]
  (let [xn (inc x)]
    (if (< xn cell-count)
      [xn y]
      (if (< (inc y) cell-count) [0 (inc y)] [0 0]))))

(defn update-field [state]
  (swap! state assoc :active (next-cell (:active @state)))
  state
  )

;;; fixme use core.async
(defn update [state]
  (js/setTimeout (fn []
                   (update-field state)
                   (update state))
                 300)
  )
(defn main []
  (let [mc (create-canvas)
        state (atom {:active [0 0]})]
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
