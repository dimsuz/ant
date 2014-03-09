(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(defn create-canvas []
  (let [canvas-dom (.getElementById js/document "canvas")]
    (if canvas-dom
      (canvas/init canvas-dom "2d")
      (prn "Failed to create canvas!"))))

(defn main []
  (canvas/add-entity (create-canvas) :background
                     (canvas/entity {:x 0 :y 0 :w 300 :h 300} ; val
                                    nil                       ; update function
                                    (fn [ctx val]             ; draw function
                                      (-> ctx
                                          (canvas/fill-style "#ff0")
                                          (canvas/fill-rect val)))))
  )
