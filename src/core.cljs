(ns ant.core
  (:require [monet.canvas :as canvas]))

(enable-console-print!)

(def field-rect {:x 0 :y 0 :w 1000 :h 1000})
(def cell-count 120)
(defn create-image []
  (let [img (js/Image.)]
    (set! (.-src img) "ant.png")
    img))
(def ant-img (create-image))
(def step-counter-pos
  (let [{x1 :x y1 :y w :w h :h} field-rect
        x2 (+ x1 w)
        y2 (+ y1 h)]
    {:x (- x2 350) :y (- y2 10)}))

(def colors-map {:grid "#c8c8c8" :c1 "#e6e6fa" :c2 "#656bff"})

(defn create-field [size]
  (into [] (for [x (range 0 size)]
             (into [] (for [y (range 0 size)]
                        :c1)))))

(defn create-canvas []
  (let [canvas-dom (.getElementById js/document "canvas")]
    (if canvas-dom
      (canvas/init canvas-dom "2d")
      (prn "Failed to create canvas!"))))

(defn cell-rect [pos]
  (let [fx (:x field-rect)
        fy (:y field-rect)
        [x y] pos
        cell-size (int (/ (:w field-rect) cell-count))]
    {:x (+ fx (* x cell-size) 0.5)
     :y (+ fy (* y cell-size) 0.5)
     :w cell-size
     :h cell-size}))

(defn cell-color  [pos field]
  (colors-map (get-in field pos)))

(defn positions [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn filled-cells [field]
  (let [row-idxs (positions #(some #{:c2} %) field)]
    (mapcat (fn [row-idx]
              (map (fn [cell-idx] [row-idx cell-idx]) (positions #{:c2} (get field row-idx)))) row-idxs))
  )

(defn draw-field [ctx state]
  (canvas/stroke-width ctx 0.5)
  (doseq [pos (filled-cells (:field @state))]
    (canvas/fill-style ctx (:c2 colors-map))
    (canvas/fill-rect ctx (cell-rect pos))
    )
  (let [pos (:pos @state)
        {rx :x ry :y w :w h :h :as r} (cell-rect pos)]
    (canvas/save ctx)
    (canvas/translate ctx (+ rx (/ w 2)) (+ ry (/ h 2)))
    (canvas/rotate ctx (/ (* (:rot @state) (.-PI js/Math)) 180))
    (canvas/translate ctx (- 0 (/ w 2)) (- 0 (/ h 2)))
    (canvas/draw-image ctx ant-img {:x 0 :y 0 :w w :h h})
    (canvas/restore ctx))
  (canvas/font-style ctx "50px Arial")
  (canvas/fill-style ctx "#000")
  (canvas/text ctx (merge {:text (:step @state)} step-counter-pos)))

(defn flip-color [pos field]
  (update-in field pos #(if (= :c2 %) :c1 :c2)))

(defn turn-right [angle]
  (mod (+ angle 90) 360))
(defn turn-left [angle]
  (mod (- angle 90) 360))

(def turn-fn-map {:c1 turn-right :c2 turn-left})

(defn move-forward [pos rot]
  (cond
   (= rot 0) [(first pos) (dec (second pos))]
   (= rot 90) [(inc (first pos)) (second pos)]
   (= rot 180) [(first pos) (inc (second pos))]
   (= rot 270) [(dec (first pos)) (second pos)]))

(defn out-of-bounds? [pos]
  (or (some neg? pos) (some #(>= % cell-count) pos)))

(defn step-ant [{:keys [rot field pos step] :as state}]
  (let [rot-fn ((get-in field pos) turn-fn-map)
        new-rot (rot-fn rot)
        new-pos (move-forward pos new-rot)
        finished? (out-of-bounds? new-pos)]
    (if finished?
      (assoc state :finished true)
      (assoc state
        :field (flip-color pos field)
        :pos new-pos
        :rot new-rot
        :step (inc step))))
  )

(defn update-field [state]
  (when (not (:finished @state))
    (swap! state step-ant))
  ;(swap! state assoc :rot (+ (:rot @state) 10))
  state
  )

(defn draw-grid [ctx state]
  (when (:render-grid @state)
    (let [{xs :x ys :y w :w h :h} field-rect
          x1 (+ xs 0.5)
          y1 (+ ys 0.5)
          x2 (+ x1 w)
          y2 (+ y1 h)]
      (canvas/begin-path ctx)
      (doseq [x (range x1 x2 (int (/ w cell-count)))]
        (canvas/move-to ctx x y1)
        (canvas/line-to ctx x y2))
      (doseq [y (range y1 y2 (int (/ w cell-count)))]
        (canvas/move-to ctx x1 y)
        (canvas/line-to ctx x2 y))
      (canvas/stroke-width ctx 0.5)
      (canvas/stroke-style ctx (:grid colors-map))
      (canvas/stroke ctx))
    ;(prn "drawing grid")
    ;(swap! state assoc :render-grid false)
    )
  )

(defn ^:export main []
  (let [mc (create-canvas)
        init-pos [(int (/ cell-count 2)) (int (/ cell-count 2))]
        state (atom {:pos init-pos :rot 270 :field (create-field cell-count) :step 1 :render-grid true})]
    (canvas/add-entity mc :field
                       (canvas/entity state
                                      update-field
                                      draw-field))
    (canvas/add-entity mc :grid (canvas/entity state nil draw-grid))
    ))
