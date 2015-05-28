(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))
(enable-console-print!)

(defn console-log [obj]
  (.log js/console obj))

(def app-state (atom {:text "Hello Chestnut!"
                      :hero {
                             :position {:bottom 211 :left 209}
                             :jump-time nil
                             }

                      :grid [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 5 5 4 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [7 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 8]
                             [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                             ]}))

(defn hero-position [app]
  #js {:bottom (get-in app [:hero :position :bottom])
       :left (get-in app [:hero :position :left])})

(def g 2000)
(def v 1000)

(defn transpose [m]
  (apply mapv vector m))

(defn floor [i]
  (.floor js/Math i))

(defn ceil [i]
  (.ceil js/Math i))

(defn floor-coord [px]
  (floor (/ px 70)))

(defn ceil-coord [px]
  (ceil (/ px 70)))

(defn hero-coords [app]
  (let [left-pos (get-in @app [:hero :position :left])
        bottom-pos (get-in @app [:hero :position :bottom])]
    [(ceil-coord left-pos), (floor-coord bottom-pos)]))

(defn current-standing-height []
  70)

(defn gravity [app]
  (let [dy (get-in @app [:hero :position :bottom])
        t (get-in @app [:hero :jump-time])
        dt (/ (- (.getTime (js/Date.)) t) 1000)
        vforce (* v dt)
        gforce (/ (* g dt dt) 2)
        gv (/ (* g dt) 2)
        height (+ (- vforce gforce) (current-standing-height))
        ]
    (if (> 70 height)
      70
      height
      ))
  )

(defn hero [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
       (fn []
         (when (get-in @app [:hero :jump-time])
           (let [height (gravity app)]
             (when (= height 70)
               (om/update! app [:hero :jump-time] nil))
             (om/update! app [:hero :position :bottom] height)
             ))



         ) (/ 1000 60)))
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "hero" :style (hero-position app)} nil))
      ))

(def id->tile-class
  {0 nil
   1 "ground"
   2 "bridge"
   3 "fence"
   4 "lava"
   5 "water"
   6 "spikes"
   7 "start"
   8 "exit"})

(defn cell [cell owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className (str "cell " (id->tile-class cell))} ""))))

(defn row [row owner]
  (reify
    om/IRender
    (render [this]
      (apply
       dom/div #js {:className "row"}
       (om/build-all cell row)))))

(defn grid [grid owner]
  (reify
    om/IRender
    (render [this]
      (apply
       dom/div nil
       (om/build-all row grid)))))

(defn move-hero! [e app]
  (console-log (get-in @app [:hero :jump-time]))
  (condp = (aget e "keyCode")
    88 (om/transact! app [:hero :position :left] (fn [i] (+ i 10)))
    90 (om/transact! app [:hero :position :left] (fn [i] (- i 10)))
    32 (when-not (get-in @app [:hero :jump-time])
         (om/update! app [:hero :jump-time] (.getTime (js/Date.))))
    nil
    )

  )

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div #js {:className "grid" :tabIndex 0 :onKeyDown (fn [e] (move-hero! e app))}
                   (om/build hero app)
                   (om/build grid (:grid app))))))
    app-state
    {:target (. js/document (getElementById "app"))}))
