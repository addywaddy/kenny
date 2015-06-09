(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))
(enable-console-print!)

(defn console-log [obj]
  (.log js/console obj))

(def app-state (atom {:text "Hello Chestnut!"
                      :hero {:move false
                             :position {:bottom 560 :left 0}
                             :jump false
                             }

                      :grid [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                             [0 0 0 0 5 5 4 4 0 0 0 0 0 0 0 0 0 0 0 0]
                             [1 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 8]
                             [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                             ]}))

(defn ceil [i]
  (.ceil js/Math i))

(defn floor [i]
  (.floor js/Math i))

(defn hero-position [app]
  #js {:bottom (get-in app [:hero :position :bottom])
       :left (get-in app [:hero :position :left])})

(defn hero-feet-coords [app]
  (let [bottom (get-in app [:hero :position :bottom])
        left (get-in app [:hero :position :left])
        hero-feet-offset 20]
    [
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 20) 70))]
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 50) 70))]
     ]
    ))

(defn most-supportive-block [app]
  (let [[left right] (hero-feet-coords app)]
    (max (get-in app (into [:grid] left))
         (get-in app (into [:grid] right))
         )))

(def g 2000)
(def v 1000)

(defn is-solid? [app [grid-x grid-y]]
  (>= (get-in app [:grid (- 9 grid-y) grid-x]) 1)
  )

(defn current-standing-height []
  70)

(def gravitational-force 10)

(defn on-solid-ground? [app]
  true)

(defn vertical-position [app]
  (let [current-vertical (get-in app [:hero :position :bottom])
        current-block-no (most-supportive-block app)]
    (if (>= current-block-no 1)
      current-vertical
      (- current-vertical 1)
  )))

(defn gravity [app]
  (let [dy (get-in @app [:hero :position :bottom])
        t (get-in @app [:hero :jump])
        dt (/ (- (.getTime (js/Date.)) t) 1000)
        vforce (* v dt)
        gforce (/ (* g dt dt) 2)
        gv (/ (* g dt) 2)
        height (+ (- vforce gforce) (current-standing-height))
        ]
    (if (or
         (is-solid? @app (first (hero-feet-coords @app)))
         (is-solid? @app (last (hero-feet-coords @app)))
         )
      (* 70  (last (first (hero-feet-coords @app))))
      70
      height
      ))
  )


(defn step-class [app]
  (let [left-offset (get-in app [:hero :position :left])]
    (rem (ceil (/ left-offset 15)) 3)
    ))

(defn hero-classes [app]
  (let [
        left-class (when (= :left (get-in app [:hero :move]))
          "left "
          )]
    (if (get-in app [:hero :move])
      (str "hero " left-class "step-" (step-class app))
      (str "hero " left-class)
      )
    ))

(defn hero [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (js/setInterval
       (fn []
         (om/update! app [:hero :position :bottom] (vertical-position @app))
         (when (= :left (get-in @app [:hero :move]))
           (om/transact! app [:hero :position :left] (fn [old] (- old 5))))
         (when (= :right (get-in @app [:hero :move]))
           (om/transact! app [:hero :position :left] (fn [old] (+ old 5))))
         ) (/ 1000 60)))
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className (hero-classes app) :style (hero-position app)} nil))
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

(defn start-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :move] :right)
    90 (om/update! app [:hero :move] :left)
    32 (when-not (get-in @app [:hero :jump])
         (om/update! app [:hero :jump] (.getTime (js/Date.))))
    nil
    )
  )

(defn stop-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :move] false)
    90 (om/update! app [:hero :move] false)
    nil
    )
  )

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div #js {:className "grid" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app))}
                   (om/build hero app)
                   (om/build grid (:grid app))))))
    app-state
    {:target (. js/document (getElementById "app"))}))
