(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer[<! chan sliding-buffer put! close! timeout]])

  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn console-log [obj]
  (.log js/console (clj->js obj)))

(def grid-content [
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [1] [0] [0] [0] [0] [0] [0] [0] [1] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [1] [1] [1] [1] [1] [0] [1] [1] [1] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
           [[9] [1] [4] [4] [4] [4] [4] [4] [4] [4] [5] [5] [5] [5] [5] [5] [5] [5] [1] [0]]
           [[1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1]]
           ])

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn contains-hero [row]
  (some #{9} row)
  )

(defn is-hero [cell]
  (= cell [9]))

(defn hero-start-position [grid-content]
  (let [row-index (first (indices contains-hero (reverse grid-content)))
        row (get (vec (reverse grid-content)) row-index)
        col (first (indices is-hero row))]
    (println row)
    (println col)
    {:bottom (* row-index 70) :left (* col 70)}
    )
  )

(def app-state (atom {:text "Hello Chestnut!"
                      :hero {:dx 0
                             :dy 0
                             :life 100
                             :position (hero-start-position grid-content)
                             :bounce 10
                             }
                      :game-over false
                      :design-game true
                      :foo ["bar"]
                      :grid grid-content}))

(defn ceil [i]
  (.ceil js/Math i))

(defn floor [i]
  (.floor js/Math i))

(defn hero-position [app]
  #js {:bottom (get-in app [:hero :position :bottom])
       :left (get-in app [:hero :position :left])})

(defn hero-feet-coords [position]
  (let [bottom (position :bottom)
        left (position :left)]
    [
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 20) 70))]
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 54) 70))]
     ]
    ))

(defn on-supporting-block? [position]
  (let [[left right] (hero-feet-coords position)
        app @app-state
        below-left (update-in left [0] + 1)
        below-right (update-in right [0] + 1)]
    (some #{[1]} [(get-in app (into [:grid] left)) (get-in app (into [:grid] right))]
          )))

(defn moving? [hero]
  (not= 0 (hero :dx))
  )

(defn step-class [app]
  (let [left-offset (get-in app [:hero :position :left])]
    (rem (ceil (/ left-offset 15)) 3)
    ))

(defn hero-classes [app]
  (let [
        left-class (when (> 0 (get-in app [:hero :dx]))
          "left "
          )]
    (if (moving? (app :hero))
      (str "hero " left-class "step-" (step-class app))
      (str "hero " left-class)
      )
    ))

(defn move-horizontally [hero]
  (let [left-position (get-in hero [:position :left])]
    (if (moving? hero)
      (update-in hero [:position :left] (fn [old-left] (+ old-left (hero :dx))))
      hero
      )))

(defn lava-damage [hero]
  hero)

(defn spike-damage [hero]
  hero)

(defn bounce [hero]
  (update-in hero [:position :bottom] + (get-in hero [:bounce]))
  )

(defn grav [hero]
  (let [new-hero (update-in hero [:dy] - 0.75)]
    (update-in new-hero [:position :bottom] + (new-hero :dy))
    )
  )

(defn vertical-block [original-hero new-hero]
  (let [original-bottom (get-in original-hero [:position :bottom])
        supported (on-supporting-block? (update-in (original-hero :position) [:bottom] - 1))]
    (if supported
      (let [nearest-vertical-border (* (ceil (/ original-bottom 70)) 70)
            newer-hero (update-in new-hero [:position :bottom] (fn [_] nearest-vertical-border))]
        (update-in newer-hero [:dy] (fn [_] 0))
        )
      new-hero
        )
      ))

(defn logger [key hero]
  ;;(console-log {key (get-in hero [:position :left])})
  hero
  )

(defn hero [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (console-log "mounting...")
      (if (>= 0 (get-in @app [:hero :life]))
        (om/update! app [:game-over] true)
        (go-loop []
          (<! (timeout 30))

          (let [original-hero (get-in @app [:hero])
                new-hero (-> original-hero
                             grav
                             ((partial vertical-block original-hero))
                             move-horizontally
                             bounce
                             )]

            (om/transact! app [:hero] (fn [hero] (merge hero new-hero)))
            )
          (recur)
          )
        )
      )
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

(defn editable-input [ctx owner]
  (reify
    om/IRender
    (render [this]
      (dom/span nil
                (dom/input #js {
                                :className ""
                                :onChange (fn [e] (om/transact! ctx [0] (fn [_] (.. e -target -value))))
                                :value (first ctx)
                                }
                           )))))

(defn cell [ctx owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (if (:design state)
        (dom/div #js {:className "cell"}
                 (dom/span nil
                           (om/build editable-input ctx))
                 )
        (dom/div #js {:className (str "cell " (id->tile-class (first ctx)))} ""))))
        )

(defn row [row owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply
       dom/div #js {:className "row"}
       (om/build-all cell row {:init-state state})))))

(defn grid [grid owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply
       dom/div nil
       (om/build-all row grid {:init-state state})))))

(defn status-bar [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
               (dom/h1 nil (get-in app [:hero :life]))
               (dom/button #js {:onClick (fn [e] (om/transact! app :design-game (fn [bool] (not bool))) false)}
                 (if (app :design-game)
                   "Play"
                   "Design"))
               )))
  )

(defn start-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :dx] 10)
    90 (om/update! app [:hero :dx] -10)
    nil
    )
  )

(defn stop-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :dx] 0)
    90 (om/update! app [:hero :dx] 0)
    nil
    )
  )

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (if (>= 0 (get-in app [:hero :life]))
            (dom/h1 nil "GAME OVER")
            (dom/div nil
                     (om/build status-bar app)
                     (if (get-in app [:design-game])
                       (om/build editable-input (:foo app))
                       (dom/div #js {:className "grid on-top" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app) (.preventDefault e))}
                                (om/build grid (:grid app) {:init-state {:design true}})
                                )

                       (dom/div #js {:className "grid" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app) (.preventDefault e))}
                                (om/build hero app)
                                (om/build grid (:grid app))
                                )))
            )
          )))
    app-state
    {:target (. js/document (getElementById "app"))}))
