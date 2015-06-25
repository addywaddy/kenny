(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer[<! chan sliding-buffer put! close! timeout]])

  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn console-log [obj]
  (.log js/console (clj->js obj)))

(defn noprintln [obj]
  obj)

(def grid-content [
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
           [0 0 0 0 1 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [9 1 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 1 0]
           [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
           ])

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn contains-hero [row]
  (some #{9} row)
  )

(defn is-hero [cell]
  (= cell 9))

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
                      :design-game false
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

(defn vertical-coord [position]
  (let [bottom (position :bottom)]
     (floor (/ bottom 70))
     ))

(defn horizontal-coords [position]
  (let [left (position :left)]
    {
     :left (floor (/ (+ left 20) 70))
     :right (floor (/ (+ left 54) 70))
     }

    ))

(defn blocks [position]
  (let [[left right] (hero-feet-coords position)
        app @app-state]
    [(get-in app (into [:grid] left)) (get-in app (into [:grid] right))]
          ))

(defn most-supportive-block [position]
  (let [[left right] (hero-feet-coords position)
        app @app-state]
    (some #{1} [(get-in app (into [:grid] left)) (get-in app (into [:grid] right))]
         )))

(defn on-supporting-block? [position]
  (let [[left right] (hero-feet-coords position)
        app @app-state
        below-left (update-in left [0] + 1)
        below-right (update-in right [0] + 1)]
    (some #{1} [(get-in app (into [:grid] left)) (get-in app (into [:grid] right))]
          )))

(defn beneath-1px [position]
  (let [{:keys [left bottom]} position]
    {:left left :bottom (- bottom 5)}
    ))

(defn left-1px [position]
  (let [{:keys [left bottom]} position]
    {:left (- left 1) :bottom bottom}
    ))

(defn right-1px [position]
  (let [{:keys [left bottom]} position]
    {:left (+ left 1) :bottom bottom}
    ))

(defn on-solid-ground? [position]
  (= 1 (most-supportive-block (beneath-1px position)))
  )

(defn move-left? [app position]
  (not= 1 (get-in app (into [:grid] (first (hero-feet-coords (left-1px position))))))
  )

(defn move-right? [app position]
  (not= 1 (get-in app (into [:grid] (second (hero-feet-coords (right-1px position))))))
  )

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

(defn can-move [direction app]
  (println direction)
  (let [position (get-in app [:hero :position])]
    (condp = direction
      :left (move-left? app position)
      :right (move-right? app position))
    ))

(defn in-lava [app]
  (let [position (get-in app [:hero :position])]
    (some #{4} (blocks position))
    )
  )

(defn move-horizontally [hero]
  (let [left-position (get-in hero [:position :left])]
    (if (moving? hero)
      (update-in hero [:position :left] (fn [old-left] (+ old-left (hero :dx))))
      hero
      )))






(defn block-resistance [hero]
  (let [position (get-in hero [:position])
        coords-x (horizontal-coords position)
        current-tiles (blocks position)]
    (if (some #{1} current-tiles)
      (condp = (moving? hero)
        :left (update-in hero [:position :left] (fn [_] (* (+ (coords-x :left) 1) 70)))

        :right (update-in hero [:position :left] (fn [_] (* (- (coords-x :left) 1) 70))))
      hero
      )
    ))

(defn lava-damage [hero]
  hero)

(defn spike-damage [hero]
  hero)

(defn off-screen [hero]
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

(defn cell [cell owner]
  (reify
    om/IRender
    (render [this]
      (when (= cell 9)
        )
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

(defn status-bar [hero owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (hero :life))))
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
                     (om/build status-bar (:hero app))
                     (dom/div #js {:className "grid" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app) (.preventDefault e))}
                              (om/build hero app)
                              (om/build grid (:grid app))
                              ))
            )
          )))
    app-state
    {:target (. js/document (getElementById "app"))}))
