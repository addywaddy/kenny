(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [alandipert.storage-atom :refer [local-storage load-local-storage]]
            [cljs.core.async :as async :refer[<! chan sliding-buffer put! close! timeout]])

  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn console-log [obj]
  (.log js/console (clj->js obj)))

(defn parse-int [str]

  (.parseInt js/window str)
  )

(def grid-content [
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[9] [0] [0] [2] [2] [3] [4] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0] [0]]
                   [[1] [0] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1] [1]]
                   ])

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn contains-hero [row]
  (some #{[9]} row)
  )

(defn is-hero [cell]
  (= cell [9]))

(defn hero-start-position [grid-content]
  (let [row-index (first (indices contains-hero (reverse grid-content)))
        row (get (vec (reverse grid-content)) row-index)
        col (first (indices is-hero row))]

    {:bottom (* row-index 70) :left (* col 70)}
    )
  )

(def default-hero
  {:dx 0
   :dy 0
   :life 100
   :position (hero-start-position grid-content)
   :game-won false
   }
  )

(def default-data {
                   :hero default-hero
                   :design-game false
                   :grid grid-content
                   :settings {:bounce [3]
                              :red-trampete [5]
                              :blue-trampete [3]
                              }})

(def app-state (local-storage (atom default-data) "game"))

(defn ceil [i]
  (.ceil js/Math i))

(defn floor [i]
  (.floor js/Math i))

(defn hero-position [app]
  #js {:bottom (get-in app [:hero :position :bottom])
       :left (get-in app [:hero :position :left])
       :display (if (get-in app [:design-game]) "none" "block")
       :z-index 10})

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

(defn on-block? [position, block-no]
  (let [[left right] (hero-feet-coords position)
        app @app-state
        below-left (update-in left [0] + 1)
        below-right (update-in right [0] + 1)]
    (some #{[block-no]} [(get-in app (into [:grid] left)) (get-in app (into [:grid] right))]
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

(defn move-left [hero]
  (let [left-position (get-in hero [:position :left])]
    (if (moving? hero)
      (if (> 0 left-position)
        (update-in hero [:position :left] (fn [_] 0))
        (update-in hero [:position :left] (fn [old-left] (+ old-left (hero :dx)))))
      hero
      )
    ))

(defn move-right [hero]
  (let [left-position (get-in hero [:position :left])]
    (if (moving? hero)
      (if (> left-position 1340)
        (update-in hero [:position :left] (fn [_] 1340))
        (update-in hero [:position :left] (fn [old-left] (+ old-left (hero :dx)))))
      hero
      )
    ))

(defn on-red-trampete? [red-trampete hero]
  (if (on-block? (get-in hero [:position]) 3)
    (update-in hero [:dy] (fn [old-dy] (+ old-dy (-> red-trampete first))))
    hero
    ))

(defn on-blue-trampete? [blue-trampete hero]
  (if (on-block? (get-in hero [:position]) 4)
    (update-in hero [:dy] (fn [old-dy] (+ old-dy (-> blue-trampete first))))
    hero
    ))

(defn spike-damage [hero]
  (if (on-block? (get-in hero [:position]) 2)
    (update-in hero [:life] - 1)
    hero
    ))

(defn bouncing [bounce hero]
  (update-in hero [:position :bottom] (fn [old-bottom] (+ old-bottom (-> bounce first))))
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

(defn home? [original-hero hero]
  (if (on-block? (get-in original-hero [:position]) 5)
    (merge original-hero {:game-won true})
    hero
    )
  )

(defn game-over? [original-hero hero]
  (if (or (> 0 (get-in hero [:position :bottom]))
          (> 0 (get-in hero [:life])))
    (merge original-hero {:life 0})
    hero
    )
  )

(defn hero [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (console-log "mounting...")
      (go-loop []
        (<! (timeout 40))
        (let [original-hero (get-in @app [:hero])
              bounce (get-in @app [:settings :bounce])
              red-trampete (get-in @app [:settings :red-trampete])
              blue-trampete (get-in @app [:settings :blue-trampete])
              new-hero (-> original-hero
                           grav
                           ((partial vertical-block original-hero))
                           move-left
                           move-right
                           ((partial bouncing bounce))
                           ((partial on-blue-trampete? blue-trampete))
                           ((partial on-red-trampete? red-trampete))
                           spike-damage
                           ((partial home? original-hero))
                           ((partial game-over? original-hero))
                           )]

          (om/transact! app [:hero] (fn [hero] (merge hero new-hero)))
          )
        (recur)
        )
      )
    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className (hero-classes app) :style (hero-position app)} nil))
    ))

(def id->tile-class
  {0 nil
   1 "ground"
   2 "spikes"
   3 "red-trampete"
   4 "blue-trampete"
   5 "exit"})

(defn editable-input [ctx owner]
  (reify
    om/IRender
    (render [this]
      (dom/span nil
                (dom/input #js {
                                :className ""
                                :onChange (fn [e] (om/transact! ctx [0] (fn [_] (parse-int (.. e -target -value)))))
                                :value (first ctx)
                                }
                           )))))



(defn option-tag [val owner]
  (reify
    om/IRender
    (render [this]
      (dom/option #js {:value (first val)} (first val)))))

(defn select-tag [ctx owner options]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply
       dom/select #js {:value (first ctx)
                       :onChange (fn [e] (om/transact! ctx [0] (fn [_] (parse-int (.. e -target -value)))))}
       (om/build-all option-tag (:options state))))))

(defn cell [ctx owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (if (:design state)
        (dom/div #js {:className "cell"}
                 (dom/span nil
                           (om/build select-tag ctx {:init-state {:options [[0] [1] [2] [3] [4] [9]]}}))
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

(defn table-cell [ctx owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/td nil
              (om/build select-tag ctx {:init-state state})
              )
  )))

(defn table-row [row owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (apply
       dom/tr nil
       (om/build-all table-cell row {:init-state {:options [[0] [1] [2] [3] [4] [9]]}})))))

(defn grid-table [grid owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/table #js {:className "grid-table"}
                 (apply
                  dom/tbody nil
                  (om/build-all table-row grid)
                  )
                            ))
    ))

(defn settings-form [settings]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/table nil
                 (dom/tbody nil
                            (dom/tr nil
                                    (dom/th nil "Red Trampoline")
                                    (om/build table-cell (:red-trampete settings) {:init-state {:options (mapv (fn [i] [i]) (range 1 7))}})
                                    )
                            (dom/tr nil
                                    (dom/th nil "Blue Trampoline")
                                    (om/build table-cell (:blue-trampete settings) {:init-state {:options (mapv (fn [i] [i]) (range 1 7))}})
                                    )
                            )

                 )
      ))
  )

(defn design-view [app owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (dom/div nil
               (dom/h2 nil "Der Raster")
               (om/build grid-table (:grid app))
               (dom/h2 nil "Kenny")
               (om/build settings-form (:settings app))
               )
      )))

(defn status-bar [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "status-bar"}
               (dom/span #js {:className "life"} (str "Leben: " (get-in app [:hero :life]) "%"))
               (dom/button #js {:onClick (fn [e] (do
                                                   (om/transact! app [:hero] (fn [_] default-hero))
                                                   (.. js/document (querySelector ".grid") (focus))
                                                   false))
                                }
                           "Neu Starten")
               (dom/button #js {:onClick (fn [e] (do
                                                   (om/transact! app :design-game (fn [bool] (not bool)))
                                                   (.. js/document (querySelector ".grid") (focus))
                                                   false
                                                   )
                                           )}
                           (if (app :design-game)
                             "Züruck"
                             "Entwerfen"))
               )))
  )

(defn start-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :dx] 5)
    90 (om/update! app [:hero :dx] -5)
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

(defn add-event-listeners [app]
  #js {:className "grid" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app) (.preventDefault e))}
  )

(defn main []
  (om/root
   (fn [app owner]
     (reify
       om/IRender
       (render [_]
         (dom/div nil
                  (when (>= 0 (get-in app [:hero :life]))
                    (dom/h1 #js {:className "banner lost"} "GAME OVER!"))
                  (when (get-in app [:hero :game-won])
                    (dom/h1 #js {:className "banner won"} "YOU DID IT!")
                    )
                  (om/build status-bar app)
                  (dom/div (if (get-in app [:design-game]) nil (add-event-listeners app))
                           (om/build hero app)
                             (if (get-in app [:design-game])
                               (om/build design-view app)
                               ;;(om/build grid-table (:grid app))
                               (om/build grid (:grid app))
                               )
                             ))
         )))
   app-state
   {:target (. js/document (getElementById "app"))}))
