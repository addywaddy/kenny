(ns kenny.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer[<! chan sliding-buffer put! close! timeout]])

  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn console-log [obj]
  (.log js/console obj))

(def grid-content [
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 9 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 1 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 1 0]
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
                      :hero {:move false
                             :position (hero-start-position grid-content)
                             :jump {:time nil :bottom nil}
                             }

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
        left (position :left)
        hero-feet-offset 20]
    [
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 20) 70))]
     [(- 9 (floor (/ bottom 70))) (floor (/ (+ left 54) 70))]
     ]
    ))


(defn most-supportive-block [position]
  (let [[left right] (hero-feet-coords position)
        app @app-state]
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

(defn new-gravity [current-vertical jump-time app]
  (let [last-bottom (get-in @app [:hero :position :bottom])
        initial-bottom (get-in @app [:hero :jump :bottom])
        dt (/ (- (.getTime (js/Date.)) jump-time) 1000)
        vforce (* 530 dt)
        gforce (/ (* 1000 dt dt) 2)
        gv (/ (* 1000 dt) 2)
        height (+ (- vforce gforce) initial-bottom)
        ]
    (if (< height last-bottom)
      (do
        (om/update! app [:hero :jump :start] nil)
        (* 70 (quot height 70))
        )
      height
      )
    )
  )

(defn vertical-position [app]
  (let [current-vertical (get-in @app [:hero :position :bottom])
        current-block-no (most-supportive-block (beneath-1px (get-in @app [:hero :position])))]
    (if-let [jump-time (get-in @app [:hero :jump :start])]
      (new-gravity current-vertical jump-time app)
      (if (on-solid-ground? (get-in @app [:hero :position]))
        current-vertical
        (- current-vertical 5)
        )
      )
    ))

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

(defn can-move [direction app]
  (let [position (get-in app [:hero :position])]
    (condp = direction
      :left (move-left? app position)
      :right (move-right? app position))
    ))

(defn hero [app owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (om/update! app [:hero :position :bottom] (vertical-position app))
        (when (= :left (get-in @app [:hero :move]))
          (when (can-move :left @app)
            (om/transact! app [:hero :position :left] (fn [old] (- old 5)))))
        (when (= :right (get-in @app [:hero :move]))
          (when (can-move :right @app)
            (om/transact! app [:hero :position :left] (fn [old] (+ old 5)))))
        (<! (timeout (/ 1000 60)))
        (recur)
        ))
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

(defn start-moving [e app]
  (condp = (aget e "keyCode")
    88 (om/update! app [:hero :move] :right)
    90 (om/update! app [:hero :move] :left)
    32 (when (and (on-solid-ground? (get-in @app [:hero :position])) (nil? (get-in @app [:hero :jump :start])))
         (om/update! app [:hero :jump :start] (.getTime (js/Date.)))
         (om/update! app [:hero :jump :bottom] (get-in @app [:hero :position :bottom])))
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
          (dom/div #js {:className "grid" :tabIndex 0 :onKeyUp (fn [e] (stop-moving e app)) :onKeyDown (fn [e] (start-moving e app) (.preventDefault e))}
                   (om/build hero app)
                   (om/build grid (:grid app))
                   ))))
    app-state
    {:target (. js/document (getElementById "app"))}))
