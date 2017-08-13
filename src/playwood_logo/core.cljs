(ns playwood-logo.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

; (println "This text is printed from src/playwood-logo/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom
  {
    :program {
      :mode :stop
      :step 0
      :code [
        [:left]
        [:forward 1]
        [:right]
        [:forward 1]
        [:left]
        [:forward 1]
      ]
    }

    :grid {
      :width 500
      :height 400
      :num-rows 21
      :num-cols 17
    }

    :turtle {
      :x 11
      :y 1
      :dir :S
    }

  }))

  (def turtle-circles
    [
      ; body
      { :cx 0.5 :cy 0.6 :r 0.25 }
      ; head
      { :cx 0.5 :cy 0.25 :r 0.1 }
      ; legs
      { :cx 0.75 :cy 0.38 :r 0.05 }
      { :cx 0.25 :cy 0.38 :r 0.05 }
      { :cx 0.75 :cy 0.83 :r 0.05 }
      { :cx 0.25 :cy 0.83 :r 0.05 }
      ])


  (def dirs [:N :E :S :W])
      
  (defn dir-to-offset [dir]
    (case dir
      :N [0 -1]
      :E [-1 0]
      :S [0 1]
      :W [1 0]
      [0 0]))

  (defn dir-to-rotation [dir]
    (case dir
      :N 0
      :E 90
      :S 180
      :W 270
      0))


(defn play! []
  (swap! app-state assoc-in [:program :mode] :play))

(defn apply-stop [prev-state]
  (-> prev-state
    (assoc-in [:program :mode] :stop)
    (assoc-in [:program :step] 0)))

(defn stop! []
  (swap! app-state apply-stop))


(defn apply-step-code [turtle-state step-code]
  (let [[cmd arg] step-code]
  (case cmd
    :forward
      (let [[dx dy] (dir-to-offset (:dir turtle-state))]
        (-> turtle-state
          (update :x + (* arg dx))
          (update :y + (* arg dy))))
    turtle-state)))


(defn next-step! []
  (swap! app-state
    (fn [prev-state]
      (let [program (:program prev-state)
            num-steps (-> program :code count dec)]
        (if (and
              (= (:mode program) :play)
              (< (:step program) num-steps))
            (-> prev-state
              (update-in [:program :step] inc)
              (update-in [:turtle] apply-step-code
                (-> program :code (nth (-> program :step inc)))))
            (apply-stop prev-state))))))





(defn turtle [{:keys [x y dir]}]
  [:g
    {:transform
      (str "translate(" x "," y ")" "rotate(" (dir-to-rotation dir) ")")
    }
    (for [c turtle-circles]
        [:circle (merge c  {
          :key (-> c vals str)
          :stroke "#161"
          :fill "#7a7"
          :stroke-width 0.05
          })])
    ])

(defn cell [{:keys [x y]}]
  [:rect {
    :x x
    :y y
    :width 1 :height 1
    :stroke-width 0.025
    :stroke "#fff"
    :fill "#cee"
    }])


(defn board []
  (let [grid (:grid @app-state)
        width (:width grid)
        height (:height grid)
        num-rows (:num-rows grid)
        num-cols (:num-cols grid)]
    [:svg {:width width
           :height height}
           [:g {:transform
             (str "scale(" (/ width num-rows) "," (/ height num-cols) ")")}
            (for [x (range num-rows)]
            (for [y (range num-cols)]
              [cell {:key (str x ":" y) :x x :y y}]
            ))
            [turtle (:turtle @app-state)]
          ]
       ]))


(defn controls []
  [:div {:style {
     :display "flex" :flex-direction "column" :align-items "center"
     :width 200
    }}
    [:div {:style {:padding 5}}
      (case (-> @app-state :program :mode)
        :play [:button {:on-click stop! } "Stop"]
        :stop [:button {:on-click play! } "Play"])]
    [:textarea {
      :rows 20 :cols 20
      :style { :font-family "monospace" }
      :value (-> @app-state :program :code (#(apply str %)))} ]

    [:div  "Step: " (-> @app-state :program :step)]
  ])


(defn world []
  [:div {:style { :display "flex" :flex-direction "row" }}
  ;  [:h1 "Playwood Logo"]
     [board]
     [controls]
   ])


(js/setInterval next-step! 100)

(reagent/render-component [world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
