(ns playwood-logo.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

; (println "This text is printed from src/playwood-logo/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(def initial-turtle-state
  {
   :x   11
   :y   1
   :dir :S
   })

(defonce app-state
         (atom {
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

                :grid    {
                          :width    400
                          :height   300
                          :num-rows 21
                          :num-cols 17
                          }

                :turtle initial-turtle-state
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

(defn rotate-right [dir]
  (let [idx (.indexOf dirs dir)] (get dirs (inc idx) (first dirs))))

(defn rotate-left [dir]
  (let [idx (.indexOf dirs dir)] (get dirs (dec idx) (last dirs))))

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

(defn reset-turtle! []
  (swap! app-state assoc :turtle initial-turtle-state))

(defn apply-stop [prev-state]
  (-> prev-state
      (assoc-in [:program :mode] :stop)
      (assoc-in [:program :step] 0)))

(defn stop! []
  (swap! app-state apply-stop))


(defn apply-command [turtle-state step-code]
  (let [[cmd arg] step-code]
    (case cmd
      :left
      (-> turtle-state
                  (update :dir rotate-left))
      :right
      (-> turtle-state
                  (update :dir rotate-right))

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
                   (update-in [:turtle] apply-command
                              (-> program :code (nth (-> program :step inc)))))
               (apply-stop prev-state))))))





(defn Turtle [{:keys [x y dir]}]
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

(defn Cell [{:keys [x y]}]
  [:rect {
          :x x
          :y y
          :width 1 :height 1
          :stroke-width 0.025
          :stroke "#fff"
          :fill "#cee"
          }])


(defn Board []
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
          [Cell {:key (str x ":" y) :x x :y y}]
          ))
      [Turtle (:turtle @app-state)]
      ]
     ]))


(defn controls []
  [:div {:style {
                 :display "flex" :flex-direction "column" :align-items "center"
                 }}
   [:div {:style {:padding 5}}
    (case (-> @app-state :program :mode)
      :play [:button {:on-click stop! } "Stop"]
      :stop [:button {:on-click play! } "Play"])
    [:button {:on-click reset-turtle! } "Reset"]]
   [:textarea {
               :style { :font-family "monospace" :width 400 :height 300 }
               :value (-> @app-state :program :code (#(apply str %)))} ]

   [:div  "Step: " (-> @app-state :program :step)]
   ])


(defn world []
  [:div {:style { :display "flex" :flex-direction "column" }}
   [Board]
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
