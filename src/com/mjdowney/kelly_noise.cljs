;; Things to simulate:
;;  - Territory
;;   - Probability per bet
;;   - Payoff per bet
;;  - Map
;;   - Noise around perception (both in absolute and percentage terms)
;;  - Bet size, number of portfolios, and number of bets
;; Views
;;  - Plot of portfolio value over time
;;  - Histogram of terminal values
;; Functionality:
;;  - Parameters are dynamic
;;  - Generalized plot of A vs B holding all else constant: e.g. bet size vs
;;    median terminal value
;;
;; Separately, a simpler thing that would also be useful and nice to have would
;; be an N-outcome optimal bet size simulator, that allows you to add as many
;; outcomes and payoffs as you want, and then optimize bet size for either
;; median return, or less than X% of portfolios losing money.
;;
;; Use cases to satisfy:
;;  - Simulate a series of low-probability, high-payoff bets
;;  - Simulate a series of high-probability, low-payoff bets
;;  - Simulate a variety of bets
(ns com.mjdowney.kelly-noise
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [leva.core :as leva]
            [com.mjdowney.kelly.plotly :as plotly]))

;;; Controls for the simulation

(defonce territory
  (r/atom
    {:p-win 50
     :ev 1.1}))

(defonce simulation
  (r/atom
    {:portfolios 100
     :bets 100
     :bet-size 0.25}))

(defonce view
  (r/atom
    {:width 800
     :height 500
     :y-axis-log true}))

(defn controls
  "Float a Leva component on the page with controls for the simulation."
  []
  [:<>
   [leva/Controls
    {:folder {:name "Territory" :settings {:order 0}}
     :atom territory
     :schema {:p-win {:label "P(win)" :min 0 :max 100}
              :ev {:label "EV" :min 0 :max 2 :step 0.1}}}]
   [leva/Controls
    {:folder {:name "Simulation" :settings {:order 1}}
     :atom simulation
     :schema {:portfolios {:label "Portfolios" :min 1 :max 1000 :step 1}
              :bets {:label "Bets" :min 1 :max 1000 :step 1}
              :bet-size {:label "Bet size" :min 0 :max 1 :step 0.01}}}]
   [leva/Controls
    {:folder {:name "View" :settings {:order 2 :collapsed true}}
     :atom view}]])

(defn app []
  [:div
   [controls]
   [plotly/plotly
    {:data   [{:x      [1 2 3 4]
               :y      [2 6 3 4]
               :type   "scatter"
               :mode   "lines+markers"
               :marker {:color "red"}}]
     :layout {:title  "A Fancy Plot"
              :width  (:width @view)
              :height (:height @view)}}]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
