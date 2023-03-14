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
(ns com.mjdowney.kelly-noise
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [leva.core :as leva]
            [com.mjdowney.kelly.plotly :as plotly]))

(defonce controls (r/atom {:width 500 :height 500}))

(defn app []
  [:div
   "Hello, world!"
   [leva/Controls {:folder {:name "Plot Controls"} :atom controls}]
   [plotly/plotly
    {:data   [{:x      [1 2 3 4]
               :y      [2 6 3 4]
               :type   "scatter"
               :mode   "lines+markers"
               :marker {:color "red"}}]
     :layout {:title  "A Fancy Plot"
              :width  (:width @controls)
              :height (:height @controls)}}]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
