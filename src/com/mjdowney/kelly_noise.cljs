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
  (:require [com.mjdowney.kelly.plotly :as plotly]
            [leva.core :as leva]
            [reagent.core :as r]
            ["seedrandom" :as seedrandom]
            [reagent.dom :as rdom]
            [taoensso.encore :as enc]))

;;; Controls for the simulation

(defonce territory
  (r/atom
    {:p-win 0.50
     :ev 1.1
     :loss 1.0}))

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
     :schema {:p-win {:label "P(win)" :min 0 :max 1 :step 0.01}
              :ev {:label "EV" :min 0 :max 10 :step 0.1}
              :loss {:label "Loss %" :min 0 :max 1 :step 0.01}}}]
   [leva/Controls
    {:folder {:name "Simulation" :settings {:order 1}}
     :atom simulation
     :schema {:portfolios {:label "Portfolios" :min 1 :max 1000 :step 1}
              :bets {:label "Bets" :min 1 :max 1000 :step 1}
              :bet-size {:label "Bet size" :min 0 :max 1 :step 0.01}}}]
   [leva/Controls
    {:folder {:name "View" :settings {:order 2 :collapsed true}}
     :atom view}]])

;;; Simulation code

(defn build-bet [{:keys [p-win ev loss]}]
  (let [p-lose (/ (- 100 (long (* p-win 100))) 100.0)
        gain (/ (- ev (* p-lose loss)) p-win)]
    {:p-win p-win
     :gain  (enc/round* :round 4 gain)
     :loss loss
     :p-lose p-lose
     :ev ev}))

(defn build-bets
  "Build a series of `n` betting opportunities according to the specified
  territory."
  [territory n]
  (into [] (repeat n (build-bet territory))))

(defn simulate-portfolio
  "Simulate a portfolio with the given behavior over some series of bets.

  Returns a sequence of bankroll values for each bet."
  [{:keys [bet-size] :as behavior} bankroll bets random-number-generator]
  (lazy-seq
    (when-let [{:keys [p-win gain loss p-lose]} (first bets)]
      (if (zero? bankroll)
        (repeat (count bets) 0.0)
        (let [stake (* bet-size bankroll)
              outcome (if (< (random-number-generator) p-win)
                        (+ bankroll (* gain stake))
                        (- bankroll (* loss stake)))]
          (cons outcome
            (simulate-portfolio
              behavior
              (if (< outcome 0.0001) 0.0 outcome)
              (subvec bets 1)
              random-number-generator)))))))

(defn app []
  (let [{:keys [portfolios bets] :as sim} @simulation
        bet-terms (build-bets @territory bets)
        rng (seedrandom 1)]
    [:div
     [controls]
     [plotly/plotly
      {:data   (mapv ; simulate 100 portfolios
                 (fn [_]
                   {:y          (into []
                                  (simulate-portfolio sim 1.0 bet-terms rng))
                    :opacity    0.15
                    :showlegend false
                    :type       :scatter})
                 (range portfolios))
       :layout {:title  "Portfolios"
                :xaxis {:title "Bet #"}
                :yaxis {:title "Bankroll"
                        :type (if (:y-axis-log @view) "log" "linear")}
                :width  (:width @view)
                :height (:height @view)}}]]))

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
