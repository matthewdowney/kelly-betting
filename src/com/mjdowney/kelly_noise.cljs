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
;;  - Non-random plot: all the wins then all the losses, the reverse, then
;;    interspersed.
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
;;
;; It'd be cool to also be able to simulate some mix of bets. E.g. define some
;; category of bets as 'high variance' and assume a portfolio of bets where 25%
;; are high variance and the rest are low variance.
;;
;; One difficulty with all of this is that there are so many degrees of freedom
;; and things one might want to simulate, so it's hard to encapsulate it in a
;; UI. I wonder if there could be presets + user-provided functions for (1)
;; building the series of bets presented to the agent, and then (2) making
;; decisions for the agent. This would be pretty robust.
(ns com.mjdowney.kelly-noise
  (:require [com.mjdowney.kelly.plotly :as plotly]
            [com.mjdowney.kelly.leva :refer [leva-sync]]
            [goog.functions :as gfn]
            [leva.core :as leva]
            ["seedrandom" :as seedrandom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [taoensso.encore :as enc]))

;;; Controls for the simulation

(defn recompute-ev [x k v]
  (let [{:keys [p-win win-frac loss-frac] :as x} (assoc x k v)]
    (assoc x :ev (+ (* p-win win-frac) (* (- 1 p-win) (- 1 loss-frac))))))

(defn recompute-win-frac [x k v]
  (let [{:keys [p-win ev loss-frac] :as x} (assoc x k v)]
    (assoc x :win-frac (/ (- ev (* (- 1 p-win) (- 1 loss-frac))) p-win))))

(defonce territory
  (r/atom
    (recompute-ev
      {:p-win     0.50
       :ev        1.1
       :loss-frac 1.0
       :win-frac  2.2}
      :win-frac 2.2)))

(defonce simulation
  (r/atom
    {:portfolios 100
     :bets       100
     :bet-size   0.25}))

(defonce view
  (r/atom
    {:width      800
     :height     500
     :y-axis-log true}))

(defn territory-controls []
  (let [!territory (leva-sync territory
                     {:loss-frac #(recompute-ev %1 :loss-frac %2)
                      :win-frac  #(recompute-ev %1 :win-frac %2)
                      :p-win     #(recompute-ev %1 :p-win %2)
                      :ev        #(recompute-win-frac %1 :ev %2)})]
    (fn []
      [leva/Controls
       {:folder {:name "Territory" :settings {:order 0}}
        :atom   !territory
        :schema {:p-win     {:label "P(win)" :min 0 :max 1 :step 0.01}
                 :loss-frac {:label "Loss frac" :min 0 :max 1 :step 0.01}
                 :win-frac  {:label "Win frac"}
                 :ev        {:label "EV" :step 0.01}}}])))

(defn controls
  "Float a Leva component on the page with controls for the simulation."
  []
  (let [t @territory]
    [:div {:title t}
     [territory-controls]

     [leva/Controls
      {:folder {:name "Simulation" :settings {:order 1}}
       :atom   simulation
       :schema {:portfolios {:label "Portfolios"}
                :bets       {:label "Bets"}
                :bet-size   {:label "Bet size" :min 0 :max 1 :step 0.01}}}]

     [leva/Controls
      {:folder {:name "View" :settings {:order 2 :collapsed true}}
       :atom   view}]]))

;;; Simulation code

(defn build-bets
  "Build a series of `n` betting opportunities according to the specified
  territory."
  [territory n]
  (into [] (repeat n territory)))

(defn simulate-portfolio
  "Simulate a portfolio with the given behavior over some series of bets.

  Returns a sequence of bankroll values for each bet."
  [{:keys [bet-size] :as behavior} bankroll bets random-number-generator]
  (lazy-seq
    (when-let [{:keys [p-win win-frac loss-frac]} (first bets)]
      (if (zero? bankroll)
        (repeat (count bets) 0.0)
        (let [stake (* bet-size bankroll)
              outcome (if (< (random-number-generator) p-win)
                        (+ bankroll (* (- win-frac 1) stake))
                        (- bankroll (* loss-frac stake)))]
          (cons outcome
            (simulate-portfolio
              behavior
              (if (< outcome 0.0001) 0.0 outcome)
              (subvec bets 1)
              random-number-generator)))))))

(defn recompute-sim-data [{:keys [portfolios bets] :as sim} territory]
  (let [rng (seedrandom 1)
        bet-terms (build-bets territory bets)]
    (mapv
      (fn [_]
        {:y          (into [] (simulate-portfolio sim 1.0 bet-terms rng))
         :opacity    0.15
         :showlegend false
         :type       :scatter})
      (range portfolios))))

;; TODO: Perhaps re-frame or rendering one portfolio at a time would improve the
;;       responsiveness here.
(defn simulation-plot []
  (let [data (r/atom [])
        cache (atom [])
        recompute (gfn/debounce
                    (fn [sim territory]
                      (when-not (= @cache [sim territory])
                        (reset! cache [sim territory])
                        (reset! data (recompute-sim-data sim territory))))
                    20)]
    (fn []
      (recompute @simulation @territory)
      [plotly/plotly
       {:data   @data
        :layout {:title  "Portfolios"
                 :xaxis  {:title "Bet #"}
                 :yaxis  {:title "Bankroll"
                          :type  (if (:y-axis-log @view) "log" "linear")}
                 :width  (:width @view)
                 :height (:height @view)}}])))

(defn app []
  [:div
   [controls]
   [simulation-plot]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
