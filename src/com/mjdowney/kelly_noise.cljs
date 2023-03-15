;; Things to simulate:
;;  - Territory
;;   - Probability per bet
;;   - Payoff per bet
;;  - Map
;;   - Noise around perception (both in absolute and percentage terms)
;;  - Bet size, number of portfolios, and number of bets
;; Views
;;  - Plot of portfolio value over time (+ median portfolio!)
;;  - Histogram of terminal values
;;
;;    ***
;;  - Comparisons? So there's a button to take a snapshot to save a comparison
;;    ***
;;
;;  - 3D plot of ... variance, noise, and kelly fraction?
;;
;;  - Non-random plot: all the wins then all the losses, the reverse, then
;;    interspersed.
;; Functionality:
;;  - Parameters are dynamic
;;  - Generalized plot of A vs B holding all else constant: e.g. bet size vs
;;    median terminal value
;;
;; Questions to answer:
;;  - For this territory, how does noise impact optimal bet size? (I.e. optimal
;;    bet size on the x-axis, noise on the y-axis)
;;  - Ditto for risk of ruin
;;  - What % of portfolios lose money for some bet size?
;;  - What is the bet size with the highest median return where < x% of
;;    portfolios lose money?
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
  (:require [com.mjdowney.kelly.leva :refer [leva-sync]]
            [com.mjdowney.kelly.plotly :as plotly]
            [goog.functions :as gfn]
            [leva.core :as leva]
            ["seedrandom" :as seedrandom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [taoensso.encore :as enc]))

;;; Controls for the simulation via leva-cljs

;; TODO: Certain values still cause Leva to go into an update loop
(defn recompute-ev
  ([x]
   (recompute-ev x :p-win (:p-win x)))
  ([x k v]
   (let [{:keys [p-win win-frac loss-frac] :as x} (assoc x k v)]
     (assoc x :ev (+ (* p-win win-frac) (* (- 1 p-win) (- 1 loss-frac)))))))

(defn recompute-win-frac [x k v]
  (let [{:keys [p-win ev loss-frac] :as x} (assoc x k v)]
    (assoc x :win-frac (/ (- ev (* (- 1 p-win) (- 1 loss-frac))) p-win))))

(defonce territory
  (r/atom
    (recompute-ev
      {:p-win     0.50 ; chance of winning the wager
       :win-frac  2.2  ; multiple of bet size returned on win (1 = break even)
       :loss-frac 1.0  ; amount lost on loss (1 = lose entire stake)
       #_#_:ev 1.1}))) ; expected value of the wager, follows from other params

(defonce simulation
  (r/atom
    {:portfolios 100   ; number of portfolios to simulate
     :bets       100   ; number of bets per portfolio
     :bet-size   0.25}))

(defonce plot-settings
  (r/atom
    {:log-axis   true
     :bins       100
     :cumulative false
     :nth-perc   50    ; highlight some percentile to show on plot (50 = median)
     :width      800
     :height     500}))

(defonce view
  (r/atom {:active "portfolios"}))

(def view->plot-settings
  {::all            #{:width :height :log-axis :nth-perc}
   :terminal-values #{:bins :cumulative}})

(def plot-settings-schema
  (letfn [(render? [k]
            (or
              (contains? (::all view->plot-settings) k)
              (let [active (keyword (:active @view))]
                (contains? (view->plot-settings active) k))))]
    (into {}
      (map (juxt identity (fn [k] {:render (partial render? k)})))
      (keys @plot-settings))))

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
     [leva/Controls
      {:folder {:name "View" :settings {:order -1}}
       :atom   view
       :schema {:active {:options {"Portfolios over time" :portfolios
                                   "Terminal values"      :terminal-values}}}}]

     [territory-controls]

     [leva/Controls
      {:folder {:name "Simulation" :settings {:order 1}}
       :atom   simulation
       :schema {:portfolios {:label "Portfolios"}
                :bets       {:label "Bets"}
                :bet-size   {:label "Bet size" :min 0 :max 1 :step 0.01}}}]

     [leva/Controls
      {:folder {:name "Plot settings" :settings {:order 2 :collapsed true}}
       :schema (enc/nested-merge plot-settings-schema
                 {:bins {:min 1 :step 1}
                  :nth-perc {:min 0 :max 100 :step 1}})
       :atom   plot-settings}]]))

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

(defn recompute-sim-data [sequence {:keys [portfolios bets] :as sim} territory]
  (let [rng (seedrandom 1)
        bet-terms (build-bets territory bets)]
    {:portfolios
     (mapv
       (fn [_]
         {:y          (into [] (simulate-portfolio sim 1.0 bet-terms rng))
          :opacity    0.15
          :showlegend false
          :type       :scatter})
       (range portfolios))
     :nth-perc nil
     :sequence (inc sequence)}))

;;; Plotting code

(defn percentile [xs perc]
  (let [index (int (Math/floor (* (dec (count xs)) (/ perc 100))))]
    (nth xs index)))

(defn compute-nth-perc
  "Progressively compute the nth percentile of the bankroll over time, adding
  it into the data atom under :nth-perc, tagging with [:nth-perc :complete]
  true when finished."
  [data perc idx sequence]
  (when (= (:sequence @data) sequence)
    (let [{:keys [portfolios nth-perc]} @data]
      (if (< idx (if-let [p (first portfolios)] (count (:y p)) 0))
        (let [sorted (vec (sort (keep (comp #(nth % idx nil) :y) portfolios)))
              median (percentile sorted perc)
              nth-perc (or nth-perc
                         {:y           []
                          :opacity     1
                          :showledgend true
                          :perc        perc
                          :line        {:width 3}
                          :name        (str "p" perc)})]
          (swap! data assoc :nth-perc
            (update nth-perc :y conj median))
          (if (zero? (mod idx 10))
            (js/window.setTimeout
              (fn []
                (compute-nth-perc data perc (inc idx) sequence)))
            (recur data perc (inc idx) sequence)))
        (swap! data assoc-in [:nth-perc :complete] true)))))

(defn vline [x & {:keys [] :as line}]
  {:type :line :x0 x :y0 0 :x1 x :y1 1 :yref :paper :line line})

(defn top-annotation [x text & {:keys [] :as font}]
  {:x x :y 1.05 :xref :x :yref :paper :showarrow false :text text :font font})

(defn annotate-nth-perc [perc med]
  (let [lbl (str "p" perc " = 10^" (enc/round2 med))]
    {:shapes      [(vline med :width 2 :color "steelblue")]
     :annotations [(top-annotation med lbl :color "steelblue")]}))

(defn histogram
  "Histogram of the final portfolio values."
  [{:keys [portfolios nth-perc] :as data}]
  (let [{:keys [log-axis cumulative bins width height]} @plot-settings
        xf (if log-axis #(Math/log %) identity)
        terminal-values (mapv (comp xf peek :y) portfolios)]
    [plotly/plotly
     {:data
      [{:x          terminal-values
        :nbinsx     bins
        :type       :histogram
        :cumulative {:enabled cumulative}
        :histnorm   :probability
        :opacity    0.55
        :showlegend false}]
      :layout
      (merge
        {:title  "Terminal Values"
         :xaxis  {:title "Log(Return multiple)"}
         :yaxis  (enc/assoc-some
                   {:title (if cumulative "Cumulative %" "Frequency")}
                   :dtick (when cumulative 0.10))
         :width  width
         :height height}
        ; When the nth percentile is known, annotate it on the plot
        (when-let [nth' (when (:complete nth-perc)
                          (xf (peek (:y nth-perc))))]
          (annotate-nth-perc (:perc nth-perc) nth')))}]))

(defn plot [{:keys [portfolios nth-perc] :as data}]
  ; Either plot a histogram or a line chart with each of the simulated
  ; portfolios
  (if (= (:active @view) "terminal-values")
    [histogram data]
    [plotly/plotly
     {:data   (enc/conj-some portfolios nth-perc)
      :layout {:title  "Portfolios"
               :xaxis  {:title "Bet #"}
               :yaxis  {:title "Bankroll"
                        :type  (if (:log-axis @plot-settings) "log" "linear")}
               :width  (:width @plot-settings)
               :height (:height @plot-settings)}}]))

(defn plot-recompute-wrapper
  "Wrap the actual plotly components and debounce the computation of simulation
  / plot data from the settings to make the page more responsive."
  []
  (let [data (r/atom {})
        cache (atom [])
        recompute (gfn/debounce
                    (fn [sim territory perc]
                      (when-not (= @cache [sim territory perc])
                        (reset! cache [sim territory perc])
                        (let [s (:sequence
                                 (swap! data
                                   (fn [{:keys [sequence]}]
                                     (recompute-sim-data sequence sim territory))))]
                          (compute-nth-perc data perc 0 s))))
                    20)]
    (fn []
      (recompute @simulation @territory (:nth-perc @plot-settings))
      [plot @data])))

(defn app []
  [:div
   [controls]
   [plot-recompute-wrapper]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
