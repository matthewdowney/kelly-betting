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

(defn recompute-ev
  ([x]
   (recompute-ev x :p-win (:p-win x)))
  ([x k v]
   (let [{:keys [p-win win-frac loss-frac] :as x} (assoc x k v)]
     (assoc x :ev (+ (* p-win win-frac) (* (- 1 p-win) (- 1 loss-frac)))))))

(defn recompute-win-frac [x k v]
  (let [{:keys [p-win ev loss-frac] :as x} (assoc x k v)]
    (if (zero? p-win)
      (assoc x :win-frac 1.0 :ev (* ev (- 1 loss-frac)))
      (assoc x :win-frac (/ (- ev (* (- 1 p-win) (- 1 loss-frac))) p-win)))))

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
     :width      575
     :height     500}))

(defonce view
  (r/atom
    {:active "portfolios" ; one of #{"portfolios" "terminal-values" "optimize"}
     :optimize "bet-size"
     :target "nth-perc"}))

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

(declare compute-optimizations)
(defn controls
  "Float a Leva component on the page with controls for the simulation."
  []
  (let [t @territory]
    [:div
     [leva/Controls
      {:folder {:name "View" :settings {:order -1}}
       :atom   view
       :schema {:active {:options {"Portfolios over time" :portfolios
                                   "Terminal values"      :terminal-values
                                   "Optimize"             :optimize}}
                :target {:options {"nth percentile return" :nth-perc}
                         :render (fn [] (= "optimize" (:active @view)))}
                :optimize {:options {"Bet size" :bet-size}
                           :render (fn [] (= "optimize" (:active @view)))}
                :compute (leva/button
                           compute-optimizations
                           {:render (fn [] (= "optimize" (:active @view)))})}}]

     [territory-controls]

     [leva/Controls
      {:folder {:name "Simulation" :settings {:order 1}}
       :atom   simulation
       :schema {:portfolios {:label "Portfolios"}
                :bets       {:label "Bets"}
                :bet-size   {:label "Bet size" :min 0 :max 1 :step 0.01}}}]

     [leva/Controls
      {:folder {:name "Plot settings" :settings {:order 2}}
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

(defn run-simulation [{:keys [simulation territory]}]
  (let [rng (seedrandom 1)
        {:keys [portfolios bets]} simulation
        bet-terms (build-bets territory bets)]
    {:portfolios
     (mapv
       (fn [_]
         {:y          (vec (simulate-portfolio simulation 1.0 bet-terms rng))
          :opacity    0.15
          :showlegend false
          :type       :scatter})
       (range portfolios))
     :nth-perc nil}))

(defn percentile [xs perc]
  (let [index (int (Math/floor (* (dec (count xs)) (/ perc 100))))]
    (nth xs index)))

(defmulti inputs-for-optimization identity)
(defmethod inputs-for-optimization "bet-size" [_]
  (let [s @simulation
        t @territory]
    (map
      (juxt #(/ % 100.0)
        (fn [size]
          {:simulation (assoc s :bet-size (/ size 100.0))
           :territory t}))
      (range 1 101))))

(defmulti rewardf-for-target identity)
(defmethod rewardf-for-target "nth-perc" [_]
  (let [nth-perc (get @plot-settings :nth-perc)]
    (fn [{:keys [portfolios]}]
      (percentile
        (vec (sort (map (comp peek :y) portfolios)))
        nth-perc))))

(defn optimization-data
  ([{:keys [target optimize]}]
   (let [inputs (inputs-for-optimization optimize)
         reward-fn (rewardf-for-target target)]
     (optimization-data inputs reward-fn)))
  ([inputs reward-fn]
   (lazy-seq
     (when-let [[input sim-data] (first inputs)]
       (cons
         [input (reward-fn (run-simulation sim-data))]
         (optimization-data (rest inputs) reward-fn))))))

;;; Plotting code

(defn incr-into-atom
  "Incrementally reduce the `from-seq` into the `to-atom` state, in chunks of
  `chunk-size`, yielding with js/setTimeout between chunks."
  ([to-atom chunk-size rf from-seq]
   (let [proc-id (gensym)]
     (swap! to-atom assoc ::incr-into-atom proc-id)
     (incr-into-atom to-atom rf chunk-size from-seq proc-id)))
  ([to-atom chunk-size rf from-seq proc-id]
   (loop [idx 0
          from-seq from-seq]
     (if (< idx chunk-size)
       (let [state @to-atom]
         (when (= (::incr-into-atom state) proc-id)
           (if-let [x (first from-seq)]
             (do
               (reset! to-atom (rf state x))
               (recur (inc idx) (rest from-seq)))
             (reset! to-atom (rf state)))))
       (js/setTimeout
         #(incr-into-atom to-atom rf chunk-size from-seq proc-id)
         0)))))

(defn compute-nth-perc
  "A lazy sequence of the bankroll values for the nth percentile portfolio."
  [{:keys [portfolios]} nth-perc]
  (let [len (if-let [p (first portfolios)] (count (:y p)) 0)]
    (letfn [(nth-percs [idx]
              (lazy-seq
                (when (< idx len)
                  (cons
                    (percentile
                      (vec (sort (keep (comp #(nth % idx nil) :y) portfolios)))
                      nth-perc)
                    (nth-percs (inc idx))))))]
      (nth-percs 0))))

(defn conj-nth-perc
  ([perc {:keys [nth-perc] :as data} data-point]
   (let [nth-perc (or nth-perc
                    {:y           []
                     :opacity     1
                     :showledgend true
                     :perc        perc
                     :line        {:width 3}
                     :name        (str "p" perc)})]
     (assoc data :nth-perc
       (update nth-perc :y conj data-point))))
  ([perc data]
   (assoc-in data [:nth-perc :complete] true)))

(defn vline [x & {:keys [] :as line}]
  {:type :line :x0 x :y0 0 :x1 x :y1 1 :yref :paper :line line})

(defn top-annotation [x text & {:keys [] :as font}]
  {:x x :y 1.06 :xref :x :yref :paper :showarrow false :text text :font font})

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

(defonce optimization-state (r/atom {:x [] :y []}))

(defn compute-optimizations []
  (reset! optimization-state {:x [] :y []})
  ; If there are a lot of portfolios, go one data point at a time
  (let [chunk-size (cond
                     (<= (:portfolios @simulation) 50)
                     10

                     (<= (:portfolios @simulation) 100)
                     2

                     :else 1)]
    (incr-into-atom optimization-state chunk-size
      (fn [state data-point]
        (-> state
            (update :x conj (first data-point))
            (update :y conj (peek data-point))))
      (optimization-data @view))))

(defn optimization-plot []
  (let [{:keys [x y]} @optimization-state
        {:keys [target optimize]} @view]
    [plotly/plotly
     {:data   [{:x x
                :y y
                :opacity    1
                :showlegend false
                :type       :scatter}]
      :layout {:title  "Optimization"
               :xaxis  {:title optimize}
               :yaxis  {:title target
                        :type  (if (:log-axis @plot-settings)
                                 "log"
                                 "linear")}
               :width  (:width @plot-settings)
               :height (:height @plot-settings)}}]))

(comment
  (compute-optimizations)
  (def od (optimization-data @view))
  (reset! optimization-state {:x (mapv first od) :y (mapv peek od)})
  )

(defn plot [{:keys [portfolios nth-perc] :as data}]
  ; Either plot a histogram or a line chart with each of the simulated
  ; portfolios
  (case (:active @view)
    "terminal-values" [histogram data]
    "optimize" [optimization-plot]
    "portfolios" [plotly/plotly
                  {:data   (enc/conj-some portfolios nth-perc)
                   :layout {:title  "Portfolios"
                            :xaxis  {:title "Bet #"}
                            :yaxis  {:title "Bankroll"
                                     :type  (if (:log-axis @plot-settings)
                                              "log"
                                              "linear")}
                            :width  (:width @plot-settings)
                            :height (:height @plot-settings)}}]
    [:div "Error: no clause for active view: " (:active @view)]))

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
                        (let [data-state (reset! data
                                           (run-simulation
                                             {:simulation sim
                                              :territory territory}))]
                          (incr-into-atom data 10
                            (partial conj-nth-perc perc)
                            (compute-nth-perc data-state perc)))))
                        #_(compute-nth-perc data perc 0 s)
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
