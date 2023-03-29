(ns com.mjdowney.kelly-simulator
  (:require [com.mjdowney.kelly.incremental :refer [incr-into-atom]]
            [com.mjdowney.kelly.leva :as leva]
            [com.mjdowney.kelly.plotly :as plotly]
            [com.mjdowney.kelly.simulation :as sim]
            [goog.array :as garray]
            [goog.functions :as gfn]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            ["seedrandom" :as seedrandom]
            [taoensso.encore :as enc]))

;;; Simulation

(def n-portfolios 100)
(def n-bets 100)
#_(def nth-perc 50) ; optimize for 50th percentile
(def simulate (enc/memoize-last sim/simulate))
(defn vbutlast "`butlast` for vectors" [v] (subvec v 0 (dec (count v))))

(defn kelly-bet [{:keys [p-win-lose frac-win-lose]}]
  (let [[pw* _pl] p-win-lose
        [fw fl] frac-win-lose]
    (- (/ pw* fl) (/ (- 1 pw*) (- fw 1)))))

(defn portfolio-simulation-plot-data [results nth-perc]
  (let [nth-percentile-idx (int (Math/floor (* (dec n-portfolios) (/ nth-perc 100))))
        nth-perc-returns (mapv #(nth % nth-percentile-idx) (peek results))]
    {:nth-percentile nth-perc
     :return (peek nth-perc-returns)
     :data (concat
             (map
               (fn [portfolio]
                 {:y          portfolio
                  :opacity    0.15
                  :showlegend false
                  :type       :scatter})
               (vbutlast results))
             [{:y           nth-perc-returns
               :opacity     1
               :showledgend true
               :perc        nth-perc
               :line        {:width 3}
               :name        (str "p" nth-perc)}])}))

(defn simulated-portfolios-plot
  [{:keys [pw p-ruin noise bet plot-width odds nth-perc]
    :or   {noise 0.0 p-ruin 0.0 odds [2 1] nth-perc 50}}]
  (let [params {:rng-seed 1
                :p-winf   (if (pos? noise)
                            (sim/random-pwin-fn pw noise)
                            (constantly pw))
                :p-ruin   p-ruin
                :odds     odds
                :bet-size bet
                :nps      n-portfolios
                :nbs      n-bets
                :sorted?  true}
        {:keys [data return]} (portfolio-simulation-plot-data
                                (simulate params)
                                nth-perc)
        title (str "p" nth-perc " return = "
                   (if (< 0.10 return 1000.0)
                     (str (enc/round2 return) "x")
                     (str "10^" (enc/round1 (Math/log10 return)))))]
    [plotly/plotly
     {:data data
      :layout {:title title
               :yaxis {:title "return multiple" :type "log"}
               :xaxis {:title "bet #"}
               :legend {:x 1 :y 1 :xanchor :right}
               :margin {:r 10 :t 60}
               :width  plot-width}}]))

;; Uncertainty

(defn bet-size-optimization-data
  [{:keys [nth-perc] :or {nth-perc 50} :as params}]
  (let [nth-percentile-idx (int
                             (Math/floor
                               (* (dec n-portfolios)
                                  (/ nth-perc 100))))]
    (letfn [(optimize* [best n]
              (lazy-seq
                (when (<= n 100)
                  (let [bet-size (/ n 100.0)
                        results (-> (assoc params :bet-size bet-size)
                                    simulate
                                    peek ; get the aggregated portfolios
                                    peek) ; get the final bankroll for each
                        ; sort the final portfolios and get the nth percentile
                        results (doto (into-array results) garray/sort)
                        return (aget results nth-percentile-idx)
                        ; if this is better than the best return heretofore
                        best (if (> return (peek best)) [bet-size return] best)]
                    (cons
                      {:x bet-size :y return :best best}
                      (optimize* best (inc n)))))))]
      (cons {:x 0 :y 1 :best [0 1]} (optimize* [0 1] 1)))))

(defn bsod-getter []
  (let [bsod (r/atom nil)]
    (letfn [(recompute [{:keys [p-winf] :as params}]
              (swap! bsod assoc :x [] :y [] :best nil :complete false)
              (incr-into-atom bsod 10
                (fn
                  ([state x]
                   (-> state
                       (update :x conj (:x x))
                       (update :y conj (:y x))
                       (assoc :best (:best x))))
                  ([state] (assoc state :complete true)))
                ; Now that we've checked memoization, we can compute the fn
                (let [[pw noise] p-winf
                      params (assoc params :p-winf
                               (if (pos? noise)
                                 (sim/random-pwin-fn pw noise)
                                 (constantly pw)))]
                  (bet-size-optimization-data params))))]
      (let [recompute (enc/memoize-last recompute)]
        (fn [params]
          (recompute (dissoc params :bet-size))
          @bsod)))))

(defn optimal-bet-size-plot [_params]
  (let [get-bsod (bsod-getter)]
    (fn [{:keys [pw noise bet plot-width odds p-ruin nth-perc shade?]
          :or {odds [2 1] p-ruin 0.0 noise 0.0 nth-perc 50 shade? false}}]
      (let [params {:rng-seed 1
                    ; compute the fn later so that the params memoize
                    :p-winf   [pw noise]
                    :p-ruin   p-ruin
                    :odds     odds
                    :bet-size bet
                    :nps      n-portfolios
                    :nbs      n-bets
                    :nth-perc nth-perc
                    :sorted?  false}
            bsod (get-bsod params)
            idx (int (* bet 100))
            f* (kelly-bet {:p-win-lose [pw (- 1 pw)] :frac-win-lose odds})
            idx' (int (* (enc/round2 f*) 100))]
        [plotly/plotly
         {:data (into
                 [(assoc bsod
                    :name (str "p" nth-perc " return")
                    :showlegend true)]
                  (if (:complete bsod)
                    [{:x [(nth (:x bsod) idx nil)]
                      :y [(nth (:y bsod) idx nil)]
                      :type :scatter
                      :mode :markers
                      :marker {:color "red" :size 8}
                      :name (str "selected = " (nth (:x bsod) idx nil))
                      :showlegend true}
                     {:x [(nth (:x bsod) idx' nil)]
                      :y [(nth (:y bsod) idx' nil)]
                      :type :scatter
                      :mode :markers
                      :marker {:color "blue" :size 8}
                      :name (str "theoretical = " (nth (:x bsod) idx' nil))
                      :showlegend true}
                     {:x [(get-in bsod [:best 0])]
                      :y [(get-in bsod [:best 1])]
                      :type :scatter
                      :mode :markers
                      :name (str "sim optimal = " (get-in bsod [:best 0]))
                      :marker {:color "green" :size 8}
                      :showlegend true}]
                    []))
          :layout {:title "Return multiple by bet size"
                   :shapes (when (and shade? (:complete bsod))
                             [{:type "rect"
                               :xref "x"
                               :x0 (get-in bsod [:best 0])
                               :x1 (nth (:x bsod) idx' nil)

                               :yref "paper" :y0 0 :y1 1
                               :fillcolor "LightSalmon"
                               :opacity 0.5
                               :line {:width 0}
                               :layer "below"}])
                   :xaxis {:title "bet size (fraction of bankroll)"
                           :range [0 1]}
                   :margin {:r 30 :t 60}
                   :width  plot-width}}]))))

;;; Main

(defonce window
  (let [a (r/atom (.-innerWidth js/window))]
    (.addEventListener js/window "resize"
      (gfn/debounce #(reset! a (.-innerWidth js/window)) 100))
    a))

(defn dec->perc [x] (int (* 100 x)))
(defn perc->dec [x] (/ x 100.0))

(defn slider [{:keys [lbl> f> f< !state min max step]
               :or {min 0 max 100 step 1
                    f> dec->perc
                    f< perc->dec}}]
  [:div {:style {:display :flex :align-items :baseline :gap "1em"}}
   [:p {:style {:min-width "25%" :text-align :right :margin-bottom 0 :font-size "0.75em"}} (lbl> @!state)]
   [:input
    {:type "range"
     :min min
     :max max
     :step step
     :value (f> @!state)
     :on-change #(reset! !state (-> % .-target .-value f<))}]])

(def uparams (r/atom {:pw 0.70 :bet 0.10 :noise 0.0}))

(defn -uncertainty []
  (let [bet-size (r/cursor uparams [:bet])
        noise (r/cursor uparams [:noise])
        pwin (r/cursor uparams [:pw])]
    (fn []
      (let [large-window? (> @window 900)
            plot-width (if large-window?
                         (- (enc/clamp 400 800 (/ @window 2)) 20)
                         (- @window 50))]

        [:div {:style {:font-weight 100}}
         [:div.container
          [:span
           "Simulation of " n-portfolios " portfolios wagering " n-bets
           " times with p(win) = " (dec->perc @pwin) "%"]]
         (let [params (assoc @uparams :plot-width plot-width)]
           [:div {:style {:display (if large-window? :flex :grid)
                          :justify-content :center
                          :margin-top "20px"}}
            [simulated-portfolios-plot params]
            [optimal-bet-size-plot params]])

         [:div.container
          [slider {:lbl> (fn [x] (str "Bet fraction = " x)) :!state bet-size}]
          [slider {:lbl> (fn [x] (str "Uncertainty σ = " x)) :!state noise}]
          [slider {:lbl> (fn [x] (str "P(win) = " x)) :!state pwin}]]]))))

;; N.b. odds here are [win-frac loss-frac] not 'N:N'
(def rparams
  (r/atom
    {:pw 0.60
     :p-ruin 0.00
     :win-frac 1.25
     :lose-frac 0.25
     :bet 0.80}))

(comment

  (let [p [0.60 0.39 0.01]
        r [0.25 -0.25 -1]
        dotprod #(apply + (apply map * %))
        war (dotprod [p r])

        square #(* % %)
        war-squared (dotprod [p (map square r)])

        variance (- war-squared (square war))]
    (/ war variance))

  )

(defn -risk-of-ruin []
  (let [bet-size (r/cursor rparams [:bet])
        p-ruin (r/cursor rparams [:p-ruin])
        pwin (r/cursor rparams [:pw])
        wfrac (r/cursor rparams [:win-frac])
        lfrac (r/cursor rparams [:lose-frac])]
    (fn []
      (let [large-window? (> @window 900)
            plot-width (if large-window?
                         (- (enc/clamp 400 800 (/ @window 2)) 20)
                         (- @window 50))]

        [:div {:style {:font-weight 100}}
         [:div.container
          [:span
           "Simulation of " n-portfolios " portfolios wagering " n-bets
           " times with p(win) = " (dec->perc @pwin) "%,"
           " p(ruin) = " (dec->perc @p-ruin) "%"]]
         (let [params (assoc @rparams :plot-width plot-width)
               params (assoc params :odds ((juxt :win-frac :lose-frac) params))]
           [:div {:style {:display (if large-window? :flex :grid)
                          :justify-content :center
                          :margin-top "20px"}}
            [simulated-portfolios-plot params]
            [optimal-bet-size-plot params]])

         [:div.container
          [slider {:lbl> (fn [x] (str "Bet fraction = " x)) :!state bet-size}]
          [slider {:lbl> (fn [x] (str "Win fraction = " x))
                   :min 1.0
                   :max (max 200 (int (* @wfrac 100 1.1)))
                   :!state wfrac}]
          [slider {:lbl> (fn [x] (str "Lose fraction = " x)) :!state lfrac}]
          [slider {:lbl> (fn [x] (str "P(win) = " x)) :!state pwin}]
          [slider {:lbl> (fn [x] (str "P(ruin) = " x))
                   :max (int (* (- 1.0 @pwin) 100))
                   :!state p-ruin}]]]))))

;; N.b. odds here are [win-frac loss-frac] not 'N:N'
(def drparams (r/atom {:pw 0.70 :bet 0.30 :win-frac 2 :lose-frac 1 :nth-perc 10}))

(defn -downside-risk []
  (let [bet-size (r/cursor drparams [:bet])
        pwin (r/cursor drparams [:pw])
        nthp (r/cursor drparams [:nth-perc])]
    (fn []
      (let [large-window? (> @window 900)
            plot-width (if large-window?
                         (- (enc/clamp 400 800 (/ @window 2)) 20)
                         (- @window 50))]

        [:div {:style {:font-weight 100}}
         [:div.container
          [:span
           "p" @nthp " of " n-portfolios " portfolios wagering " n-bets
           " times with p(win) = " (dec->perc @pwin) "%"]]
         (let [params (assoc @drparams :plot-width plot-width)
               params (assoc params :odds ((juxt :win-frac :lose-frac) params))]
           [:div {:style {:display (if large-window? :flex :grid)
                          :justify-content :center
                          :margin-top "20px"}}
            [simulated-portfolios-plot params]
            [optimal-bet-size-plot (assoc params :shade? true)]])

         [:div.container
          [slider {:lbl> (fn [x] (str "Bet fraction = " x)) :!state bet-size}]
          [slider {:lbl> (fn [x] (str "P(win) = " x)) :!state pwin}]
          [slider
           {:lbl> (fn [x] (str "nth percentile = " x))
            :f< identity
            :f> identity
            :!state nthp}]]]))))

;;; Lifecycle / entry point
(defn start []
  (rdom/render [-uncertainty] (.getElementById js/document "uncertainty-sim"))
  (rdom/render [-risk-of-ruin] (.getElementById js/document "risk-of-ruin-sim"))
  (rdom/render [-downside-risk] (.getElementById js/document "downside-risk-sim"))

  (set! (.-setUncertaintyParameters js/window)
    (fn [e ps]
      (swap! uparams merge (js->clj ps :keywordize-keys true))
      (.preventDefault e)))

  (set! (.-setRiskOfRuinParameters js/window)
    (fn [e ps]
      (swap! rparams merge (js->clj ps :keywordize-keys true))
      (.preventDefault e)))

  (set! (.-setDownsideParameters js/window)
    (fn [e ps]
      (swap! drparams merge (js->clj ps :keywordize-keys true))
      (.preventDefault e))))

(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
