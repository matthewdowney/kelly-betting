(ns com.mjdowney.kelly-noise
  (:require [com.mjdowney.kelly.leva :as leva]
            [com.mjdowney.kelly.plotly :as plotly]
            [goog.array :as garray]
            [goog.functions :as gfn]
            [leva.core :as l]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            ["seedrandom" :as seedrandom]
            [taoensso.encore :as enc]))

;;; Simulation controls

(defonce wager-controls
  (r/atom
    {:p-win-lose    [0.5 0.5]
     :frac-win-lose [2 1]}))

(def wager-control-schema
  {:p-win-lose
   {:label     "P(Win, Lose)"
    :hint      "Probability of winning, losing."
    :x         {:min 0 :max 1 :step 0.01}
    :y         {:min 0 :max 1 :step 0.01}
    :on-change (fn [state [pw pl]]
                 (if (= pw (first (:p-win-lose state)))
                   (assoc state :p-win-lose [(- 1 pl) pl])
                   (assoc state :p-win-lose [pw (- 1 pw)])))
    :joystick  false}
   :frac-win-lose
   {:label    "Frac(Win, Lose)"
    :hint     (str "Multiply wagered amount by Frac(Win), or lose Frac(Lose), "
                   "according to the outcome.")
    :joystick false}

   ;; TODO: Consider getting rid of these two, and moving noise up here, to make
   ;;       the spacing nicer. Can display these computed values elsewhere.
   #_#_:odds
   {:label    "Odds"
    :hint     "This is `b` in the Kelly formula."
    :value    1.0
    :disabled true}
   #_#_:ev
   {:label    "Expected value"
    :value    1.1
    :disabled true}})

#_(defn recompute-odds-and-ev [{[pw pl] :p-win-lose [fw fl] :frac-win-lose :as x}]
  (assoc x
    :odds (enc/round2 (/ (- fw 1) fl))
    :ev (+ (* pw fw) (* pl (- 1 fl)))))

(defonce behavior-controls
  (r/atom
    {:bet-strategy   "% of bankroll"
     :bet-size       0.25
     :noise          0
     :nth-percentile 50}))

(def behavior-controls-schema
  {:bet-strategy
   {:label   "Bet type"
    :hint    (str "Is the bet size a fraction of the bankroll or of "
                  "the theoretical Kelly-optimal bet?")
    :options ["% of bankroll" "% of Kelly bet"]}
   :bet-size
   {:label "Bet size"
    :hint  "Fraction to bet on each wager."
    :min   0
    :max   1
    :step  0.01}
   :noise
   {:label "Noise"
    :hint  "A value σ to compute P(win) ~ Gaussian(P(win), σ) for each bet."
    :min   0
    :max   1
    :step  0.01}
   :nth-percentile
   {:label "Nth percentile"
    :hint  "Optimize for the Nth percentile simulated portfolio return."
    :min   0
    :max   100
    :step  1}
   :optimize (l/button #(js/alert "optimize") {:label "Optimize"})})

(def leva-theme
  {:sizes   {:titleBarHeight "30px"}
   :shadows {:level1 "none" :level2 "none"}
   :colors  {:highlight1 "#fefefe"
             :highlight2 "#b7bccb"
             :accent2    "steelblue"
             ; bg color of the root panel (main title bar), rows (main panel
             ; color), and inputs
             :elevation1 "#0a2f52"
             :elevation2 "#ffffff0b"
             :elevation3 "#292d39ff"}})

(defn leva-controls []
  [leva/SubPanel
   {:fill           true
    :flat           false
    :titleBar       {:drag false :filter false :title "Controls"}
    :hideCopyButton true
    :theme          leva-theme}
   [:<>
    [leva/Controls
     {:folder    {:name "Wager characteristics"}
      :atom      wager-controls
      :schema    wager-control-schema
      #_#_:set-state recompute-odds-and-ev}]
    [leva/Controls
     {:folder {:name "Behavior"}
      :atom   behavior-controls
      :schema behavior-controls-schema}]]])

;;; Simulation
;;; TODO: Noise

(defn simulate*
  [p-win-lose frac-win-lose bet-size n-portfolios n-bets rng-seed]
  (let [[pw _pl] p-win-lose
        [fw fl] frac-win-lose
        fw-minus-1 (- fw 1)
        rng (seedrandom rng-seed)]
    (letfn [(make-bet [portfolio all-portfolios-array]
              (let [bankroll (peek portfolio)
                    wager (* bankroll bet-size)
                    val (if (<= (rng) pw)
                          (+ bankroll (* wager fw-minus-1))
                          (- bankroll (* wager fl)))]
                (.push all-portfolios-array val)
                (conj portfolio val)))]
      (loop [n-bets n-bets
             portfolios (vec (repeat n-portfolios [1.0]))
             sorted-portfolios [(vec (repeat n-portfolios 1.0))]]
        (if (pos? n-bets)
          (let [apa (make-array 0)
                portfolios (into [] (map #(make-bet % apa) portfolios))]
            (recur
              (dec n-bets)
              portfolios
              (conj sorted-portfolios (vec (doto apa garray/sort)))))
          (conj portfolios sorted-portfolios))))))

(def msimulate* (enc/memoize-last simulate*))

(defn simulate
  "Simulate a series of bets.

  Returns a series of each portfolio's bankroll over time:

    [[bankroll(bet = 0), bankroll(bet = 1), ..., bankroll(bet = n)]
     ...]

  where the last element in the series is a vector of the sorted bankrolls at
  each bet, to facilitate calculation of the nth percentile portfolio."
  [{:keys [p-win-lose frac-win-lose]}
   {:keys [bet-strategy bet-size noise nth-percentile]}
   n-portfolios n-bets rng-seed]
  (msimulate* p-win-lose frac-win-lose bet-size n-portfolios n-bets rng-seed))

(defn vbutlast "`butlast` for vectors" [v] (subvec v 0 (dec (count v))))

(defn portfolio-simulation-data []
  (let [rng-seed 1
        n-portfolios 100
        n-bets 100
        bh @behavior-controls
        results (simulate @wager-controls bh n-portfolios n-bets rng-seed)
        perc (:nth-percentile bh)
        nth-percentile-idx (int (Math/floor (* (dec n-portfolios) (/ perc 100))))]
    (cons
      {:y           (mapv #(nth % nth-percentile-idx) (peek results))
       :opacity     1
       :showledgend true
       :perc        perc
       :line        {:width 3}
       :name        (str "p" perc)}
      (map
        (fn [portfolio]
          {:y          portfolio
           :opacity    0.15
           :showlegend false
           :type       :scatter})
        (vbutlast results)))))

(defn optimize
  [{:keys [p-win-lose frac-win-lose]}
   {:keys [bet-strategy bet-size noise nth-percentile]}
   n-portfolios n-bets rng-seed]
  (let [nth-percentile-idx (int
                             (Math/floor
                               (* (dec n-portfolios)
                                  (/ nth-percentile 100))))]
    (loop [x []
           y []
           n 0]
      (if (<= n 100)
        (let [bet-size (/ n 100.0)
              results (peek
                        (peek
                          (simulate* p-win-lose frac-win-lose
                            bet-size n-portfolios n-bets rng-seed)))]
          (recur
            (conj x bet-size)
            (conj y (nth results nth-percentile-idx))
            (inc n)))
        {:x x :y y}))))

(defn bet-size-optimization-data []
  (let [rng-seed 1
        n-portfolios 100
        n-bets 100
        bh @behavior-controls]
    (optimize @wager-controls bh n-portfolios n-bets rng-seed)))

(defonce window
  (let [a (r/atom (.-innerWidth js/window))]
    (.addEventListener js/window "resize"
      (gfn/debounce #(reset! a (.-innerWidth js/window)) 100))
    a))

(defn optimization-plot [{:keys [width]}]
  (let [{:keys [bet-size nth-percentile]} @behavior-controls
        bsod (bet-size-optimization-data)
        idx (* bet-size 100)]
    [plotly/plotly
     {:data [(assoc bsod
               :name (str "p" nth-percentile " return")
               :showlegend false)
             {:x [(nth (:x bsod) idx)]
              :y [(nth (:y bsod) idx)]
              :type :scatter
              :mode :markers
              :marker {:color "red" :size 8}
              :name " "
              :showlegend false}]
      :layout {:title (str "p" nth-percentile " return multiple by bet size")
               :yaxis {#_#_:type "log"}
               :xaxis {:title "Bet size"}
               :margin {:r 30}
               :width  width}}]))


(defn app []
  (let [large-window? (> @window 900)
        plot-width (if large-window?
                     (- (enc/clamp 400 1000 (/ @window 2)) 20)
                     (- @window 50))]
    [:div
     [:div {:style {:display (if large-window? :flex :grid)
                    :justify-content :center}}
      [plotly/plotly
       {:data (portfolio-simulation-data)
        :layout {:title "Simulated portfolios"
                 :yaxis {:title "Return multiple" :type "log"}
                 :xaxis {:title "Bet #"}
                 :legend {:x 1 :y 1 :xanchor :right}
                 :margin {:r 10}
                 :width  plot-width}}]
      [optimization-plot {:width plot-width}]]
     [:div.container.leva {:style {:line-height 2.45}}
      [leva-controls]]]))

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
