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

(defonce wager-controls (r/atom {:p-win-lose [0.5 0.5] :frac-win-lose [1.6 0.4]}))

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
    :joystick false}})

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
    :step  1}})

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
      :schema    wager-control-schema}]
    [leva/Controls
     {:folder {:name "Behavior"}
      :atom   behavior-controls
      :schema behavior-controls-schema}]]])

;;; Simulation

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
    (concat
      (map
        (fn [portfolio]
          {:y          portfolio
           :opacity    0.15
           :showlegend false
           :type       :scatter})
        (vbutlast results))
      [{:y           (mapv #(nth % nth-percentile-idx) (peek results))
        :opacity     1
        :showledgend true
        :perc        perc
        :line        {:width 3}
        :name        (str "p" perc)}])))

(defn bet-size-optimization-data
  [{:keys [p-win-lose frac-win-lose]} nth-percentile n-portfolios n-bets rng-seed]
  (let [nth-percentile-idx (int
                             (Math/floor
                               (* (dec n-portfolios)
                                  (/ nth-percentile 100))))]
    (letfn [(optimize* [best n]
              (lazy-seq
              (when (<= n 100)
                (let [bet-size (/ n 100.0)
                      results (peek
                                (peek
                                  (simulate* p-win-lose frac-win-lose
                                    bet-size n-portfolios n-bets rng-seed)))
                      return (nth results nth-percentile-idx)
                      best (if (> return (peek best)) [bet-size return] best)]
                  (cons
                    {:x bet-size :y return :best best}
                    (optimize* best (inc n)))))))]
      (cons {:x 0 :y 1 :best [0 1]} (optimize* [0 1] 1)))))

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

(defn bsod-getter []
  (let [bsod (r/atom nil)]
    (letfn [(recompute [wager-controls nth-perc]
              (swap! bsod assoc :x [] :y [] :best nil :complete false)
              (incr-into-atom bsod 10
                (fn
                  ([state x]
                   (-> state
                       (update :x conj (:x x))
                       (update :y conj (:y x))
                       (assoc :best (:best x))))
                  ([state] (assoc state :complete true)))
                (bet-size-optimization-data
                  wager-controls nth-perc
                  100 100 1)))]
      (let [recompute (enc/memoize-last recompute)]
        (fn [wager-controls nth-percentile]
          (recompute wager-controls nth-percentile)
          @bsod)))))

(defn optimization-plot [{:keys [width]}]
  (let [get-bsod (bsod-getter)
        get-bsodp50 (bsod-getter)]
    (fn [{:keys [width]}]
      (let [{:keys [bet-size nth-percentile]} @behavior-controls
            bsod (get-bsod @wager-controls nth-percentile)
            bsod1 (when-not (= nth-percentile 50)
                    (get-bsodp50 @wager-controls 50))
            idx (* bet-size 100)]
        [plotly/plotly
         {:data (enc/conj-some
                  [(assoc bsod
                     :name (str "p" nth-percentile " return")
                     :showlegend true)]
                  (when bsod1
                    (assoc bsod1 :name (str "p50 return") :showlegend true))
                  (when (:complete bsod)
                    {:x [(nth (:x bsod) idx)]
                     :y [(nth (:y bsod) idx)]
                     :type :scatter
                     :mode :markers
                     :marker {:color "red" :size 8}
                     :name " "
                     :showlegend false}))
          :layout {:title "Return multiple by bet size"
                   :xaxis {:title "Bet size" :range [0 1]}
                   :legend {:x 1 :y 1 :xanchor :right}
                   :margin {:r 30}
                   :width  width
                   :annotations (when (:complete bsod)
                                  [{:x (get-in bsod [:best 0])
                                    :y (get-in bsod [:best 1])
                                    :text (str "f* = " (get-in bsod [:best 0]))
                                    :showarrow true}])}}]))))

(defonce window
  (let [a (r/atom (.-innerWidth js/window))]
    (.addEventListener js/window "resize"
      (gfn/debounce #(reset! a (.-innerWidth js/window)) 100))
    a))

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
