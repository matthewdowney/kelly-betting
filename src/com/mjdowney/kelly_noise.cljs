(ns com.mjdowney.kelly-noise
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

;;; Simulation controls

(defonce wager-controls
  (r/atom
    {:p-win-lose [0.5 0.5]
     :frac-win-lose [1.6 0.4]
     :noise 0}))

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
   :noise
   {:label "Noise %"
    :hint  "A % value σ to compute P(win) ~ Gaussian(P(win), σ) for each bet."
    :min   0
    :max   100
    :step  1}})

(defonce behavior-controls
  (r/atom
    {:bet-strategy   "% of bankroll"
     :bet-size       25}))

(def behavior-controls-schema
  {:bet-strategy
   {:label   "Bet type"
    :hint    (str "Is the bet size a fraction of the bankroll or of "
                  "the theoretical Kelly-optimal bet?")
    :options ["% of bankroll" "% of Kelly bet"]}
   :bet-size
   {:label "Bet size %"
    :hint  "Amount to bet on each wager, either as a % of bank roll, or of f*."
    :min   0
    :max   100
    :step  1}})

(defonce view-controls (r/atom {:nth-percentile 50}))

(def view-controls-schema
  {:nth-percentile
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
      :schema behavior-controls-schema}]
    [leva/Controls
     {:folder {:name "View"}
      :atom   view-controls
      :schema view-controls-schema}]]])

;;; Simulation

(def n-portfolios 100)
(def simulate (enc/memoize-last sim/simulate))
(defn vbutlast "`butlast` for vectors" [v] (subvec v 0 (dec (count v))))

(defn kelly-bet [{:keys [p-win-lose frac-win-lose]}]
  (let [[pw* _pl] p-win-lose
        [fw fl] frac-win-lose]
    (- (/ pw* fl) (/ (- 1 pw*) (- fw 1)))))

(defn run-portfolio-simulation
  ([bc wc] (run-portfolio-simulation bc wc true))
  ([{strat :bet-strategy bet :bet-size} wc sort?]
   (let [pw (first (:p-win-lose wc))
         bet (/ bet 100.0)
         noise (/ (:noise wc) 100.0)]
     (simulate
       {:rng-seed 1
        :p-winf   (if (pos? noise) (sim/random-pwin-fn pw noise) (constantly pw))
        :odds     (:frac-win-lose wc)
        :bet-size (if (= strat "% of bankroll") bet (* bet (kelly-bet wc)))
        :nps      n-portfolios
        :nbs      100
        :sorted?  sort?}))))

(defn portfolio-simulation-plot-data []
  (let [bh @behavior-controls
        wc @wager-controls
        results (run-portfolio-simulation bh wc)
        perc (:nth-percentile @view-controls)
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
  [{:keys [p-win-lose frac-win-lose noise] :as wc} strat nth-percentile n-portfolios n-bets rng-seed]
  (let [nth-percentile-idx (int
                             (Math/floor
                               (* (dec n-portfolios)
                                  (/ nth-percentile 100))))
        pw (first p-win-lose)
        pwf (if (pos? noise) (sim/random-pwin-fn pw (/ noise 100.0)) (constantly pw))
        kb (kelly-bet wc)
        limit (if (= strat "% of bankroll") 100 (min (* (/ 1 kb) 100) 150))
        bet-size-denom (if (= strat "% of bankroll") 1.0 kb)]
    (letfn [(optimize* [best n]
              (lazy-seq
                (when (<= n limit)
                  (let [bet-size (/ n 100.0)
                        results (peek
                                  (peek
                                    (simulate
                                      {:rng-seed rng-seed
                                       :p-winf pwf
                                       :odds frac-win-lose
                                       :bet-size (* bet-size bet-size-denom)
                                       :nps n-portfolios
                                       :nbs n-bets
                                       :sorted? false})))
                        results (doto (into-array results) garray/sort)
                        return (aget results nth-percentile-idx)
                        best (if (> return (peek best)) [bet-size return] best)]
                    (cons
                      {:x bet-size :y return :best best}
                      (optimize* best (inc n)))))))]
      (cons {:x 0 :y 1 :best [0 1]} (optimize* [0 1] 1)))))

(defn bsod-getter []
  (let [bsod (r/atom nil)]
    (letfn [(recompute [wager-controls bet-strategy nth-perc]
              (swap! bsod assoc :x [] :y [] :best nil :complete false)
              (incr-into-atom bsod 10
                (fn
                  ([state x]
                   (-> state
                       (update :x conj (:x x))
                       (update :y conj (:y x))
                       (assoc :best (:best x))))
                  ([state] (assoc state :complete true)))
                (bet-size-optimization-data wager-controls bet-strategy
                  nth-perc 100 100 1)))]
      (let [recompute (enc/memoize-last recompute)]
        (fn [wager-controls bet-strategy nth-percentile]
          (recompute wager-controls bet-strategy nth-percentile)
          @bsod)))))

(defn optimization-plot [{:keys [width]}]
  (let [get-bsod (bsod-getter)
        get-bsodp50 (bsod-getter)]
    (fn [{:keys [width]}]
      (let [{:keys [bet-size bet-strategy]} @behavior-controls
            np (:nth-percentile @view-controls)
            bsod (get-bsod @wager-controls bet-strategy np)
            bsod1 (when-not (= np 50) (get-bsodp50 @wager-controls bet-strategy 50))
            idx bet-size
            xmax (if (= bet-strategy "% of bankroll") 1 1.5)]
        [plotly/plotly
         {:data (enc/conj-some
                  [(assoc bsod
                     :name (str "p" np " return")
                     :showlegend true)]
                  (when bsod1
                    (assoc bsod1 :name (str "p50 return") :showlegend true))
                  (when (:complete bsod)
                    {:x [(nth (:x bsod) idx)]
                     :y [(nth (:y bsod) idx)]
                     :type :scatter
                     :mode :markers
                     :marker {:color "red" :size 8}
                     :name (str "selected = " (nth (:x bsod) idx))
                     :showlegend true})

                  (when (:complete bsod)
                    {:x [(get-in bsod [:best 0])]
                     :y [(get-in bsod [:best 1])]
                     :type :scatter
                     :mode :markers
                     :name (str "optimal = " (get-in bsod [:best 0]))
                     :marker {:color "green" :size 8}
                     :showlegend true}))
          :layout {:title "Return multiple by bet size"
                   :xaxis {:title "Bet size" :range [0 xmax]}
                   :legend {:x 1 :y 1 :xanchor :right}
                   :margin {:r 30}
                   :width  width}}]))))

;;; ===========================================================================
;;; WIP 3D optimization plot
;;; ===========================================================================

(defonce o3d (r/atom {:x [] :y [] :z []}))

(defn o3d-reward [results idx]
  (aget
    (doto (into-array (-> results peek peek))
      garray/sort)
    idx))

(defn o3d-data
  ([]
   (o3d-data (get @view-controls :nth-percentile)))
  ([nth-perc]
   (let [bc @behavior-controls
         wc @wager-controls
         idx (int (Math/floor (* (dec n-portfolios) (/ nth-perc 100))))]
     (for [noise (concat (range 0 10 2)
                   (range 10 20 4)
                   (range 20 (long (* (first (:p-win-lose wc)) 100)) 8))
           bet-size (range 0 101 2)]
       {:x bet-size
        :y noise
        :z (o3d-reward
             (run-portfolio-simulation
               (assoc bc :bet-size bet-size)
               (assoc wc :noise noise)
               false)
             idx)}))))

(defn compute-o3d []
  (reset! o3d {:x [] :y [] :z []})
  (incr-into-atom o3d 3
    (fn
      ([state {:keys [x y z]}]
       (let [last-x (peek (:x state))]
         (if (= y (peek (:y state)))
           ; same y, new x
           (let [zs (get state :z)]
             (assoc state
               :x (if (and last-x (<= x last-x))
                    (:x state)
                    (conj (:x state) x))
               :z (update zs (dec (count zs)) conj z)))
           (assoc state
             :x (if (and last-x (<= x last-x))
                  (:x state)
                  (conj (:x state) x))
             :y (conj (:y state) y)
             :z (conj (:z state) [z])))))
      ([state] state))
    (o3d-data)))

;; same as the function above, but swapping x and y

(comment
  (compute-o3d)
  @o3d)

(defn plot-o3d [{:keys [width]}]
  (let [t (r/track compute-o3d)
        ;@wager-controls
        {:keys [x y z]} @o3d]
    @t
    [plotly/plotly
     {:data   [{:x x
                :y y
                :z z
                :opacity    1
                :showlegend false
                :type       :surface
                #_#_:mode       :lines
                #_#_:line      {:width 3 :colorscale "Viridis" :color z}}]
      :layout {:scene {:yaxis  {:title "Noise"
                                :range [1 (* (first (:p-win-lose @wager-controls)) 100)]}
                       :xaxis  {:title "Bet size"
                                :range [1 100]}
                       :zaxis  {:title "Return %"
                                #_#_:type  (if (:log-axis @plot-settings) "log" "linear")}}

               :margin {:t 00 :b 0}
               :width  width
               :height 800}}]))

;;; ===========================================================================
;;; End WIP 3D optimization plot
;;; ===========================================================================

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
       {:data (portfolio-simulation-plot-data)
        :layout {:title "Simulated portfolios"
                 :yaxis {:title "Return multiple" :type "log"}
                 :xaxis {:title "Bet #"}
                 :legend {:x 1 :y 1 :xanchor :right}
                 :margin {:r 10}
                 :width  plot-width}}]
      [optimization-plot {:width plot-width}]]
     [:div.container.leva {:style {:line-height 2.45}}
      [leva-controls]]
     [:div {:style {:display (if large-window? :flex :grid)
                    :justify-content :center}}
      [plot-o3d {:width plot-width}]]]))

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
