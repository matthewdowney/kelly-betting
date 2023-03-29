;; Older version of the main namespace, using Leva controls. This was cool for
;; exploring the probability space, but harder to present well on a blog page.
;; Leaving it here in case I want to modify it in the future.
(ns com.mjdowney.kelly.kelly-simulator-deprecated
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

(defonce controls3d
  (r/atom
    {:show3d-plots? false
     :large-plots? false
     :bet-sizes [0 85]
     :percentiles [25 75]
     :log-return? true}))

(def controls3d-schema
  {:show3d-plots?
   {:label "Show plots"
    :hint  "Show 3D plots? They are slow to render!"}

   :large-plots?
   {:label "Large plots"
    :hint  "Force 3d plots to take up the whole row."}

   :bet-sizes
   {:label    "Bet limits"
    :hint     "Exclude the extremes to get a more viewable plot."
    :min 0
    :max 100
    :joystick false}
   :percentiles
   {:label    "pN limits"
    :hint     "Percentiles. Exclude extremes for a more viewable plot."
    :min 0
    :max 100
    :joystick false}
   :log-return?
   {:hint  "Plot log returns instead of returns."}})

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

(defonce floating-window? (r/atom false))

(def popout-arrow-svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\">\n    <path d=\"M7.646 11.354l2.853-2.853-2.853-2.854a.5.5 0 0 1 .708-.708l3 3a.5.5 0 0 1 0 .708l-3 3a.5.5 0 0 1-.708-.708z\"/>\n  </svg>")

(defn create-popout-arrow []
  (when-let [ele (enc/rfirst
                   (fn [node] (= (.-textContent node) "Controls"))
                   (js/document.querySelectorAll "div[class^='leva-']"))]
    ;; append a button to the element's children
    (let [first-child-class (.-className (first (.-children ele)))
          rotation (if @floating-window? 180 0)
          i (doto (.createElement js/document "i")
              (.setAttribute "id" "custom-leva-popout-arrow")
              (.setAttribute "class" first-child-class)
              (.setAttribute "style"
                (str "margin-left: -4em; transform: rotate(" rotation "deg);")))]
      (set! (.-innerHTML i) popout-arrow-svg)
      (.appendChild ele i)
      (.addEventListener i "click"
        #(let [rotation (if (swap! floating-window? not) 180 0)]
           (set! (.-transform (.-style i)) (str "rotate(" rotation "deg)"))))
      i)))

(defn get-or-create-popout-arrow []
  (or
    (js/document.getElementById "custom-leva-popout-arrow")
    (create-popout-arrow)))

(defn leva-controls []
  (get-or-create-popout-arrow)
  [leva/SubPanel
   {:fill           (not @floating-window?)
    :flat           false
    :titleBar       {:drag @floating-window? :filter false :title "Controls"}
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
      :schema view-controls-schema}]
    [leva/Controls
     {:folder {:name "3d plots"}
      :atom   controls3d
      :schema controls3d-schema}]]])

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
        nth-percentile-idx (int (Math/floor (* (dec n-portfolios) (/ perc 100))))
        nth-perc-returns (mapv #(nth % nth-percentile-idx) (peek results))]
    {:nth-percentile perc
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
               :perc        perc
               :line        {:width 3}
               :name        (str "p" perc)}])}))

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
                   :xaxis {:title (str "bet size ("
                                       (if (= bet-strategy "% of bankroll")
                                         "fraction of bankroll"
                                         "fraction of kelly bet")
                                       ")")
                           :range [0 xmax]}
                   :legend {:x 1 :y 1 :xanchor :right}
                   :margin {:r 30}
                   :width  width}}]))))

;;; WIP 3D optimization plot

(defonce bet-noise-return3d (r/atom {:x [] :y [] :z []}))
(defonce bet-return3d (r/atom {:x [] :y [] :z []}))

(defn o3d-reward [results idx]
  (aget
    (doto (into-array (-> results peek peek))
      garray/sort)
    idx))

(defn bet-noise-return-data3d
  ([]
   (bet-noise-return-data3d (get @view-controls :nth-percentile)))
  ([nth-perc]
   (let [bc @behavior-controls
         wc @wager-controls
         {:keys [bet-sizes]} @controls3d
         idx (int (Math/floor (* (dec n-portfolios) (/ nth-perc 100))))]
     (for [noise (concat (range 0 10 2)
                   (range 10 20 4)
                   (range 20 (long (* (first (:p-win-lose wc)) 100)) 8))
           bet-size (apply range bet-sizes)]
       {:x bet-size
        :y noise
        :z (o3d-reward
             (run-portfolio-simulation
               (assoc bc :bet-size bet-size)
               (assoc wc :noise noise)
               false)
             idx)}))))

(defn bet-return-data3d []
  (let [bc @behavior-controls
        wc @wager-controls
        {:keys [bet-sizes percentiles]} @controls3d]
    (for [bet-size (apply range bet-sizes)
          :let [results (run-portfolio-simulation
                          (assoc bc :bet-size bet-size)
                          wc
                          false)]
          nth-perc (apply range percentiles)]
      {:x nth-perc
       :y bet-size
       :z (o3d-reward results
            (int (Math/floor (* (dec n-portfolios) (/ nth-perc 100)))))})))

(defn into-3d-rf
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

(defn compute-o3d [atom-3d lazy-3d-data-fn chunk-size]
  (reset! atom-3d {:x [] :y [] :z []})
  (incr-into-atom atom-3d chunk-size into-3d-rf (lazy-3d-data-fn)))

(defn plot-bet-noise-return3d [{:keys [width]}]
  (let [t (r/track compute-o3d bet-noise-return3d bet-noise-return-data3d 3)
        {:keys [x y z]} @bet-noise-return3d
        {:keys [bet-sizes log-return?]} @controls3d]
    @t
    [plotly/plotly
     {:data   [{:x x
                :y y
                :z (if log-return? (map (fn [xs] (map #(Math/log %) xs)) z) z)
                :opacity    1
                :showlegend false
                :type       :surface}]
      :layout {:scene {:yaxis  {:title "noise"
                                :range [1 (* (first (:p-win-lose @wager-controls)) 100)]}
                       :xaxis  {:title (if (= (:bet-strategy @behavior-controls) "% of Kelly bet")
                                         "% of Kelly bet"
                                         "% of bankroll")
                                :range bet-sizes}
                       :zaxis  {:title (if log-return? "log(return multiple)" "return multiple")}}
               :margin {:t 0 :b 0}
               :width  width
               :height 800}}]))

(defn plot-bet-return3d [{:keys [width]}]
  (let [{:keys [bet-sizes percentiles log-return?]} @controls3d
        t (r/track compute-o3d bet-return3d bet-return-data3d 250)
        {:keys [x y z]} @bet-return3d]
    @t
    [plotly/plotly
     {:data   [{:x x
                :y y
                :z (if log-return? (map (fn [xs] (map #(Math/log %) xs)) z) z)
                :opacity    1
                :showlegend false
                :type       :surface}]
      :layout {:scene {:xaxis  {:title "nth percentile" :range percentiles}
                       :yaxis  {:title (if (= (:bet-strategy @behavior-controls) "% of Kelly bet")
                                         "fraction of Kelly bet"
                                         "bet size")
                                         :range bet-sizes}
                       :zaxis  {:title (if log-return? "log(return multiple)" "return multiple")}}
               :margin {:t 0 :b 0}
               :width  width
               :height 800}}]))

(defonce window
  (let [a (r/atom (.-innerWidth js/window))]
    (.addEventListener js/window "resize"
      (gfn/debounce #(reset! a (.-innerWidth js/window)) 100))
    a))

;; =============================================================================

(defonce data-3d (r/atom {:x [] :y [] :z []}))
(defonce data-3d2 (r/atom {:x [] :y [] :z []}))
(defonce data-3d3 (r/atom {:x [] :y [] :z []}))

(defn p-win-for-odds
  "Given odds, return the probability of winning necessary for the bet to have
  the `ev`."
  [odds-multiplier ev]
  (/ ev (+ odds-multiplier 1)))

(defn odds-for-p-win [p-win ev]
  (/ (- ev p-win) p-win))

(defn -returns [bet-size p-win odds]
  (let [final-returns
        (peek
          (peek
            (simulate
              {:rng-seed 1
               :p-winf (constantly p-win)
               :odds [(+ odds 1) 1]
               :bet-size bet-size
               :nps n-portfolios
               :nbs 100
               :sorted? false})))]
    (vec
      (doto (into-array final-returns)
        garray/sort))))

(defn compute-surface [p-win odds]
  (into {}
    (comp
      (map #(/ % 100.0))
      (map (juxt identity #(-returns % p-win odds))))
    (range 1 101)))

(defn optimal-bet-size [bet-size->returns p]
  (let [idx (int (Math/floor (* (dec n-portfolios) (/ p 100))))]
    (first
      (reduce
        (fn [[best-bet-size best-return] [bet-size returns]]
          (let [return-for-p (nth returns idx 0)]
            (if (> return-for-p best-return)
              [bet-size return-for-p]
              [best-bet-size best-return])))
        [0 1]
        bet-size->returns))))


(defn compute-3d [ev]
  (for [p-win (map #(/ % 20.0) (range 1 20))
        :let [odds (odds-for-p-win p-win ev)
              bet-size->returns (compute-surface p-win odds)]
        p (range 5 75 1)
        :let [optimal (optimal-bet-size bet-size->returns p)]]
    {:x p
     :y (enc/round2 p-win)
     :z optimal}))

(comment
  (def computed
    (compute-3d))

  (compute-o3d data-3d #(compute-3d 1.1) 100)
  (compute-o3d data-3d2 #(compute-3d 1.5) 100)
  (compute-o3d data-3d3 #(compute-3d 5) 100)

  (count (pr-str @data-3d))

  (defn download-file [text filename]
    (let [blob (js/Blob. (array text) #js {:type "text/plain"})
          url (js/URL.createObjectURL blob)
          link (js/document.createElement "a")]
      (set! (.-href link) url)
      (set! (.-download link) filename)
      (js/document.body.appendChild link)
      (.click link)
      (js/document.body.removeChild link)))

  (download-file (pr-str {1.1 @data-3d 1.5 @data-3d2 5 @data-3d3}) "three-d-plots.edn")
  )

(comment
  (def pid "G__8")

  (def plot (.getElementById js/document pid))

  (.on plot "plotly_hover" #(let [selectedPoints (.-points %)]
                                 (println "X")
                                 (doseq [point selectedPoints]
                                   (let [traceIndex (.-curveNumber point)
                                         xCoord (.-x point)
                                         yCoord (.-y point)
                                         zCoord (.-z point)]
                                     ;; Do something with the trace index and coordinates
                                     (println (str "Trace Index: " traceIndex))
                                     (println (str "X Coord: " xCoord))
                                     (println (str "Y Coord: " yCoord))
                                     (println (str "Z Coord: " zCoord))))))
  )

(defn p3d [{:keys [width]}]
  (let [#_#_t (r/track compute-o3d data-3d compute 1)
        {:keys [x y z]} @data-3d]
    #_@t
    [plotly/plotly
     {:data   [{:x          x
                :y          y
                :z          z
                :opacity    1
                :showlegend false
                :showscale  false
                :type       :surface
                :name      "2.5x EV"
                :colorscale [[0, "rgb(255,245,240)"],
                             [0.125, "rgb(254,224,210)"],
                             [0.25, "rgb(252,187,161)"],
                             [0.375, "rgb(252,146,114)"],
                             [0.5, "rgb(251,106,74)"],
                             [0.625, "rgb(239,59,44)"],
                             [0.75, "rgb(203,24,29)"],
                             [0.875, "rgb(165,15,21)"],
                             [1, "rgb(103,0,13)"]]}
               (let [{:keys [x y z]} @data-3d2]
                 {:x          x
                  :y          y
                  :z          z
                  :opacity    1
                  :showlegend false
                  :type       :surface
                  :showscale  false
                  :name       "1.5x EV"
                  :colorscale [[0, "rgb(158,202,225)"],
                               [0.125, "rgb(49,130,189)"],
                               [0.25, "rgb(34,94,168)"],
                               [0.375, "rgb(29,62,115)"],
                               [0.5, "rgb(8,29,88)"],
                               [0.625, "rgb(37,52,148)"],
                               [0.75, "rgb(34,94,168)"],
                               [0.875, "rgb(29,62,115)"],
                               [1, "rgb(8,29,88)"]]})
               (let [{:keys [x y z]} @data-3d3]
                 {:x          x
                  :y          y
                  :z          z
                  :opacity    1
                  :showlegend false
                  :type       :surface
                  :showscale  false
                  :name       "3.5x EV"
                  :colorscale [[0, "rgb(247,252,245)"],
                               [0.125, "rgb(229,245,224)"],
                               [0.25, "rgb(199,233,192)"],
                               [0.375, "rgb(161,217,155)"],
                               [0.5, "rgb(116,196,118)"],
                               [0.625, "rgb(65,171,93)"],
                               [0.75, "rgb(35,139,69)"],
                               [0.875, "rgb(0,109,44)"],
                               [1, "rgb(0,68,27)"]]})]
      :layout {:scene  {:xaxis {:title "percentile" #_#_:range [1 10]}
                        :yaxis {:title "p(win)" #_#_:range [0 100]}
                        :zaxis {:title "optimal bet size"}}
               :margin {:t 0 :b 0}
               :width  width
               :height 800}}]))

;; =============================================================================

(defn app []
  (let [large-window? (> @window 900)
        plot-width (if large-window?
                     (- (enc/clamp 400 1000 (/ @window 2)) 20)
                     (- @window 50))]
    [:div {:style {:display (if (:large-plots? @controls3d) :grid :flex)
                   :justify-content :center}}
     [p3d {:width plot-width}]]
    #_[:div
     [:div {:style {:display (if large-window? :flex :grid)
                    :justify-content :center}}
      (let [{:keys [data]} (portfolio-simulation-plot-data)]
        [plotly/plotly
         {:data data
          :layout {:title (str "Simulated return for " n-portfolios " portfolios over 100 bets")
                   :yaxis {:title "return multiple" :type "log"}
                   :xaxis {:title "bet #"}
                   :legend {:x 1 :y 1 :xanchor :right}
                   :margin {:r 10}
                   :width  plot-width}}])
      [optimization-plot {:width plot-width}]]
     [:div.container.leva {:style {:line-height 2.45}}
      [leva-controls]]

     (when (:show3d-plots? @controls3d)
       (let [plot-width (- (enc/clamp 800 1600 (/ @window 2)) 20)
             plot-width (if (:large-plots? @controls3d) plot-width (- (/ @window 2) 20))]
         [:div {:style {:display (if (:large-plots? @controls3d) :grid :flex)
                        :justify-content :center}}
          [plot-bet-noise-return3d {:width plot-width}]
          [plot-bet-return3d {:width plot-width}]]))]))

;;; Lifecycle / entry point

;; Add a mutation observer to add the popout arrow when the leva panel is changed
(def leva-popout-arrow-hack
  (js/MutationObserver. (fn [_ _] (get-or-create-popout-arrow))))

(defn start []
  #_(rdom/render [app] (.getElementById js/document "app"))
  (.observe leva-popout-arrow-hack (.getElementById js/document "app")
    #js {:childList true :subtree true}))

(defn stop []
  ;; Remove mutation observer
  (.disconnect leva-popout-arrow-hack)
  (js/console.log "stopping..."))

(defn ^:export init [] (start))
