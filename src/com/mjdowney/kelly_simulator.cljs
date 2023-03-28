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
(def nth-perc 50) ; optimize for 50th percentile
(def simulate (enc/memoize-last sim/simulate))
(defn vbutlast "`butlast` for vectors" [v] (subvec v 0 (dec (count v))))

(defn kelly-bet [{:keys [p-win-lose frac-win-lose]}]
  (let [[pw* _pl] p-win-lose
        [fw fl] frac-win-lose]
    (- (/ pw* fl) (/ (- 1 pw*) (- fw 1)))))

(defn portfolio-simulation-plot-data [results]
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

(defn simulated-portfolios-plot [{:keys [pw noise bet plot-width]}]
  (let [params {:rng-seed 1
                :p-winf   (if (pos? noise)
                            (sim/random-pwin-fn pw noise)
                            (constantly pw))
                :odds     [2 1]
                :bet-size bet
                :nps      n-portfolios
                :nbs      n-bets
                :sorted?  true}
        {:keys [data return nth-percentile]} (portfolio-simulation-plot-data (simulate params))
        title (str "p" nth-percentile " return = "
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

;; Optimization

(defn bet-size-optimization-data [params]
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
    (fn [{:keys [pw noise bet plot-width]}]
      (let [params {:rng-seed 1
                    ; compute the fn later so that the params memoize
                    :p-winf   [pw noise]
                    :odds     [2 1]
                    :bet-size bet
                    :nps      n-portfolios
                    :nbs      n-bets
                    :sorted?  false}
            bsod (get-bsod params)
            idx (int (* bet 100))
            f* (kelly-bet {:p-win-lose [pw (- 1 pw)] :frac-win-lose [2 1]})
            idx' (int (* (enc/round2 f*) 100))]
        [plotly/plotly
         {:data (into
                 [(assoc bsod
                    :name (str "p" nth-perc " return")
                    :showlegend true)]
                  (if (:complete bsod)
                    [{:x [(nth (:x bsod) idx)]
                      :y [(nth (:y bsod) idx)]
                      :type :scatter
                      :mode :markers
                      :marker {:color "red" :size 8}
                      :name (str "selected = " (nth (:x bsod) idx))
                      :showlegend true}
                     {:x [(nth (:x bsod) idx')]
                      :y [(nth (:y bsod) idx')]
                      :type :scatter
                      :mode :markers
                      :marker {:color "blue" :size 8}
                      :name (str "theoretical = " (nth (:x bsod) idx'))
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
                   :xaxis {:title "bet size (fraction of bankroll)"
                           :range [0 1]}
                   :legend {:x 1 :y 1 :xanchor :right}
                   :margin {:r 30 :t 60}
                   :width  plot-width}}]))))

;;; Main

(defonce window
  (let [a (r/atom (.-innerWidth js/window))]
    (.addEventListener js/window "resize"
      (gfn/debounce #(reset! a (.-innerWidth js/window)) 100))
    a))

(def params (r/atom {:pw 0.70 :bet 0.10 :noise 0.0}))
(def bet-size (r/cursor params [:bet]))
(def noise (r/cursor params [:noise]))
(def pwin (r/cursor params [:pw]))

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
     :min 0
     :max 100
     :step 1
     :value (f> @!state)
     :on-change #(reset! !state (-> % .-target .-value f<))}]])

(defn app []
  (let [large-window? (> @window 900)
        plot-width (if large-window?
                     (- (enc/clamp 400 800 (/ @window 2)) 20)
                     (- @window 50))]

    [:div {:style {:font-weight 100 :margin-bottom "20px"}}
     [:div.container
       [:span (str "Simulation of " n-portfolios " portfolios wagering " n-bets
                   " times with p(win) = " (dec->perc @pwin) "%")]]
     (let [params (assoc @params :plot-width plot-width)]
       [:div {:style {:display (if large-window? :flex :grid)
                      :justify-content :center}}
        [simulated-portfolios-plot params]
        [optimal-bet-size-plot params]])

     [:div.container
      [slider {:lbl> (fn [x] (str "Bet fraction = " x)) :!state bet-size}]
      [slider {:lbl> (fn [x] (str "Uncertainty σ = " x)) :!state noise}]
      [slider {:lbl> (fn [x] (str "P(win) = " x)) :!state pwin}]]]))

;;; Lifecycle / entry point
(defn start []
  (rdom/render [app] (.getElementById js/document "app"))
  (set! (.-setUncertaintyParameters js/window)
    (fn [e ps]
      (swap! params merge (js->clj ps :keywordize-keys true))
      (.preventDefault e))))

(defn stop [] (js/console.log "stopping..."))
(defn ^:export init  [] (start))
