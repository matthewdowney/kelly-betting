(ns com.mjdowney.kelly-noise
  (:require [com.mjdowney.kelly.plotly :as plotly]
            [com.mjdowney.kelly.leva :as leva]
            [leva.core :as l]
            [reagent.core :as r]
            [reagent.dom :as rdom]
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
   :odds
   {:label    "Odds"
    :hint     "This is `b` in the Kelly formula."
    :value    1.0
    :disabled true}
   :ev
   {:label    "Expected value"
    :value    1.1
    :disabled true}})

(defn recompute-odds-and-ev [{[pw pl] :p-win-lose [fw fl] :frac-win-lose :as x}]
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
      :set-state recompute-odds-and-ev}]
    [leva/Controls
     {:folder {:name "Behavior"}
      :atom   behavior-controls
      :schema behavior-controls-schema}]]])

;;; Placeholder stuff

(defn rand-between [a b] (+ a (* (- b a) (rand))))

(defn random-walk
  ([] (random-walk 50))
  ([n]
   (lazy-seq
     (cons n
       (random-walk
         (Math/round
           (* n (rand-between 0.9 1.1))))))))

(defn app []
  [:div
   [plotly/plotly
    {:data   [{:x      (range 100)
               :y      (take 100 (random-walk 50))
               :type   "scatter"
               :mode   "lines+markers"
               :marker {:color "red"}}]
     :layout {:title "A Plot"
              :width  (- #p (enc/clamp 550 1000 #p (.-innerWidth js/window)) 50)}}]
   [:div.container.leva {:style {:line-height 2.45}}
    [leva-controls]]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
