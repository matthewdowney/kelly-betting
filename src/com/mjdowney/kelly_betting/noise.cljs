(ns com.mjdowney.kelly-betting.noise
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [leva.core :as leva]
            ["plotly.js-dist-min" :as plotlyjs]))

;;; React / Reagent version of a Plotly component

(defn -plotly-new [id data+layout] (.newPlot plotlyjs id (clj->js data+layout)))
(defn -plotly-react [id new-state] (.react plotlyjs id (clj->js new-state)))

(defn plotly [data]
  (let [plot-id (str (gensym))]
    (r/create-class
      {:display-name "custom-plot-component"
       :reagent-render (fn [_] [:div {:id plot-id :class "plotly-plot"}])
       :component-did-mount (fn [this] (-plotly-new plot-id data))
       :component-did-update (fn [this old-argv]
                               (let [new-argv (rest (r/argv this))]
                                 (-plotly-react plot-id (first new-argv))))})))

;;; App

(defonce controls (r/atom {:width 500 :height 500}))

(defn app []
  [:div
   "Hello, world!"
   [leva/Controls {:folder {:name "Plot Controls"} :atom controls}]
   [plotly {:data [{:x [1 2 3]
                    :y [2 6 3]
                    :type "scatter"
                    :mode "lines+markers"
                    :marker {:color "red"}}]
            :layout {:title "A Fancy Plot"
                     :width (:width @controls)
                     :height (:height @controls)}}]])

;;; Lifecycle / entry point

(defn start [] (rdom/render [app] (.getElementById js/document "app")))
(defn stop [] (js/console.log "stopping..."))
(defn ^:export init [] (start))
