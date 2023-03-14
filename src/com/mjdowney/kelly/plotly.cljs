(ns com.mjdowney.kelly.plotly
  "React / Reagent plotly component."
  (:require [reagent.core :as r]
            ["plotly.js-dist-min" :as plotlyjs]))

(defn -plotly-new [id data+layout] (.newPlot plotlyjs id (clj->js data+layout)))
(defn -plotly-react [id new-state] (.react plotlyjs id (clj->js new-state)))

(defn plotly
  "A Plotly plot.

  See https://plotly.com/javascript/ for usage."
  [{:keys [data layout] :as plot-properties}]
  (let [plot-id (str (gensym))]
    (r/create-class
      {:display-name         "plotly-plot-component"
       :reagent-render       (fn [_] [:div {:id plot-id :class "plotly-plot"}])
       :component-did-mount  (fn [this] (-plotly-new plot-id plot-properties))
       :component-did-update (fn [this old-argv]
                               (let [new-argv (rest (r/argv this))]
                                 (-plotly-react plot-id (first new-argv))))})))
