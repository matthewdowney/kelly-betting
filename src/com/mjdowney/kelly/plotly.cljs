(ns com.mjdowney.kelly.plotly
  "React / Reagent plotly component."
  (:require [reagent.core :as r]
            ["plotly.js-dist-min" :as plotlyjs]))

(defn -plotly-new [id {:keys [data layout config]}]
  (.newPlot plotlyjs id (clj->js data) (clj->js layout) (clj->js config)))
(defn -plotly-react [id new-state] (.react plotlyjs id (clj->js new-state)))

(defn plotly
  "A Plotly plot.

  See https://plotly.com/javascript/ for usage."
  [{:keys [data layout config event->handler] :as plot-properties}]
  (let [plot-id (str (gensym))]
    (r/create-class
      {:display-name         "plotly-plot-component"
       :reagent-render       (fn [_] [:div {:id plot-id :class "plotly-plot"}])
       :component-did-mount  (fn [this]
                               (-plotly-new plot-id plot-properties)
                               (let [p (.getElementById js/document plot-id)]
                                 (doseq [[event handler] event->handler]
                                   (.on p (name event) handler))))
       :component-did-update (fn [this old-argv]
                               (let [new-argv (rest (r/argv this))]
                                 (-plotly-react plot-id (first new-argv))))})))
