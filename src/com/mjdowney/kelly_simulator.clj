(ns com.mjdowney.kelly-simulator
  "Macros for kelly-simulator.")

(defmacro plot-data-3d
  "A map of expected value -> 3d plotly surface data."
  []
  (update-vals
    (read-string (slurp "resources/three-d-plots.edn"))
    #(select-keys % [:x :y :z])))
