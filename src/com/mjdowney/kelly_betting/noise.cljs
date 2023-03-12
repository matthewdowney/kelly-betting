(ns com.mjdowney.kelly-betting.noise
  (:require [reagent.dom :as rdom]))


(defn app []
  [:div "Hello, world!"])

(defn ^:export init []
  (rdom/render [app] (.getElementById js/document "app")))
