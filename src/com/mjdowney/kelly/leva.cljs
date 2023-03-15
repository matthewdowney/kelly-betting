(ns com.mjdowney.kelly.leva
  (:require [goog.functions :as gfn]
            [reagent.core :as r]))

(defn -recompute-state [prv-state nxt-state key->on-change]
  (reduce-kv
    (fn [state key value]
      (if-let [handler (get key->on-change key)]
        (if (not= (get prv-state key) value)
          (handler state value)
          state)
        state))
    nxt-state
    nxt-state))

; See https://github.com/mentat-collective/Leva.cljs/issues/11
(defn leva-sync
  "Build a reagent atom to back a Leva component which syncs its state with the
  given `state` atom, but allows the user to register :on-change handlers for
  individual fields.

  Each on-change handler is shaped (fn [state new-value] => state')."
  [state key->on-change]
  (let [local-state (r/atom @state)]

    ; Propagate updates from `state` -> `local-state`
    (add-watch state ::sync (fn [_ _ _ nxt] (reset! local-state nxt)))

    ; Run some recompute logic on `local-state` when it changes, and update
    ; `state` with the result
    (add-watch local-state ::sync
      ; Without debounce I get "Maximum call stack size exceeded", possibly
      ; from updates coming out of order? (Naively I thought that, because the
      ; state of `state` is always valid, this would only be triggered on
      ; the user changing some Leva input.)
      (gfn/debounce
        (fn [key ref prv nxt]
          (let [state' (-recompute-state prv nxt key->on-change)]
            (when-not (= state' nxt)
              (reset! state state'))))
        100))

    local-state))