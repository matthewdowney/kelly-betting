(ns com.mjdowney.kelly.leva
  (:require [goog.functions :as gfn]

            [leva :as leva-js]
            [leva.core :as leva-cljs]
            [com.mjdowney.kelly.leva-schema :as leva-cljs-schema]

            ["react" :as react]
            [reagent.core :as r]))

(def inputs leva/LevaInputs)

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
        (fn [_key _ref prv nxt]
          (let [state' (-recompute-state prv nxt key->on-change)]
            (when-not (= state' nxt)
              (reset! state state'))))
        100))

    local-state))

(defn Controls* [opts]
  (let [[watch-id] (react/useState (str (random-uuid)))
        !state     (:atom opts)
        set-state  (get opts :set-state (constantly identity))
        initial    (if !state (.-state !state) {})
        ks         (keys initial)
        opts       (update opts :store #(or % (leva-js/useStoreContext)))
        [_ set]    (apply leva-js/useControls
                     (leva-cljs-schema/opts->argv opts))]
    (react/useEffect
      (fn mount []
        (if !state
          (do
            (add-watch !state watch-id
              (fn [_ _ _ new-state]
                (set (clj->js (set-state (select-keys new-state ks))))))
            (fn unmount []
              (remove-watch !state watch-id)))
          js/undefined)))
    nil))

(defn Controls
  "Component that renders inputs into a global or local Leva control panel,
  possibly synchronizing the panel's state into a provided atom.

  Placing this component anywhere in the render tree will add controls to the
  global Leva panel.

  To modify a local Leva panel, nest this component inside of a [[SubPanel]].

  Supported `opts` are:

  - `:schema`: A leva schema definition. Any value _not_ present in the
     supplied `:atom` should provide an `:onChange` handler.

  - `:atom`: atom of key => initial value for schema entries. Any entry found
     in both `:atom` and in `:schema` will remain synchronized between the panel
     and the supplied `:atom`.

  - `:folder`: optional map with optional keys `:name` and `:settings`:

    - `:name`: if provided, these controls will be nested inside of a folder
      with this name.

    - `:settings`: optional map customizing the folder's settings.
      See [[folder]] for a description of the supported options.

  - `:store`: this is an advanced option that you probably won't need. If you
    _do_ need this, pass a store created via leva's `useCreateStore`."
  [opts]
  [:f> Controls* opts])

(defn SubPanel
  "Component that configures a non-global, standalone Leva panel with the
  supplied map of `opts`.

  Any instance of [[Controls]] passed as `children` will render into this
  subpanel and not touch the global store.

  See the type [`LevaRootProps`][1] for a full list of available entries for
  `opts` and documentation for each.

  [1]: https://github.com/pmndrs/leva/blob/main/packages/leva/src/components/Leva/LevaRoot.tsx#L13-L93"
  [opts children]
  [:f> leva-cljs/SubPanel opts children])
