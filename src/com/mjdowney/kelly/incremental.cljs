(ns com.mjdowney.kelly.incremental)

(defn incr-into-atom
  "Incrementally reduce the `from-seq` into the `to-atom` state, in chunks of
  `chunk-size`, yielding with js/setTimeout between chunks."
  ([to-atom chunk-size rf from-seq]
   (let [proc-id (gensym)]
     (swap! to-atom assoc ::incr-into-atom proc-id)
     (incr-into-atom to-atom rf chunk-size from-seq proc-id)))
  ([to-atom chunk-size rf from-seq proc-id]
   (loop [idx 0
          from-seq from-seq]
     (if (< idx chunk-size)
       (let [state @to-atom]
         (when (= (::incr-into-atom state) proc-id)
           (if-let [x (first from-seq)]
             (do
               (reset! to-atom (rf state x))
               (recur (inc idx) (rest from-seq)))
             (reset! to-atom (rf state)))))
       (js/setTimeout
         #(incr-into-atom to-atom rf chunk-size from-seq proc-id)
         0)))))
