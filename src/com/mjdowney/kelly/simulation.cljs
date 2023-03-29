(ns com.mjdowney.kelly.simulation
  "Number crunching code for simulating iterated binary wagers."
  (:require ["seedrandom" :as seedrandom]
            [com.mjdowney.kelly.leva :as leva]
            [com.mjdowney.kelly.plotly :as plotly]
            [goog.array :as garray]
            [taoensso.encore :as enc]))

(defn deterministic-random-number-generator [seed] (seedrandom seed))

(defn rand-norm [rng mean stdev] ; box muller transform
  (let [u1 (rng)
        u2 (rng)
        r (Math/sqrt (* -2 (Math/log u1)))
        theta (* 2 Math/PI u2)]
    (+ mean (* stdev r (Math/cos theta)))))

(defn random-pwin-fn
  ; Math symbol for "in set" is "∈"
  "Build a function: random number generator -> p-win ∈ [0, 1].

  Takes a probability of winning each wager and adds some noise to it, drawing
  each p-win from a normal distribution, i.e. p-win ~ N(p-win, noise), clamped
  to [0 1]."
  [p-win-mean noise-stddev]
  (fn [rng]
    (enc/clamp 0.0 1.0 (rand-norm rng p-win-mean noise-stddev))))

(defn simulate
  "Simulate a series of bets.

  Returns a series of each portfolio's bankroll over time:

    [[bankroll(bet = 0), bankroll(bet = 1), ..., bankroll(bet = n)]
     ...]

  where the last element in the series is a vector of the sorted bankrolls at
  each bet, to facilitate calculation of the nth percentile portfolio.

  Parameters:

    - :rng-seed  seed for a deterministic random number generator
    - :p-winf    (fn [rng] => p-win ∈ [0, 1]), where rng generates uniform
                 numbers in [0, 1]
    - :p-ruin    Optionally, a risk of total ruin.
    - :odds      A vector of [frac-win frac-lose]. If losing a wager means
                 losing the whole stake, frac-lose = 1.

                 If betting 10 means winning 10 (e.g. you started with 10 and
                 end with 20, then frac-win = 2).

                 So the odds are (/ (- frac-win 1) frac-lose).
    - :bet-size  A number ∈ [0, 1] representing the fraction of the portfolio
                 to bet on each wager. If 0, no bets are made.
    - :nps       The number of portfolios to simulate.
    - :nbs       The number of bets to make per portfolio.
    - :sorted?   If true, append a final element containing the sorted bankrolls
                 at each bet. If false, same, but don't sort it."
  [{:keys [rng-seed p-winf p-ruin odds bet-size nps nbs sorted?] :or {sorted? true p-ruin 0}}]
  (let [[fw fl] odds
        fw-minus-1 (- fw 1)
        rng (seedrandom rng-seed)
        maybe-sort (if sorted? (fn [x] (vec (doto x garray/sort))) vec)]
    (letfn [(make-bet [portfolio all-portfolios-array]
              (let [bankroll (peek portfolio)
                    wager (* bankroll (max bet-size 0))
                    pw (p-winf rng)
                    rn (rng)
                    val (cond
                          (<= rn p-ruin) (- bankroll wager)
                          (<= rn (+ p-ruin pw)) (+ bankroll (* wager fw-minus-1))
                          :else (- bankroll (* wager fl)))]
                (.push all-portfolios-array val)
                (conj portfolio val)))]
      (loop [n-bets nbs
             portfolios (vec (repeat nps [1.0]))
             agg-portfolios [(vec (repeat nps 1.0))]]
        (if (pos? n-bets)
          (let [apa (make-array 0)
                portfolios (into [] (map #(make-bet % apa) portfolios))]
            (recur
              (dec n-bets)
              portfolios
              (conj agg-portfolios (maybe-sort apa))))
          (conj portfolios agg-portfolios))))))
