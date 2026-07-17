(ns roguelike.rng)

(def ^:private mask-32 0xFFFFFFFF)
(def ^:private warmup-iterations 12)

(defn- rotate-left-32
  [x r]
  (bit-and mask-32
           (bit-or (bit-shift-left x r)
                   (unsigned-bit-shift-right x (- 32 r)))))

;; sfc32 algorithm
;; takes in a state and returns the new state and the output
;; the new state is a length 4 vector [a b c d] where each value is a 32-bit int
;; output is the next random number from the generator
(defn next-rng-state
  [state]
  (let [[a b c d] state
        output (bit-and mask-32 (+ a b d))
        d   (bit-and mask-32 (inc d))
        a   (bit-and mask-32 (bit-xor b (unsigned-bit-shift-right b 9)))
        b   (bit-and mask-32 (+ c (bit-shift-left c 3)))
        c   (rotate-left-32 c 21)
        c   (bit-and mask-32 (+ c output))]
    [[a b c d] output]))

(defn- seed-to-initial-state
  "Takes in a 64-bit integer and spreads it to the 4 state values for use in the sfc32 algorithm.
  Returns the state values as a vector [a b c d]."
  [seed]
  (let [top (unsigned-bit-shift-right seed 32)
        bottom (bit-and seed mask-32)]
    [0 top bottom 1]))

(defn make
  "'Warms up' the PRNG. Splits the seed into an initial state, generates an infinite sequence of states
  then takes the 12th state. Returns the final state in a vector usable by next-rng-stage ([a b c d])."
  [seed]
  (let [initial-state (seed-to-initial-state seed)
        step (fn [state _] (first (next-rng-state state)))]
    (reduce step initial-state (range warmup-iterations))))

(defn rand-double
  "Takes an RNG state and returns a random double between 0.0 (inclusive) and 1.0 (exclusive)."
  [state]
  (let [[next-state output] (next-rng-state state)
        normalized (/ output (double 0x100000000))]
    [next-state normalized]))

(defn rand-int-range
  "Takes a state and a min and max val, then returns a new state along with a random integer
  that is within the provided range. The max val is EXCLUSIVE of the value. If you want the range
  1-100 to include 100, you need to pass 101 as the max."
  [state min-val max-val]
  (let [[next-state normalized] (rand-double state)
        span (- max-val min-val)
        value (long (Math/floor (+ min-val (* normalized span))))]
    [next-state value]))
