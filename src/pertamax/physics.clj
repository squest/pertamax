(ns pertamax.physics
  (:require
    [clojure.string :as cs]))

(defn sqrt [x] (Math/sqrt x))

(defn bulet
  "Returns the result of ngebuletin d into 5 decimal after ."
  [d]
  (if (< -0.0000001 d 0.00000001)
    0.0
    (let [[a b] (cs/split (str d) #"\.")
          n (read-string a)]
      (+ n (read-string (str "0." (apply str (take 5 b))))))))

(def chapters ["mekanika" "ggb" "termo" "optik" "fismod" "limag" "kapsel"])

(def g -10)

(defn abs [x] (Math/abs x))

(defn mekanika-1
  "Given m, miu-s, miu-k, and F returns [friction :statis/kinetik]"
  [m ms mk f]
  (let [fs-max (* m (- g) ms)]
    (if (> f fs-max)
      [(bulet (- (* m (- g) mk))) :kinetik]
      [(bulet (- fs-max)) :statis])))

