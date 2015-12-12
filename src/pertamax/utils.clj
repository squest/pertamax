(ns pertamax.utils
  (:require [clojure.string :as cs]))

(defn bulet
  "Returns the result of ngebuletin d into 5 decimal after ."
  [d]
  (if (< -0.0000001 d 0.00000001)
    0.0
    (let [[a b] (cs/split (str d) #"\.")
          n (read-string a)]
      (+ n (read-string (str "0." (apply str (take 5 b))))))))

(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
(defn ->rad [x] (Math/toRadians x))
(defn sin [x] (Math/sin (->rad x)))
(defn cos [x] (Math/cos (->rad x)))
