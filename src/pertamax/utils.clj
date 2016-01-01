(ns pertamax.utils
  (:require [clojure.string :as cs]))

(defn f=
  "Equality that works for decimal approximation."
  [a b & [precision]]
  (cond
   (number? a) (let [prec (if precision precision 0.001)]
                 (<= (- prec) (- a b) prec))
   (list? a) (every? true? (map f= a b))
   (vector? a) (every? true? (map f= a b))
   (map? a) (let [ksa (keys a)]
              (every? true? (map f= (map a ksa) (map b ksa))))
   :else (= a b)))

(defn tester-utils
  [your-function test-fn data]
  (let [bener (atom 0)
        salah (atom 0)
        ctr (atom 1)]
    (doseq [d data]
      (let [test-result (apply test-fn d)
            your-result (apply your-function d)]
        (println "Doing test no :" @ctr)
        (println "Your result :" your-result)
        (println "Correct result :" test-result)
        (if (f= test-result your-result)
          (do (println "Yeay bener!!")
              (swap! bener inc))
          (do (println "Salah nyong!")
              (swap! salah inc)))
        (println "----------------------------------")
        (swap! ctr inc)))
    (println "Your score : ")
    (println "Bener :" @bener)
    (println "Salah :" @salah)
    (println "Score :" (* 100.0 (/ @bener (count data))) "%")
    (* 100.0 (/ @bener (count data)))))

(defn bulet
  "Returns the result of ngebuletin d into 5 decimal after ."
  [d]
  (if (< -0.0000001 d 0.00000001)
    0.0
    (let [[a b] (cs/split (str d) #"\.")
          n (read-string a)]
      (+ n (read-string (str "0." (apply str (take 5 b))))))))

(defn rand-range
  ([i j] (rand-nth (range i (inc j))))
  ([i j step] (rand-nth (range i (inc j) step))))

(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
(defn ->rad [x] (Math/toRadians x))
(defn sin [x] (Math/sin (->rad x)))
(defn cos [x] (Math/cos (->rad x)))
