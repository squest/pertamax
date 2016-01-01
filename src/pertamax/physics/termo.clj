(ns pertamax.physics.termo
  (:require [pertamax.utils :refer :all]))

(defn kalor-1
  "Soal : given a c and a list of entities with m and t.
  Returns the t which is the termal equillibrium of these entities."
  [c ms]
  (let [things (sort-by :t ms)]
    (loop [[x & xs] things res {:m 0 :t 0}]
      (if x
        (let [{:keys [m t]} x
              [mr tr] (mapv #(get res %) [:m :t])]
          ;; m1.c1.tr - m1.c1.t1 == m2.c2.t2 - m2.c2.tr
          ;; tr (m1.c1 + m2.c2) == (m1.c1.t1 + m2.c2.t2)
          (if (== 0 mr)
            (recur xs x)
            (recur xs {:m (+ m mr)
                       :t (/ (+ (* mr c tr) (* m c t)) (+ (* m c) (* mr c)))})))
        [(get res :t)]))))

(defn kalor-2
  "Soal : given a list of entities with m, c, and t.
  Returns the t which is the termal equillibrium of these entities."
  [ms]
  (let [things (sort-by :t ms)]
    (loop [[x & xs] things res {:m 0 :t 0 :c 0}]
      (if x
        (let [{:keys [m c t]} x
              [mr cr tr] (mapv #(get res %) [:m :c :t])]
          ;; m1.c1.tr - m1.c1.t1 == m2.c2.t2 - m2.c2.tr
          ;; tr (m1.c1 + m2.c2) == (m1.c1.t1 + m2.c2.t2)
          (if (== 0 mr)
            (recur xs x)
            (recur xs {:m (+ m mr)
                       :t (/ (+ (* mr cr tr) (* m c t)) (+ (* m c) (* mr cr)))
                       :c (/ (+ (* m c) (* mr cr)) (+ m mr))})))
        [(get res :t)]))))

(defn kalor-3
  "Soal : given two entities with m,c1,c2,c3,L,U,t,tl,tu.
  Where L = kalor lebur, U = Kalor uap, t = temperatur, tl = titik lebur, tu = titik uap"
  [b1 b2]
  (let [[m1 c11 c12 c13 L1 U1 t1 tl1 tu1 :as mb1]
        (mapv #(get b1 %) [:m :c1 :c2 :c3 :L :U :t :tl :tu])
        [m2 c21 c22 c23 L2 U2 t2 tl2 tu2 :as mb2]
        (mapv #(get b2 %) [:m :c1 :c2 :c3 :L :U :t :tl :tu])
        ft (fn [t tl tu]
             (cond (< t tl) 0
                   (== t tl) 1
                   (< tl t tu) 2
                   (== t tu) 3
                   (> t tu) 4))
        which-f (fn [{:keys [m c1 c2 c3 L U t tl tu]}]
                  (-> {0 c1 1 L 2 c2 3 U 4 c3}
                      (get (ft t tl tu))))]
    ))

(def data-kalor-1
  (for [i (range 5)]
    [(rand-range 1 5 0.7)
     (for [j (range 5)]
       {:m (rand-range 2 8 0.15)
        :t (rand-range -20 30 4)})]))

(def data-kalor-2
  (for [i (range 5)]
    [(for [j (range 5)]
       {:m (rand-range 2 8 0.15)
        :c (rand-range 1 3 0.2)
        :t (rand-range -20 30 4)})]))

(defn tester
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
        (if (->> (mapv #(if (number? %) (f= % %2) (= % %2)) test-result your-result)
                 (every? true?))
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
