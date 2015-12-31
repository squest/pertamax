(ns pertamax.physics.mekanika
  (:require
    [pertamax.utils :refer :all]
    [gorilla-plot.core :refer [bar-chart]]))

(def g -9.8)

(defn mekanika-1
  [{:keys [m ms mk f alfa teta]
    :or {alfa 0 teta 0}
    :as data}]
  (let [tets (let [res (- alfa teta)]
               (println "tets" (bulet res))
               res)
        w (let [res (* m g)]
            (println "w" (bulet res))
            res)
        wke-bidang (let [res (* w (cos teta))]
                     (println "wke-bidang" (bulet res))
                     res)
        wke-datar (let [res (* w (sin teta))]
                    (println "wke-datar" (bulet res))
                    res)
        fke-bidang (let [res (* f (sin tets))]
                     (println "fke-bidang" (bulet res))
                     res)
        tekan (let [res (+ wke-bidang fke-bidang)]
                (println "f-kontak" res)
                res)
        fke-datar (let [res (* f (cos tets))]
                    (println "fke-datar" (bulet res))
                    res)
        N (let [res (- tekan)]
            (println "N" (bulet res))
            res)
        fs-max (let [res (* N ms)]
                 (println "fs-max" (bulet res))
                 res)
        ftot (let [res (+ fke-datar wke-datar)]
               (println "sigma-f-datar" (bulet res))
               res)]
    (println data)
    (cond
      (<= N 0)
      [0.0 :fly-high]
      (>= (abs ftot) (abs fs-max))
      [(bulet (abs (* N mk))) :kinetik]
      :else
      [(bulet (abs ftot)) :statis])))

(def keys-1 [:m :ms :mk :f :alfa :teta])

(def jaga2
  [[50 0.3 0.2 50 0 0]
   [150 0.3 0.2 50 0 0]
   [20 0.3 0.1 450 0 30]
   [234 0.5 0.3 332 90 20]
   [23 0.3 0.1 324 -30 230]
   [23 0.2 0.1 230 134 32]
   [23 0.2 0.1 230 234 32]
   [30 0.2 0.15 250 -50 20]
   [56 0.3 0.2 50 130 210]
   [93 0.6 0.25 320 30 60]
   [93 0.6 0.25 320 30 60]
   [245 0.3 0.2 250 -90 90]
   [50 0.1 0.05 150 90 10]
   [30 0.2 0.15 250 -50 20]
   [56 0.3 0.2 50 30 210]
   [93 0.3 0.2 500 30 60]
   [24 0.3 0.2 25 -30 30]
   [5 0.1 0.05 150 90 10]
   [245 0.3 0.2 250 -90 90]
   [67 0.1 0.05 150 210 50]
   [30 0.2 0.15 250 -50 20]
   [56 0.3 0.2 50 30 210]
   [93 0.3 0.2 500 30 60]
   [245 0.3 0.2 250 -90 90]
   [5 0.1 0.05 150 90 10]])

(def test-1
  (mapv #(zipmap keys-1 %)
        (for [m (range 10 60 17)
              ms (range 0.1 0.5 0.17)
              f (range 20 300 57)
              teta [0 30 45 90]
              alfa [-39 0 29 180 210]
              :let [mk (/ ms 2)]]
          [m ms mk f alfa teta])))

(def my-funs
  [mekanika-1])

(def keterangans
  ["mekanika-1"])

(def test-data
  [test-1])

(defn mekanika-test
  [fn-list]
  (let [score (atom (vec (repeat (count fn-list) 0)))
        total (atom (vec (repeat (count fn-list) 0)))]
    (doseq [i (range (count fn-list))]
      (println "Testing " (nth keterangans i))
      (doseq [data (nth test-data i)]
        (let [ress ((nth fn-list i) data)
              resi ((nth my-funs i) data)]
          (if (not= ress resi)
            (do (println (nth keterangans i) " salah")
                (println "Seharusnya" (str resi) "Jawaban lo:" (str ress))
                (reset! total (update-in @total [i] inc)))
            (do (println (nth keterangans i) "Bener!")
                (reset! total (update-in @total [i] inc))
                (reset! score (update-in @score [i] inc)))))))
    (let [scr (reduce + @score) ttl (reduce + @total)]
      (println "SCORE :" scr "dari total score yang mungkin" ttl)
      (println "Which means elo dapet" (int (* 100 (/ scr ttl))) "%"))
    (bar-chart keterangans (mapv #(* 100 (/ % %2)) @score @total))))



