(ns pertamax.physics.mekanika
  (:require
    [pertamax.utils :refer :all]
    [gorilla-plot.core :refer [bar-chart]]))

(def g -9.8)

(defn mekanika-1
  [{:keys [m ms mk f alfa teta]
    :or {alfa 0 teta 0}}]
  (let [tets (- alfa teta)
        w (* m g)
        wke-bidang (* w (cos teta))
        wke-datar (* w (sin teta))
        fy (* f (sin tets))
        tekan (+ wke-bidang fy)
        fx (* f (cos tets))
        N (- tekan)
        fs-max (* N ms)]
    (if (>= (abs fx) (abs fs-max))
      [(bulet (abs fx)) :statis]
      [(bulet (abs (* N mk))) :kinetik])))

(def keys-1 [:m :ms :mk :f :alfa :teta])

(def test-1
  (mapv #(zipmap keys-1 %)
        [[50 0.3 0.2 50 0 0]
         [150 0.3 0.2 50 0 0]
         [20 0.3 0.1 450 0 30]
         [234 0.5 0.3 332 90 -20]
         [23 0.3 0.1 324 -30 230]
         [23 0.2 0.1 230 234 32]
         [30 0.2 0.15 250 50 20]
         [56 0.3 0.2 50 30 210]
         [93 0.3 0.2 500 30 60]
         [245 0.3 0.2 250 -90 90]
         [5 0.1 0.05 150 90 -10]]))

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

