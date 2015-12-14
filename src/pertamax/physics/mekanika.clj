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
      [(abs fx) :statis]
      [(abs (* N mk)) :kinetik])))

(def keys-1 [:m :ms :mk :f :alfa :teta])

(def test-1
  (mapv #(zipmap keys-1 %)
        [[50 0.3 0.2 50 0 0]]))

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

