(ns pertamax.physics.mekanika
  (:require
    [pertamax.utils :refer :all]))

(def g -9.8)

(defn mekanika-1
  "Given m, miu-s, miu-k, and F returns [nilai-gaya-gesek :statis/kinetik]"
  [{:keys [m ms mk f]}]
  (let [N (- (* m g))
        fs-max (* N ms)]
    (if (> f fs-max)
      [(bulet (* m (- g) mk)) :kinetik]
      [(bulet fs-max) :statis])))

(def test-1
  (mapv #(zipmap [:m :ms :mk :f] %)
        [[10 0.3 0.2 5] [20 0.3 0.2 50]
         [10 0.3 0.2 50] [20 0.1 0.05 5]]))

(defn mekanika-2
  "Given m, miu-s, miu-k, F, and teta returns [nilai-gaya-gesek :statis/kinetik]"
  [{:keys [m ms mk f teta]}]
  (let [N (- (* m g) (* f (sin teta)))
        fx (* f (cos teta))
        fs-max (* N ms)]
    (if (> fx fs-max)
      [(bulet (- (* N mk))) :kinetik]
      [(bulet (- fs-max)) :statis])))

(def test-2
  (mapv #(zipmap [:m :ms :mk :f :teta] %)
        [[10 0.3 0.2 5 30] [20 0.3 0.2 50 60]
         [10 0.3 0.2 50 10] [20 0.3 0.2 150 20]]))

(def my-funs
  [mekanika-1 mekanika-2])

(def keterangans
  ["mekanika-1" "mekanika-2"])

(def test-data
  [test-1 test-2])

(defn mekanika-test
  [fn-list]
  (let [score (atom 0)
        total (atom 0)]
    (doseq [i (range (count fn-list))]
      (println "Testing " (nth keterangans i))
      (doseq [data (nth test-data i)]
        (let [ress ((nth fn-list i) data)
              resi ((nth my-funs i) data)]
          (if (not= ress resi)
            (do (println (nth keterangans i) " salah")
                (println "Seharusnya" (str resi) "Jawaban lo:" (str ress))
                (swap! total inc))
            (do (println (nth keterangans i) "Bener!")
                (swap! total inc)
                (swap! score inc))))))
    (println "SCORE :" @score "dari total score yang mungkin" @total)
    (println "Which means elo dapet" (int (* 100 (/ @score @total))) "%")))

