(ns pertamax.physics.mekanika
  (:require [pertamax.utils :refer :all]))

(def g -9.8)

(def tester tester-utils)

(defn newton-1
  "Sebuah balok bermasa m diberi gaya f dengan sudut alfa terhadap
  sumbu x positif berada pada bidang dengan sudut teta thdp sbx+
  dengan ms sbg miu-s dan mk miu-k antara balok dan bidang. Outputnya
  [besar-f-gesek :statis/:kinetis]. Expected input :m :ms :mk :f :alfa
  :teta."
  [{:keys [m ms mk f alfa teta]
    :or {alfa 0 teta 0}
    :as data}]
  (let [tets (- alfa teta)
        w (* m g)
        wke-bidang (* w (cos teta))
        wke-datar (* w (sin teta))
        fke-bidang (* f (sin tets))
        tekan (+ wke-bidang fke-bidang)
        fke-datar (* f (cos tets))
        N (- tekan)
        fs-max (* N ms)
        ftot (+ fke-datar wke-datar)]
    (cond
      (<= N 0)
      [0.0 :fly-high]
      (>= (abs ftot) (abs fs-max))
      [(bulet (abs (* N mk))) :kinetik]
      :else
      [(bulet (abs ftot)) :statis])))





