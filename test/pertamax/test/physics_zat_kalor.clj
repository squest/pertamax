(ns pertamax.test.physics-termo
  (:require
    [pertamax.utils :refer :all]
    [clojure.test :refer :all]
    [pertamax.physics.zat-kalor :refer :all]
    [taoensso.timbre :as log]))

(deftest kalor
  (testing "Kalor"
    (log/info "Testing kalor 1")
    (is (= 100.0 (tester kalor-1 kalor-1 data-kalor-1)))
    (log/info "Testing kalor 2")
    (is (= 100.0 (tester kalor-2 kalor-2 data-kalor-2)))))

