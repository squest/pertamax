(ns pertamax.test.physics-mekanika
  (:require
    [clojure.test :refer :all]
    [pertamax.physics.mekanika :refer :all]
    [taoensso.timbre :as log]))

(deftest testing-mekanika

  (log/info "Testing mekanika functions")

  (testing "pasti jalan"
    (is (= 2 (+ 1 1)))))

