(ns pertamax.test.physics-mekanika
  (:require
    [clojure.test :refer :all]
    [pertamax.physics.mekanika :refer :all]
    [taoensso.timbre :as log]))

(deftest testing-mekanika

  (log/info "Testing mekanika functions")

  (testing "mekanika-1"
    (is (= [java.lang.Double clojure.lang.Keyword]
           (->> (mekanika-1 {:m 10 :ms 0.2 :mk 0.1 :f 20})
                (mapv type))))
    (is (= true
           (let [res (second (mekanika-1 {:m 10 :ms 0.2 :mk 0.1 :f 20}))]
             (or (= :kinetik res) (= :statis res))))))

  (testing "mekanika-2"
    (is (= [java.lang.Double clojure.lang.Keyword]
           (->> (mekanika-2 {:m 10 :ms 0.2 :mk 0.1 :f 20 :teta 200})
                (mapv type))))
    (is (= true
           (let [res (second (mekanika-2 {:m 10 :ms 0.2 :mk 0.1 :f 20 :teta 200}))]
             (or (= :kinetik res) (= :statis res)))))))

