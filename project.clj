(defproject zenedu/pertamax "0.1.5"
  :description "Functions to test math/physics/chem problems"
  :url "https://github.com/squest"
  :license {:name "Eclipse Public License"
            :url  "https://github.com/squest"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.taoensso/timbre "4.1.4"]
                 [gorilla-plot "0.1.3"]]
  :plugins [[lein-gorilla "0.3.5"]]
  :repositories [["releases" {:url   "http://clojars.org/repo"
                              :creds :gpg}]])
