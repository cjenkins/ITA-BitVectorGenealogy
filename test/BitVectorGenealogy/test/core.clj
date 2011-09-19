(ns BitVectorGenealogy.test.core
  (:require [BitVectorGenealogy.io :as io])
  (:use [BitVectorGenealogy.core] :reload)
  (:use [clojure.test]))

(def small-data "./Data/bitvectors-genes.data.small")

(def result-data "./Data/bitvectors-parents.data.small.txt")

(deftest result-test
  (is (= (solve-ita-problem small-data)
	 (io/load-results result-data))))
