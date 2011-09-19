(ns BitVectorGenealogy.io
  (:require [clojure.java.io :as io]
	    [BitVectorGenealogy.bitset-util :as bs]))

(defn load-bitsets [path]
  "Loads file and converts the line into bitsets."
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)]
      (doall (map bs/create-bitset lines)))))

(defn load-results [path]
  "Loads file and reads in ITA result format.

One number per line with the number being the 0-based index of that
BitVector's parent or -1 if it is the progenitor."
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)]
      (doall (map #(Integer/parseInt %) (filter #(> (count %) 0) lines))))))