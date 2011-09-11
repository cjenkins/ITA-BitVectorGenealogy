(ns BitVectorGenealogy.core
  (:require [clojure.java.io :as io])
  (:import java.util.BitSet))

(defn create-bitset [line]
  "Takes in a string of 0s and 1s and creates a BitSet out of them."
  (let [bs (BitSet. (count line))]
    (doseq [i (range (count line))]
      (if (= \1 (nth line i))
	(.set bs i true)
	(.set bs i false)))
    bs))

(defn load-bitsets [path]
  "Loads file and converts the line into bitsets."
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)]
      (doall (map create-bitset lines)))))

(defn bitset-difference [^BitSet bs1 ^BitSet bs2]
  "Performs an XOR of the two bitsets and returns the cardinality of
the new bitset."
  (let [new-bitset (.clone bs1)]
    (.xor new-bitset bs2)
    (.cardinality new-bitset)))

(defn create-difference-graph [bitsets]
  "Takes a sequence of bitsets and compares them to each other.  The
differences are returned in n seqs of n length where n is the count
of bitsets passed in."
  (for [bs bitsets]
    (map #(bitset-difference bs %) bitsets)))

(defn create-relative-graph
  [difference-graph mutation-ratio number-of-bits]
  "Takes a difference graph, the probability of mutation upon reproduction
and the number of bits in each BitVector being and converts the difference
graph values into a ratio where 1.0 is an ideal relative and the values
range higher and lower from there."
  (let [ideal-relative-value (* number-of-bits mutation-ratio)]
    (for [line difference-graph]
      (map #(/ (Math/abs (- % ideal-relative-value)) ideal-relative-value) line))))

;Calculate minimum spanning tree of graph



;Topological sort to get the ordering
