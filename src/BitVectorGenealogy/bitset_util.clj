(ns BitVectorGenealogy.bitset-util
  (:import java.util.BitSet))

(defn create-bitset [line]
  "Takes in a string of 0s and 1s and creates a BitSet out of them."
  (let [bs (BitSet. (count line))]
    (doseq [i (range (count line))]
      (if (= \1 (nth line i))
	(.set bs i true)
	(.set bs i false)))
    bs))

(defn- bitset-difference [^BitSet bs1 ^BitSet bs2]
  "Performs an XOR of the two bitsets and returns the cardinality of
the new bitset.  If bs1 and bs2 are the same objects returns
Integer/MAX_VALUE."
  (if (identical? bs1 bs2)
    Integer/MAX_VALUE
    (let [new-bitset (.clone bs1)]
      (.xor new-bitset bs2)
      (.cardinality new-bitset))))

(defn create-difference-graph [bitsets]
  "Takes a sequence of bitsets and compares them to each other.  The
differences are returned in n seqs of n length where n is the count
of bitsets passed in.  Each value y in each seq x is the number of bits
that were different between bitset x and y.  A bitset has
Integer/MAX_VALUE bits different from itself as it's not possible for a
bitset to be its own ancestor."
  (for [bs bitsets]
    (map #(bitset-difference bs %) bitsets)))