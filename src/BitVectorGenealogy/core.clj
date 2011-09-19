(ns BitVectorGenealogy.core
  (:require [BitVectorGenealogy.io :as io]
	    [BitVectorGenealogy.bitset-util :as bs])
  (:import (org.jgrapht.graph SimpleWeightedGraph DefaultWeightedEdge Subgraph)
	   (org.jgrapht.alg KruskalMinimumSpanningTree)))

(defn create-distance-graph
  [difference-graph mutation-ratio]
  "Takes a difference graph and the probability of mutation upon
reproduction converts the difference graph values into a distance where
0 is an ideal relative."
  (let [ideal-relative-value (* (count difference-graph) mutation-ratio)]
    (for [line difference-graph]
      (map #(Math/abs (- % ideal-relative-value)) line))))

(defn- to-jgrapht-graph [distance-graph]
  "Converts a distance graph into a jgrapht graph for further processing."
  (let [graph (SimpleWeightedGraph. DefaultWeightedEdge)
	indexes (range (count distance-graph))]
    (doseq [idx indexes]
      (.addVertex graph idx))
    (doseq [x indexes]
      ;Undirected graph so it's symmetric
      (doseq [y (range (inc x) (count distance-graph))]
	(let [edge (.addEdge graph x y)]
	  (.setEdgeWeight graph edge (nth (nth distance-graph y) x)))))
    graph))

(defn minimum-spanning-tree [distance-graph]
  "Takes a distance-graph and finds the minimum spanning tree of it.  Returns
the subgraph of the distance graph."
  (let [jgrapht-graph (to-jgrapht-graph distance-graph)
	mst (KruskalMinimumSpanningTree. jgrapht-graph)]
    (Subgraph. jgrapht-graph (.vertexSet jgrapht-graph) (.getEdgeSet mst))))

(defn- other-edge-vertex-of [graph vertex]
  "Returns the other vertex in the first edge found of the passed in vertex."
  (if (not= vertex (.getEdgeSource graph (first (.edgesOf graph vertex))))
    (.getEdgeSource graph (first (.edgesOf graph vertex)))
    (.getEdgeTarget graph (first (.edgesOf graph vertex)))))

(defn- remove-all-vertices! [graph vertices]
  "Removes all vertices from the passed in graph and returns it.  This is done
in place via mutaion on graph!"
  (doseq [vertex vertices]
    (.removeVertex graph vertex))
  graph)

(defn calculate-parent-child [mst-graph]
  "Takes a minimum spanning tree graph and determines all of the parent child
relationships.  Returns a map of child to parent.

This is dones by starting at the outer edges of the graph with the leaf nodes
and working our way in.  Each leaf node has its parent marked and is removed
from the graph.  This process is then repeated iteratively until we are left
with the root node(s) of the graph."
  (loop [relationship-map {}
	 current-graph (Subgraph. mst-graph (.vertexSet mst-graph) (.edgeSet mst-graph))]
    ;Get all vertices with only a single edge
    (let [single-vertices (filter #(= 1 (count (.edgesOf current-graph %)))
				  (.vertexSet current-graph))]
      ;Continue until we're out of vertices
      (if (> (count single-vertices) 0)
	;Set the child->parent of those vertices to edgeSource->edgeTarget
	(recur (reduce #(assoc %1 %2 (other-edge-vertex-of current-graph %2))
		       relationship-map single-vertices)
	       ;Remove those vertices
	       (remove-all-vertices! current-graph single-vertices))
	;Assign root values to the remaining vertices
	(reduce #(assoc %1 %2 -1) relationship-map (.vertexSet current-graph))))))

(defn- to-ita-format [parent-child-map]
  "Converts the results to the prescribed ITA format of a list of numbers where
each number is the 0-based index of the parent of that BitVector.  The progenitor
BitVector has a value of -1."
  (map #(second %1) (sort parent-child-map)))

(defn solve-ita-problem [bitset-filepath]
  "Load bitsets from file.

Calculate the difference from each bitset to each other bitset.

Calculate a measure of how close a bitset is to an ideal relative for each other bitset.

Calculate the minimum spanning tree of the ideal relative graph to find the most probable
anscestry path through the BitVectors.

Identify the parent-child directional relationship by working our way from the outside of
the minimum spanning tree graph inwards, marking child->parent relationships as we go."
  (let [bitsets (io/load-bitsets bitset-filepath)
	diff-graph (bs/create-difference-graph bitsets)
	dist-graph (create-distance-graph diff-graph 0.20)
	mst-graph (minimum-spanning-tree dist-graph)]
    (to-ita-format (calculate-parent-child mst-graph))))