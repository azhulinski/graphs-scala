package Graphs

trait Graph[V] {

  def vertices: List[V]

  def edges: List[(V, V)]

  def addVertex(vertex: V): Graph[V]

  def addEdge(from: V, to: V): Graph[V]

  def neighbours(vertex: V): List[V]

}

object Graph {

  def apply[V](adjList: Map[V, List[V]]): Graph[V] = new DirectedGraph(adjList)

  def apply[V](): Graph[V] = new DirectedGraph(Map[V, List[V]]())

}
