package Graphs

case class WeightedEdge[V](destination: V, weight: Int)

class WeightedGraph[V](adjList: Map[V, List[WeightedEdge[V]]]) extends Graph[V] {

  override def vertices: List[V] = adjList.keys.toList

  override def edges: List[(V, V)] = adjList.flatMap {
    case (vertex, edges) => edges.map(edge => (vertex, edge.destination))
  }.toList

  override def addVertex(vertex: V): WeightedGraph[V] = new WeightedGraph(adjList + (vertex -> List()))

  def addEdge(from: V, weightedEdge: WeightedEdge[V]): WeightedGraph[V] = {
    val fromNeighbours = weightedEdge +: adjList.getOrElse(from, Nil)
    val graph = new WeightedGraph(adjList + (from -> fromNeighbours))

    val to = weightedEdge.destination
    if(graph.vertices.contains(to)) graph else graph.addVertex(to)
  }

  override def addEdge(from: V, to: V): WeightedGraph[V] = addEdge(from, WeightedEdge(to, 0))

  override def neighbours(vertex: V): List[V] = adjList.getOrElse(vertex, Nil).map(_.destination)

  def neighboursWithWeights(vertex: V): List[WeightedEdge[V]] = adjList.getOrElse(vertex, Nil)

}
