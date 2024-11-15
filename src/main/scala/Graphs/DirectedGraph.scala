package Graphs

class DirectedGraph[V](adjList: Map[V, List[V]]) extends Graph[V] {

  override def vertices: List[V] = adjList.keys.toList

  override def edges: List[(V, V)] = adjList.flatMap {
    case (vertex, neighbours) => neighbours.map(neighbour => (vertex, neighbour))
  }.toList

  override def addVertex(vertex: V): Graph[V] = new DirectedGraph(adjList + (vertex -> List()))

  override def addEdge(from: V, to: V): Graph[V] = {
    val fromNeighbours = to +: neighbours(from)
    val graph = new DirectedGraph(adjList + (from -> fromNeighbours))

    if (graph.vertices.contains(to)) graph else graph.addVertex(to)
  }

  override def neighbours(vertex: V): List[V] = adjList.getOrElse(vertex, Nil)
}
