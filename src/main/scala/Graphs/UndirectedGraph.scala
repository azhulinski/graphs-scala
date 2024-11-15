package Graphs

class UndirectedGraph[V](adjList: Map[V, List[V]]) extends DirectedGraph[V](adjList) {

  override def addVertex(vertex: V): UndirectedGraph[V] = new UndirectedGraph(adjList + (vertex -> List()))

  override def addEdge(from: V, to: V): UndirectedGraph[V] = {
    val fromNeighbours = to +: neighbours(from)
    val toNeighbours = from +: neighbours(to)
    val graph = new UndirectedGraph(adjList ++ Map(from -> fromNeighbours, to -> toNeighbours))

    if(graph.vertices.contains(to)) graph else graph.addVertex(to)
  }

}
