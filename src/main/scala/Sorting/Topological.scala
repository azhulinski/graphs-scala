package Sorting

import Graphs.Graph

import scala.annotation.tailrec

object Topological {

  def sortKahn[V](graph: Graph[V]): List[V] = {

    def buildInDegreeMap[V](graph: Graph[V]): Map[V, Int] = {
      val zeroMap = graph.vertices.map(n => n -> 0).toMap
      val edgesCnt = graph.edges.groupBy(e => e._2)
        .map {
          case (vertex, edges) => vertex -> edges.size
        }

      zeroMap ++ edgesCnt
    }


    val inDegree: Map[V, Int] = buildInDegreeMap(graph)
    val startVertices = inDegree.filter(_._2 == 0)

    if (startVertices.isEmpty) throw new Exception("Graph contains cycles!")

    @tailrec
    def sort(startVertices: Map[V, Int], result: List[V], inDegree: Map[V, Int]): List[V] = {

      if (startVertices.isEmpty) result
      else {
        val firstVertex = startVertices.head._1
        val neighbours = graph.neighbours(firstVertex)

        // decrease value of incoming edges for first vertex's neighbours
        val newInDegree = neighbours.foldLeft(inDegree)((acc, curr) => adjust(acc, curr)(_ - 1))

        // neighbours which have now no incoming edge
        val zeroNeighbours = newInDegree.filter({ case (k, v) => neighbours.contains(k) && v == 0 })
        val newStartVertices = startVertices.removed(firstVertex) ++ zeroNeighbours

        sort(newStartVertices, result :+ firstVertex, newInDegree)
      }
    }

    sort(startVertices, List[V](), inDegree)

  }

  def sortDFS[V](graph: Graph[V]): List[V] = {

    case class DfsStep[N](visited: Set[N], sort: List[N])

    def sortDfsRec(vertex: V, dfsStep: DfsStep[V]): DfsStep[V] = {
      if (dfsStep.visited.contains(vertex)) dfsStep
      else {
        val preDfsStep = dfsStep.copy(visited = dfsStep.visited + vertex)
        val postDfsStep = graph.neighbours(vertex)
          .foldLeft(preDfsStep)((step, n) => sortDfsRec(n, step))

        postDfsStep.copy(sort = vertex +: postDfsStep.sort)
      }
    }

    // call sort for every vertex
    graph.vertices.foldLeft(DfsStep(Set[V](), Nil))((step, n) => sortDfsRec(n, step)).sort

  }

  private def adjust[A, B](m: Map[A, B], k: A)(f: B => B): Map[A, B] = m.updated(k, f(m(k)))

}
