package ShortestPath

import Graphs.{WeightedEdge, WeightedGraph}

import scala.util.Try

object Dijkstra {

  case class ShortStep[V](parents: Map[V, V],
                          unprocessed: Set[V],
                          distances: Map[V, Int]) {
    def extractMin(): Option[(V, Int)] = Try(unprocessed.minBy(v => distances(v))).toOption.map(v => (v, distances(v)))
  }

  def findShortestPaths[V](source: V, graph: WeightedGraph[V]): ShortStep[V] = {

    // set distances to all vertices to infinity, distance to source vertex is zero
    val sDistances: Map[V, Int] = graph.vertices.map(_ -> Int.MaxValue).toMap + (source -> 0)

    def shortestPath(step: ShortStep[V]): ShortStep[V] = {
      // get vertices with minimal value
      step.extractMin().map {
        case (vertex, currentDistance) =>
          val newDist = graph.neighboursWithWeights(vertex).collect {
            case WeightedEdge(neighbour, neighbourDistance) if step.distances.get(neighbour).exists(_ > currentDistance + neighbourDistance) =>
              neighbour -> (currentDistance + neighbourDistance)
          }

          val newParents = newDist.map { case (neighbour, _) => neighbour -> vertex }

          shortestPath(ShortStep(
            step.parents ++ newParents,
            step.unprocessed - vertex, // current vertex is processed
            step.distances ++ newDist))
      }.getOrElse(step)
    }

    shortestPath(ShortStep(Map(), graph.vertices.toSet, sDistances))
  }

  private def findPathRec[V](vertex: V, parents: Map[V, V]): List[V] =
    parents.get(vertex).map(parent => vertex +: findPathRec(parent, parents)).getOrElse(List(vertex))

  def findPath[V](destination: V, parents: Map[V, V]): List[V] = {
    findPathRec(destination, parents).reverse
  }

}
