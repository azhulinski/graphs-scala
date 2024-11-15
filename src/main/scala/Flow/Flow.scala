package Flow

import scala.collection.immutable.Queue

sealed trait MaxFlow {
  def buildPath[V](child: V, parents: Map[V, V]): List[(V, V)]

  def findPathBFS[V](source: V, target: V, graph: Matrix[V]): List[(V, V)]

  def maxFlow[V](residualGraph: Matrix[V], source: V, target: V, flow: Int): Int
}

object Flow extends MaxFlow {

  def buildPath[V](child: V, parents: Map[V, V]): List[(V, V)] = {
    parents.get(child).map(p => (p, child) +: buildPath(p, parents)).getOrElse(Nil)
  }

  def findPathBFS[V](source: V, target: V, graph: Matrix[V]): List[(V, V)] = {
    val sq = LazyList.iterate((Queue(source), Set(source), Map[V, V]())) {
      case (queue, visitedVertices, parents) =>
        val (vertex, tail) = queue.dequeue
        val neighbours: Set[V] = graph.neighbours(vertex).toSet -- visitedVertices
        val newQueue = tail ++ neighbours
        val newVisited = neighbours ++ visitedVertices
        val newParents = parents ++ neighbours.map(_ -> vertex)

        (newQueue, newVisited, newParents)
    }
    val parentsMap: Map[V, V] = sq.takeWhile(q => q._1.nonEmpty).last._3
    buildPath(target, parentsMap).reverse
  }

  def maxFlow[V](residualGraph: Matrix[V], source: V, target: V, flow: Int = 0): Int = {
    val path = findPathBFS(source, target, residualGraph)

    if (path.nonEmpty) {
      val minPathFlow = path.map {
        case (u, v) => residualGraph.edgeAt(u, v)
      }.min

      val newResidualGraph = path.foldLeft(residualGraph) {
        case (resGraph, (eA, eB)) =>
          val flowForward = resGraph.edgeAt(eA, eB)
          val flowBackward = resGraph.edgeAt(eB, eA)

          resGraph.update(eA, eB, flowForward - minPathFlow)
                  .update(eB, eA, flowBackward + minPathFlow)
      }
      val newFlow = flow + minPathFlow
      maxFlow(newResidualGraph, source, target, newFlow)
    }
    else {
      flow
    }
  }

}
