package CycleDetection

import Graphs.Graph

import scala.annotation.tailrec

object CycleDetection {

  def containsCycleDFS[V](graph: Graph[V]): Boolean = {

    case class DfsCycleResult[V](visited: Set[V], isCyclic: Boolean = false)

    def containsCycleDFSRec(vertex: V, visited: Set[V] = Set[V](), ancestors: Set[V] = Set[V]()): DfsCycleResult[V] = {
      if (ancestors.contains(vertex))
        DfsCycleResult(visited, isCyclic = true)
      else if (visited.contains(vertex))
        DfsCycleResult(visited)
      else {
        graph.neighbours(vertex).foldLeft(DfsCycleResult(visited + vertex)) {
          case (DfsCycleResult(v, true), _) => DfsCycleResult(v, isCyclic = true)
          case (acc, v) => containsCycleDFSRec(v, acc.visited, ancestors + vertex)
        }
      }
    }

    // vertices that don't have an edge pointed towards them
    val startVertices = graph.vertices.filter(n => graph.edges.forall(e => e._2 != n))

    startVertices.isEmpty || startVertices.exists(n => containsCycleDFSRec(n).isCyclic)
  }

  def containsCycleFloyd[V](vertex: V, graph: Graph[V]): Boolean = {

    case class Race(tortoise: List[V], hare: List[V])

    //move one step
    def moveOnce(stack: List[V]): List[V] =
      stack.headOption.map(vertex => graph.neighbours(vertex) ++ stack.tail).getOrElse(Nil)

    // compare heads of tortoise and hare stacks
    def compareHeads(tortoiseHead: Option[V], hareHead: Option[V]): Boolean = (tortoiseHead, hareHead) match {
      case (Some(t), Some(h)) => t == h
      case _ => false
    }

    @tailrec
    def containsCycleFloydTailrec(race: Race): Race = {
      if (race.hare.isEmpty || compareHeads(race.tortoise.headOption, race.hare.headOption))
        race
      else
        containsCycleFloydTailrec(Race(
          moveOnce(race.tortoise),
          moveOnce(moveOnce(race.hare))
        ))
    }

    val race = containsCycleFloydTailrec(Race(
      moveOnce(List(vertex)),
      moveOnce(moveOnce(List(vertex)))
    ))

    compareHeads(race.tortoise.headOption, race.hare.headOption)

  }

}
