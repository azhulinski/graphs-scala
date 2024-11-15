package Traversal

import Graphs.Graph

object DFS {

  def recursiveDFS[V](start: V, graph: Graph[V], f: V => Unit, visited: Set[V] = Set[V]()): Set[V] = {

    def recursiveDFSRec(vertex: V, newVisited: Set[V]): Set[V] = {
      if (newVisited.contains(vertex))
        newVisited
      else {
        f(vertex)
        graph.neighbours(vertex).foldLeft(newVisited + vertex)((allVisited, neighbour) =>
          recursiveDFSRec(neighbour, allVisited)
        )
      }
    }

    if (!graph.vertices.contains(start))
      Set()
    else {
      recursiveDFSRec(start, visited)
    }

  }

  def iterativeDFS[V](start: V, graph: Graph[V], f: V => Unit): Set[V] = {
    if (!graph.vertices.contains(start))
      Set()
    else {
      // LazyList - an immutable linked list that evaluates elements in order and only when needed
      LazyList.iterate((List(start), Set[V](start))) {
        case (stack, visited) =>
          // get head of the stack
          val vertex = stack.head
          // new stack will contain non visited neighbours of `vertex` and the rest of the stack
          val newStack = graph.neighbours(vertex).filterNot(visited.contains) ++ stack.tail
          // add all neighbours of `vertex` to `the visited` set
          val newVisited = graph.neighbours(vertex).toSet ++ visited
          (newStack, newVisited)
      }.takeWhile(tuple => tuple._1.nonEmpty).foldLeft(Set[V]())((acc, curr) => {
        val head = curr._1.head
        f(head)
        acc ++ Set(head)
      })
    }
  }

}
