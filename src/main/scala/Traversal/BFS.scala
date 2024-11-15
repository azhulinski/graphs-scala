package Traversal

import Graphs.Graph

import scala.collection.immutable.Queue

object BFS {

  def iterativeBFS[V](start: V, graph: Graph[V], f: V => Unit): Set[V] = {

    if (!graph.vertices.contains(start))
      Set()
    else {
      // LazyList - an immutable linked list that evaluates elements in order and only when needed
      LazyList.iterate((Queue(start), Set[V](start))) {
        case (queue, visited) => {
          // get the first element of the queue
          val (vertex, rest) = queue.dequeue
          // new queue will contain non visited neighbours of `vertex` and the rest of the queue
          val newQueue = rest.enqueueAll(graph.neighbours(vertex).filterNot(visited.contains))
          // add all neighbours of `vertex` to `the visited` set
          val newVisited = graph.neighbours(vertex).toSet ++ visited

          (newQueue, newVisited)
        }
      }.takeWhile(tuple => tuple._1.nonEmpty).foldLeft(Set[V]())((acc, curr) => {
        val head = curr._1.head
        f(head)
        acc ++ Set(head)
      })
    }

  }

}
