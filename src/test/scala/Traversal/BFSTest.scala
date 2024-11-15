package Traversal

import Graphs.Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BFSTest extends AnyFlatSpec with Matchers {

  val graph: Graph[String] = Graph[String]()
    .addEdge("A", "B")
    .addEdge("A", "C")
    .addEdge("B", "C")
    .addEdge("B", "D")
    .addEdge("C", "D")
    .addEdge("D", "B")
    .addEdge("D", "E")
    .addEdge("D", "F")
    .addEdge("E","B")
    .addEdge("E", "F")

  it should "return visited vertices" in {
    val visitedFromAActual = BFS.iterativeBFS("A", graph, print)
    val visitedFromAExpected = List("A", "B", "C", "D", "E", "F")

    visitedFromAActual.toList.sorted shouldBe visitedFromAExpected
  }

  it should "return empty set for non-existing vertex" in {
    val visitedVertices = BFS.iterativeBFS("V", graph, print)

    visitedVertices shouldBe empty
  }

}
