package Traversal

import Graphs.Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DFSTest extends AnyFlatSpec with Matchers {

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

  "Recursive traversal" should "return visited vertices" in {
    val visitedFromAActual = DFS.recursiveDFS("A", graph, print)
    val visitedFromAExpected = List("A", "B", "C", "D", "E", "F")

    visitedFromAActual.toList.sorted shouldBe visitedFromAExpected
  }

  "Recursive traversal" should "return empty set for non-existing vertex" in {
    val visitedVertices = DFS.recursiveDFS("V", graph, print)

    visitedVertices shouldBe empty
  }

  "Iterative traversal" should "return visited vertices" in {
    val visitedFromAActual = DFS.iterativeDFS("A", graph, print)
    val visitedFromAExpected = List("A", "B", "C", "D", "E", "F")

    visitedFromAActual.toList.sorted shouldBe visitedFromAExpected
  }

  "Iterative traversal" should "return empty set for non-existing vertex" in {
    val visitedVertices = DFS.iterativeDFS("V", graph, print)

    visitedVertices shouldBe empty
  }

}
