package CycleDetection

import Graphs.Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CycleDetectionTest extends AnyFlatSpec with Matchers {

  val bigCycle: Graph[String] = Graph[String]()
    .addEdge("A", "B")
    .addEdge("B", "C")
    .addEdge("C", "D")
    .addEdge("D", "A")

  val withCycle: Graph[String] = Graph[String]()
    .addEdge("A", "B")
    .addEdge("B", "C")
    .addEdge("C", "D")
    .addEdge("D", "B")

  val withoutCycle: Graph[String] = Graph[String]()
    .addEdge("A", "B")
    .addEdge("B", "C")
    .addEdge("B", "D")

  val badForFloyd: Graph[String] = Graph[String]()
    .addEdge("A", "C")
    .addEdge("A", "B")
    .addEdge("B", "C")
    .addEdge("C", "D")
    .addEdge("D", "E")
    .addEdge("E", "F")
    .addEdge("F", "G")
    .addEdge("G", "H")
    .addEdge("H", "I")
    .addEdge("I", "J")
    .addEdge("J", "K")

  "DFS - Graph" should "contain cycle - empty start vertices" in {
    CycleDetection.containsCycleDFS(bigCycle) shouldBe true
  }

  "DFS - Graph" should "contain cycle" in {
    CycleDetection.containsCycleDFS(withCycle) shouldBe true
  }

  "DFS - Graph" should "not contain cycle" in {
    CycleDetection.containsCycleDFS(withoutCycle) shouldBe false
  }

  "Floyd - Graph" should "contain cycle" in {
    CycleDetection.containsCycleFloyd("A", withCycle) shouldBe true
  }

  "Floyd - Graph" should "not contain cycle" in {
    CycleDetection.containsCycleFloyd("A", withoutCycle) shouldBe false
  }

  "Floyd - Graph" should "contain cycle - false positive" in {
    // graph is not cyclic, result is false positive
    CycleDetection.containsCycleFloyd("A", badForFloyd) shouldBe true
  }

}
