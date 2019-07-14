package scalan.util

import scalan.BaseNestedTests
import debox.{Set => DSet, Buffer => DBuffer}

class GraphUtilTests extends BaseNestedTests {
  import GraphUtil._

  describe("Collecting dependencies") {
    val graph = Array(
      List(1, 2), // 0
      List(3),    // 1
      List(4),    // 2
      List(5, 6), // 3
      List(6),    // 4
      List(6),    // 5
      List()      // 6
    )

    object neighbours extends NeighbourFunc[Int] {
      def get(node: Int, ns: DBuffer[Int]): Unit = {
        graph(node) foreach (ns.+=)
      }
    }

    it("depthFirstSetFrom") {
      depthFirstSetFrom(DBuffer(6))(neighbours) shouldBe (DSet(6))
      depthFirstSetFrom(DBuffer(5))(neighbours) shouldBe (DSet(5, 6))
      depthFirstSetFrom(DBuffer(3))(neighbours) shouldBe (DSet(3, 5, 6))
      depthFirstSetFrom(DBuffer(2))(neighbours) shouldBe (DSet(2, 4, 6))
      depthFirstSetFrom(DBuffer(0))(neighbours) shouldBe (DSet(0, 1, 2, 3, 4, 5, 6))
    }
  }
  describe("StronglyConnectedComponents") {
    val graph: String => List[String] = {
      case "A" => List("B")
      case "B" => List("C")
      case "C" => List("A", "D")
      case "D" => Nil
    }
    def test(startNodes: Array[String], graph: String => List[String]): Seq[Set[String]] = {
      val result = stronglyConnectedComponents(startNodes)(n => graph(n).toArray).toArray.map(_.toIterable().toSet)
      result
    }
    it("accessAll") {
      val expected = Seq(Set("D"), Set("A", "B", "C"))
      val result = test(Array("A"), graph)
      result.shouldEqual(expected)
    }
    it("accessOne") {
      val result = test(Array("D"), graph)
      val expected = Seq(Set("D"))
      result.shouldEqual(expected)
    }
    it("accessAllByManyStarts") {
      val result = test(Array("A", "B", "C"), graph)
      val expected = Seq(Set("D"), Set("A", "B", "C"))
      result.shouldEqual(expected)
    }
    it("manyComponents") {
      val result = test(Array("D"), {
        case "D" => List("A")
        case "A" => List("B", "C")
        case "B" => List("A", "C")
        case "C" => List("A", "B", "E")
        case "E" => List("F")
        case "F" => List("E")
      })
      val expected = Seq(Set("E", "F"), Set("A", "B", "C"), Set("D"))
      result.shouldEqual(expected)
    }
    val dag: String => List[String] = {
      case "A" => List("B", "C")
      case "B" => List("C")
      case "C" => Nil
      case "D" => List("E", "F")
      case "E" => List("G", "H")
      case "F" => Nil
      case "G" => Nil
      case "H" => Nil
    }
    it("topologicallySortDag") {
      val result = test(Array("A"), dag)
      result.flatten.shouldEqual(Seq("C", "B", "A"))
    }
    it("allRootsTopologicallySortDag") {
      val result = test(Array("A", "B", "C"), dag)
      result.flatten.shouldEqual(Seq("C", "B", "A"))
    }
    it("topologicallySortTree") {
      val result = test(Array("D"), dag)
      result.flatten.shouldEqual(Seq("G", "H", "E", "F", "D"))
    }
    val forest: String => List[String] = {
      case "A" => List("B", "C")
      case "B" => List("C")
      case "C" => Nil
      case "D" => List("F")
      case "E" => List("F")
      case "F" => Nil
      case "G" => Nil
      case "H" => List("G")
    }
    it("topologicallySortForest") {
      val result = test(Array("A", "B", "C", "D", "E", "F", "G", "H"), forest)
      result.flatten.shouldEqual(Seq("C", "B", "A", "F", "D", "E", "G", "H"))
    }
  }
}
