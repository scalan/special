package scalan.json

import scalan.TestLibrary

class ApiJsonTests extends JsonTests {

  class Ctx extends ToolkitScalan with TestLibrary

  describe("Wrapped methods <-> Json") {
    val tester = getTester(new Ctx)
    import tester._
    import protocol._
    import ctx._
    import Coll._

    def testLam[A, B](f: Ref[A => B], fileName: String = ""): Unit = {
      val g = new PGraph(f)
      test(g, fileName)
    }

    testLam(fun { xs: Ref[Coll[Int]] => xs.length }, "lambda with Coll argument")
    testLam(
      fun { p: Ref[(Coll[Int], Coll[Int])] =>
        val Pair(xs, ys) = p
        xs.length + ys.length
      }, "lambda with (Coll,Coll) argument")
  }
}
