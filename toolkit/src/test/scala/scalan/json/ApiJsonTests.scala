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

    def testLam[A, B](f: Rep[A => B], fileName: String = ""): Unit = {
      val g = new PGraph(f)
      test(g, fileName)
    }

    testLam(fun { xs: Rep[Coll[Int]] => xs.length }, "lambda with Coll argument")
    testLam(
      fun { p: Rep[(Coll[Int], Coll[Int])] =>
        val Pair(xs, ys) = p
        xs.length + ys.length
      }, "lambda with (Coll,Coll) argument")
  }
}
