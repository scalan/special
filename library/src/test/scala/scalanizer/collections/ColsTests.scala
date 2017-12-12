package scalanizer.collections

import scala.language.reflectiveCalls
import scala.wrappers.WrappersModule
import scalan._
import scalan.collection.ColsModule

class ColsTests extends BaseCtxTests {
  class Ctx extends TestContext with Library {
    lazy val t1 = fun { (xs: Rep[WArray[Double]]) =>
      val b = ColOverArrayBuilder()
      b.ddmvm(xs)
    }
  }

  test("Col methods") {
    val ctx = new Ctx {
      val M = WArrayMethods; val C = WArrayCompanionMethods
      def test() = {
      }
    }
    ctx.test()
    ctx.emit("t1", ctx.t1)
  }

}
