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
    lazy val t2 = fun { (xs: Rep[WArray[Double]]) =>
      val c = ColOverArray(xs)
      c.map(fun { x => x + 1.0 })
    }
  }

  test("Col methods") {
    val ctx = new Ctx {
      val M = WArrayMethods; val C = WArrayCompanionMethods
      def test() = {
        { val Def(Lambda(_, _, x, ColOverArray(M.map(in, _)))) = t2; assert(in == x) }
      }
    }
    ctx.test()
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
  }

}
