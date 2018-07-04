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
    lazy val t3 = fun { (x: Rep[Int]) =>
      val b = ColOverArrayBuilder()
      b.apply(x, x + 1, x + 2)
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
    ctx.emit("t3", ctx.t3)
  }

}
