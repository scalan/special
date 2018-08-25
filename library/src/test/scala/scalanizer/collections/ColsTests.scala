package scalanizer.collections

import scala.language.reflectiveCalls
import special.wrappers.WrappersModule
import scalan._

class ColsTests extends BaseCtxTests {
  class Ctx extends TestContext with Library {
    import WArray._
    import ColOverArray._
    import ColOverArrayBuilder._
    lazy val t1 = fun { (xs: Rep[WArray[Double]]) =>
      val b = RColOverArrayBuilder()
      b.ddmvm(xs)
    }
    lazy val t2 = fun { (xs: Rep[WArray[Double]]) =>
      val c = RColOverArray(xs)
      c.map(fun { x => x + 1.0 })
    }
    lazy val t3 = fun { (x: Rep[Int]) =>
      val b = RColOverArrayBuilder()
      b.apply(x, x + 1, x + 2)
    }
  }

  test("Col methods") {
    val ctx = new Ctx {
      import WArray._
      import ColOverArray._
      import ColOverArrayBuilder._
      val M = WArrayMethods; val C = WArrayCompanionMethods
      def test() = {
//        { val Def(Lambda(_, _, x, RColOverArray(M.map(in, _)))) = t2; assert(in == x) }
      }
    }
    ctx.test()
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

}
