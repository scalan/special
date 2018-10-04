package scalanizer.collections

import scala.language.reflectiveCalls
import special.wrappers.WrappersModule
import sun.misc.GC

import scalan._
import scalan.util.BenchmarkUtil._

class ColsTests extends BaseCtxTests {
  class Ctx extends TestContext with Library {
    import WArray._
    import ColOverArray._
    import ColOverArrayBuilder._
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
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

  test("measure: create proxy") {
    val ctx = new Ctx {
      override val performViewsLifting = false
    }
    import ctx._
    import Col._
    import ColBuilder._
    import ColOverArrayBuilder._

    val colBuilder: Rep[ColBuilder] = RColOverArrayBuilder()
    var res: Sym = null
    measure(10) { i =>
      for (j <- 0 until 3000) {
        val col = colBuilder.replicate(i*j, 0)
        res = col.map(fun {x => x + 1})
      }
    }
    emit("res", res)
  }

}
