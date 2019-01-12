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
      b.fromItems(x, x + 1, x + 2)
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

  test("measure: build graph and resetContext") {
    val ctx = new Ctx {
      override val performViewsLifting = false
      useAlphaEquality = false
    }
    import ctx._
    import Col._
    import ColBuilder._
    import ColOverArrayBuilder._

    var res: Sym = null
    measure(10) { i =>
      for (j <- 0 until 3000) {
        val colBuilder: Rep[ColBuilder] = RColOverArrayBuilder()
        val col = colBuilder.replicate(i*j, 0)
        res = col.map(fun {x => x + 1})
      }
      println(s"Defs: ${ctx.defCount}")
      ctx.resetContext()
    }
    emit("res", res)
  }

  test("measure: build graph with new context") {
    measure(10) { i =>
      var sum: Int = 0
      for (j <- 0 until 3000) {
        val ctx = new Ctx {
          override val performViewsLifting = false
          useAlphaEquality = false
        }
        import ctx._
        import Col._
        import ColBuilder._
        import ColOverArrayBuilder._
        val colBuilder: Rep[ColBuilder] = RColOverArrayBuilder()
        val col = colBuilder.replicate(i*j, 0)
        val res = col.map(fun {x => x + 1})
        sum += ctx.defCount
      }
      println(s"Defs: ${sum}")
    }
  }

  def runMeasure(name: String, alphaEq: Boolean, keepOrig: Boolean, unfoldWithOrig: Boolean) = {
    println(s"runMeasure($name, alphaEq = $alphaEq, keepOrig = $keepOrig, unfoldWithOrig = $unfoldWithOrig)")
    val nIters = 10
    val nRepeats = 1000
    def warmUp(i: Int) = {
      val ctx = new Ctx {
        override val performViewsLifting = false
        useAlphaEquality = alphaEq
        keepOriginalFunc = keepOrig
        unfoldWithOriginalFunc = unfoldWithOrig
      }
      import ctx._
      import Col._
      import ColBuilder._
      import ColOverArrayBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Rep[(ColBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, 0)
          val res = col.map(fun {x => x + delta})
          res
        }
        outGraph = Pair(f, f(Pair(RColOverArrayBuilder(), 1)))
      }
    }
    def measureUp(i: Int) = {
      val ctx = new Ctx {
        override val performViewsLifting = false
        useAlphaEquality = alphaEq
        keepOriginalFunc = keepOrig
        unfoldWithOriginalFunc = unfoldWithOrig
      }
      import ctx._
      import Col._
      import ColBuilder._
      import ColOverArrayBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Rep[(ColBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, 0)
          val res = col.map(fun {x => x + delta})
          res
        }
        outGraph = Pair(f, f(Pair(RColOverArrayBuilder(), 1)))
      }
      println(s"Defs: ${ctx.defCount}")

      if (i == nIters - 1)
        emit(name, outGraph)
    }
    measure(nIters)(warmUp)
    measure(nIters)(measureUp)
  }

  test("measure: unfoldLambda") {
    val dummyCtx = new Ctx  // to force class loading
    runMeasure("default", true, true, true)
    runMeasure("noAlpha", false, true, true)
    runMeasure("noAlpha_noKeepOrig", false, false, true)
  }

  test("invokeTransformedAdapterMethodCall") {
    val ctx = new Ctx {
      useAlphaEquality = true
      keepOriginalFunc = false
    }
    import ctx._
    import Col._
    import WArray._
    import ColOverArray._
    val f = fun { col: Rep[Col[Int]] =>  col.length }
    val g = fun { arr: Rep[WArray[Int]] => f(RColOverArray(arr)) }
    val exp = fun { arr: Rep[WArray[Int]] => arr.length }
    emit("graphs", f, g, exp)
    g shouldBe exp
  }

}
