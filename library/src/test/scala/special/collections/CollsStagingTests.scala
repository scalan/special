package special.collections

import scala.language.reflectiveCalls
import special.wrappers.WrappersTests
import scalan._
import scalan.util.BenchmarkUtil._

class CollsStagingTests extends WrappersTests {
  class Ctx extends TestContext with TestLibrary {
    import WArray._
    import CollOverArray._
    import CollOverArrayBuilder._
    lazy val t2 = fun { (xs: Rep[WArray[Double]]) =>
      val c = RCollOverArray(xs)
      c.map(fun { x => x + 1.0 })
    }
    lazy val t3 = fun { (x: Rep[Int]) =>
      val b = RCollOverArrayBuilder()
      b.fromItems(x, x + 1, x + 2)
    }
  }

  test("Coll methods") {
    val ctx = new Ctx {
      import WArray._
      import CollOverArray._
      import CollOverArrayBuilder._
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
    import Coll._
    import CollBuilder._
    import CollOverArrayBuilder._

    var res: Sym = null
    measure(10) { i =>
      var sum: Int = 0
      for (j <- 0 until 3000) {
        val colBuilder: Rep[CollBuilder] = RCollOverArrayBuilder()
        val col = colBuilder.replicate(i*j, 0)
        res = col.map(fun {x => x + 1})
        sum += ctx.defCount
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
        import Coll._
        import CollBuilder._
        import CollOverArrayBuilder._
        val colBuilder: Rep[CollBuilder] = RCollOverArrayBuilder()
        val col = colBuilder.replicate(i*j, 0)
        val res = col.map(fun {x => x + 1})
        sum += ctx.defCount
      }
      println(s"Defs: ${sum}")
    }
  }

  def runMeasure(name: String, alphaEq: Boolean, keepOrig: Boolean, unfoldWithOrig: Boolean) = {
    println(s"runMeasure($name, alphaEq = $alphaEq, keepOrig = $keepOrig, unfoldWithOrig = $unfoldWithOrig)")
    val nIters = 5
    val nRepeats = 100
    def warmUp(i: Int) = {
      val ctx = new Ctx {
        override val performViewsLifting = false
        useAlphaEquality = alphaEq
        keepOriginalFunc = keepOrig
        unfoldWithOriginalFunc = unfoldWithOrig
      }
      import ctx._
      import Coll._
      import CollBuilder._
      import CollOverArrayBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Rep[(CollBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, 0)
          val res = col.map(fun {x => x + delta})
          res
        }
        outGraph = Pair(f, f(Pair(RCollOverArrayBuilder(), 1)))
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
      import Coll._
      import CollBuilder._
      import CollOverArrayBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Rep[(CollBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, 0)
          val col2 = colBuilder.replicate(j+i, 0)
          val res = col.map(fun {x => x + delta}).zip(col2)
          res
        }
        outGraph = Pair(f, f(Pair(RCollOverArrayBuilder(), 1)))
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
    import Coll._
    import WArray._
    import CollOverArray._
    val f = fun { col: Rep[Coll[Int]] =>  col.length }
    val g = fun { arr: Rep[WArray[Int]] => f(RCollOverArray(arr)) }
    val exp = fun { arr: Rep[WArray[Int]] => arr.length }
    emit("graphs", f, g, exp)
    g shouldBe exp
  }

  test("invokeUnlifted for Col") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import WArray._
    import Coll._
    import CollBuilder._
    import EnvRep._

    val Cols: SCollBuilder = new special.collection.CollOverArrayBuilder
    val arr = Array(1, 2, 3)
    val col = Cols.fromArray(arr)

    check(col, { env: EnvRep[Coll[Int]] => for {xs <- env; arg <- lifted(2) } yield xs.apply(arg) }, col.apply(2))

    val inc = (x: Int) => x + 1
    check(col, { env: EnvRep[Coll[Int]] => for { xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, col.map(inc))

//    check(Cols, { env: EnvRep[CollBuilder] => for { b <- env; arrL <- lifted(arr) } yield b.fromArray(arrL) }, Cols.fromArray(arr))

    check(Cols,
    {env: EnvRep[CollBuilder] => for {
      b <- env; x1 <- lifted(1); x2 <- lifted(2); x3 <- lifted(3)
    } yield b.fromItems(x1, x2, x3) },
    Cols.fromItems(1, 2, 3))
  }

  test("invokeUnlifted for method of Ctor") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import WArray._
    import Coll._
    import CollBuilder._
    import CReplColl._
    import EnvRep._

    val Cols: SCollBuilder = new special.collection.CollOverArrayBuilder
    val colData = Cols.replicate(10, 10)
    val colSym = RCReplColl(10, 10)
    val resSym = colSym.append(colSym)
    val resData = colData.append(colData)
    val env = Map[Sym, AnyRef]()
    val resEnvSym = EnvRep.add(colSym -> colData.asInstanceOf[AnyRef])
    val (resEnv, _) = resEnvSym.run(env)
    resSym match {
      case Def(mc: MethodCall) =>
        val res = invokeUnlifted(colSym.elem, mc, resEnv)
        res shouldBe resData
    }
  }
}
