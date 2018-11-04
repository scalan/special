package special.wrappers

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.Library

class WArrayTests extends WrappersTests {

  test("WArray methods") {
    val ctx = new WrappersCtx {
      import WArray._
      lazy val t1 = fun { (xs: Rep[WArray[Int]]) => xs.length }
      lazy val t2 = fun { (xs: Rep[WArray[Int]]) => xs(10) }
      lazy val t3 = fun { (xs: Rep[WArray[Int]]) => xs.zip(RWArray.fill(xs.length, Thunk(10))) }
      lazy val t4 = fun { (xs: Rep[WArray[Int]]) => xs.map(fun {x => x + 1}) }

      val M = WArrayMethods; val C = WArrayCompanionMethods
      def test() = {
        { val Def(Lambda(_, _, x, M.length(obj))) = t1; assert(x == obj) }
        { val Def(Lambda(_, _, x, M.apply(obj, Def(Const(10))))) = t2; assert(x == obj) }
        { val Def(Lambda(_, _, x, M.zip(xs, C.fill(M.length(xs1), th)))) = t3;
          assert(x == xs)
          assert(xs == xs1) }
      }
    }
    ctx.test()
    ctx.emit("t1", ctx.t1)
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
    ctx.emit("t4", ctx.t4)
  }

  test("invokeUnlifted") {
    val ctx = new WrappersCtx
    import ctx._
    import Liftables._
    import WArray._
    import EnvRep._

    val arr = Array(1, 2, 3)
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arg <- lifted(2) } yield xs.apply(arg) }, arr.apply(2))

    var sum1 = 0
    val f1 = (x: Int) => sum1 += x
    var sum2 = 0
    val f2 = (x: Int) => sum2 += x
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arg <- lifted(f1) } yield xs.foreach(arg) }, arr.foreach(f2))
    sum2 shouldBe sum2

    val p = (x: Int) => x == 2
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arg <- lifted(p) } yield xs.exists(arg) }, arr.exists(p))
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arg <- lifted(p) } yield xs.forall(arg) }, arr.forall(p))
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arg <- lifted(p) } yield xs.filter(arg) }, arr.filter(p))

    val add = (p: (Int,Int)) => p._1 + p._2
    check(arr, { env: EnvRep[WArray[Int]] =>
      for { xs <- env; z <- lifted(0); addL <- lifted(add) } yield xs.foldLeft(z, addL) },
      arr.foldLeft(0)((x,y) => add((x,y))))

    check(arr, { env: EnvRep[WArray[Int]] =>
      for { xs <- env; f <- lifted(1); u <- lifted(2) } yield xs.slice(f, u) }, arr.slice(1, 2))
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env } yield xs.length }, arr.length)

    val inc = (x: Int) => x + 1
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, arr.map(inc))
    val arr2 = Array("a", "b", "c")
    check(arr, { env: EnvRep[WArray[Int]] => for {xs <- env; arr2L <- lifted(arr2) } yield xs.zip(arr2L) }, arr.zip(arr2))
  }

  test("invokeUnlifted for Col") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import WArray._
    import Col._
    import ColBuilder._
    import EnvRep._

    val Cols: SColBuilder = new special.collection.ColOverArrayBuilder
    val arr = Array(1, 2, 3)
    val col = Cols.fromArray(arr)

    check(col, { env: EnvRep[Col[Int]] => for {xs <- env; arg <- lifted(2) } yield xs.apply(arg) }, col.apply(2))

    val inc = (x: Int) => x + 1
    check(col, { env: EnvRep[Col[Int]] => for { xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, col.map(inc))

    check(Cols, { env: EnvRep[ColBuilder] => for { b <- env; arrL <- lifted(arr) } yield b.fromArray(arrL) }, Cols.fromArray(arr))

    check(Cols,
      {env: EnvRep[ColBuilder] => for {
          b <- env; x1 <- lifted(1); x2 <- lifted(2); x3 <- lifted(3)
        } yield b.fromItems(x1, x2, x3) },
      Cols.fromItems(1, 2, 3))
  }
}
