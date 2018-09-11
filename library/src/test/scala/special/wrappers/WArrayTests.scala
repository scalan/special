package special.wrappers

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.Library

class WArrayTests extends WrappersTests {

  test("WArray methods") {
    val ctx = new WrappersCtx {
      import WArray._
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

    val arr = Array(1, 2, 3)
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.apply(env.lifted(2)), arr.apply(2))

    var sum1 = 0
    val f1 = (x: Int) => sum1 += x
    var sum2 = 0
    val f2 = (x: Int) => sum2 += x
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.foreach(env.lifted(f1)), arr.foreach(f2))
    sum2 shouldBe sum2

    val p = (x: Int) => x == 2
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.exists(env.lifted(p)), arr.exists(p))
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.forall(env.lifted(p)), arr.forall(p))
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.filter(env.lifted(p)), arr.filter(p))

    val add = (p: (Int,Int)) => p._1 + p._2
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.foldLeft(env.lifted(0), env.lifted(add)), arr.foldLeft(0)((x,y) => add((x,y))))

    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.slice(env.lifted(1), env.lifted(2)), arr.slice(1, 2))
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.length, arr.length)

    val inc = (x: Int) => x + 1
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.map(env.lifted(inc)), arr.map(inc))
    val arr2 = Array("a", "b", "c")
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.zip(env.lifted(arr2)), arr.zip(arr2))
  }

  test("invokeUnlifted for Col") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import WArray._
    import Col._
    import ColBuilder._

    val Cols: SColBuilder = new special.collection.ColOverArrayBuilder
    val arr = Array(1, 2, 3)
    val col = Cols.fromArray(arr)

    check(col, (env: DataEnv, xs: Rep[Col[Int]]) => xs.apply(env.lifted(2)), col.apply(2))

    val inc = (x: Int) => x + 1
    check(col, (env: DataEnv, xs: Rep[Col[Int]]) => xs.map(env.lifted(inc)), col.map(inc))

    check(Cols, (env: DataEnv, Cols: Rep[ColBuilder]) => Cols.fromArray(env.lifted(arr)), Cols.fromArray(arr))

    check(Cols,
      (env: DataEnv, Cols: Rep[ColBuilder]) => Cols.apply(env.lifted(1), env.lifted(2), env.lifted(3)),
      Cols.apply(1, 2, 3))
  }
}
