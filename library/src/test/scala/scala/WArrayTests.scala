package scala

import special.wrappers.WrappersModule

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan._

class WArrayTests extends BaseCtxTests {
  class Ctx extends TestContext with WrappersModule {
    import WArray._
    lazy val t1 = fun { (xs: Rep[WArray[Int]]) => xs.length }
    lazy val t2 = fun { (xs: Rep[WArray[Int]]) => xs(10) }
    lazy val t3 = fun { (xs: Rep[WArray[Int]]) => xs.zip(RWArray.fill(xs.length, Thunk(10))) }
    lazy val t4 = fun { (xs: Rep[WArray[Int]]) => xs.map(fun {x => x + 1}) }
  }

  test("WArray methods") {
    val ctx = new Ctx {
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
    val ctx = new Ctx
    import ctx._
    import Liftables._
    import WArray._
    /** Check the MethodCall reified in f can be mapped to unlifted method which can be invoked.*/
    def check[ST, T](obj: ST, f: (DataEnv, Rep[T]) => Sym, expected: Any)(implicit lT: Liftable[ST,T]) = {
      val objSym: Rep[T] = liftConst(obj)
      val env = mutable.Map[Sym, AnyRef]()
      env += (objSym -> obj.asInstanceOf[AnyRef])
      val resSym = f(env, objSym)
      resSym match {
        case Def(mc: MethodCall) =>
          val res = objSym.elem.invokeUnlifted(mc, env)
          res shouldBe expected
      }
    }

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
}
