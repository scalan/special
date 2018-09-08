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
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.slice(env.lifted(1), env.lifted(2)), arr.slice(1, 2))
    check(arr, (env: DataEnv, xs: Rep[WArray[Int]]) => xs.length, arr.length)

//    @External def foreach(f: Rep[scala.Function1[T, Unit]]): Rep[Unit];
//    @External def exists(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
//    @External def forall(p: Rep[scala.Function1[T, Boolean]]): Rep[Boolean];
//    @External def filter(p: Rep[scala.Function1[T, Boolean]]): Rep[WArray[T]];
//    @External def foldLeft[B](zero: Rep[B], op: Rep[scala.Function1[scala.Tuple2[B, T], B]]): Rep[B];
//    @External def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]];
//    @External def length: Rep[Int];
//    @External def map[B](f: Rep[scala.Function1[T, B]]): Rep[WArray[B]];
//    @External def zip[B](ys: Rep[WArray[B]]): Rep[WArray[scala.Tuple2[T, B]]]
  }
}
