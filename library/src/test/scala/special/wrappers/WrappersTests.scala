package special.wrappers

import scala.collection.mutable
import scalan.BaseCtxTests

/** Base class inhereted by test suite for each wrapper like WArray, WOption etc. */
abstract class WrappersTests extends BaseCtxTests {
  class WrappersCtx extends TestContext with WrappersModule {
    import WArray._
    import Liftables._
    
    lazy val t1 = fun { (xs: Rep[WArray[Int]]) => xs.length }
    lazy val t2 = fun { (xs: Rep[WArray[Int]]) => xs(10) }
    lazy val t3 = fun { (xs: Rep[WArray[Int]]) => xs.zip(RWArray.fill(xs.length, Thunk(10))) }
    lazy val t4 = fun { (xs: Rep[WArray[Int]]) => xs.map(fun {x => x + 1}) }

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

  }
}
