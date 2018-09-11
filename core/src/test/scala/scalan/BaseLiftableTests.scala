package scalan

import scala.collection.mutable

trait BaseLiftableTests { self: BaseCtxTests =>

  trait LiftableTestKit { scalan: Scalan =>
    import Liftables._

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
