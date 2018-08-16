package scalan

import java.lang.reflect.Method
import java.util.Objects
import special.collection.{CostsModule, ConcreteCostsModule, ColsModule, MonoidsModule, ColsOverArraysModule, MonoidInstancesModule}
import special.wrappers.{WrappersModule, WrappersSpecModule}
import special.wrappers.impl.WrappersSpecModule

import scalan.util.ReflectionUtil

trait Library extends Scalan
  with WrappersModule
  with WrappersSpecModule
  with ColsModule
  with ColsOverArraysModule
  with CostsModule
  with ConcreteCostsModule
  with MonoidsModule
  with MonoidInstancesModule {
  import WArray._; import Col._; import ColBuilder._; import ReplCol._
  import Costed._;
  import CostedFunc._; import Closure._

  override def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = eA match {
    case ea: WArrayElem[_,_] => Objects.deepEquals(x, y)
    case _ => super.equalValues[A](x, y)
  }

  override protected def getResultElem(receiver: Sym, m: Method, args: List[AnyRef]): Elem[_] = receiver.elem match {
    case ae: WArrayElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        wArrayElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case ce: ColElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        colElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case b: ColBuilderElem[_] => m.getName match {
      case "apply" =>
        ReflectionUtil.overloadId(m) match {
          case Some("apply_items") =>
            val eItem = args(0).asInstanceOf[Seq[Sym]](0).elem
            colElement(eItem)
          case _ => super.getResultElem(receiver, m, args)
        }
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }

  private val WA = WArrayMethods
  private val CM = ColMethods
  private val CBM = ColBuilderMethods

  object IsProjectFirst {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(First(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  object IsProjectSecond {
    def unapply[A,B](f: Rep[_]): Option[Rep[A=>B]] = f match {
      case Def(Lambda(_,_,x, Def(Second(p)))) if p == x => Some(f.asRep[A=>B])
      case _ => None
    }
  }
  override def rewriteDef[T](d: Def[T]) = d match {
    case WA.length(WA.map(xs, _)) => xs.length
    case CM.length(CM.map(xs, _)) => xs.length

    case WA.zip(WA.map(xs, IsProjectFirst(_)), WA.map(ys, IsProjectSecond(_))) if xs == ys => xs
    case CM.zip(CM.map(xs, IsProjectFirst(_)), CM.map(ys, IsProjectSecond(_))) if xs == ys => xs

    case WA.map(WA.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>
      implicit val ea = f.elem.eDom
      val xs = _xs.asRep[WArray[a]]
      val g  = _g.asRep[b => c]
      xs.map(fun { x: Rep[a] => g(f(x)) })
    case CM.map(CM.map(_xs, f: RFunc[a, b]), _g: RFunc[_,c]) =>
      implicit val ea = f.elem.eDom
      val xs = _xs.asRep[Col[a]]
      val g  = _g.asRep[b => c]
      xs.map(fun { x: Rep[a] => g(f(x)) })

    case CM.map(xs, Def(IdentityLambda())) => xs
    case CM.map(xs, Def(ConstantLambda(res))) => RReplCol(res, xs.length)

    case _ => super.rewriteDef(d)
  }

  implicit class CostedFuncOps[A,B](fC: Rep[Costed[A => B]]) {
    def applyCost(x: Rep[A]): Rep[Int] = {
      val cf = fC.asRep[CostedFunc[Any, A, B]]
      val y = cf.costFunc.apply(x).asRep[Int]
      y
    }
  }
}
