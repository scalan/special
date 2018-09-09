package wrappers.scala

import scalan._
import impl._
import special.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import java.lang.reflect.Method

  import special.wrappers.ArrayWrapSpec

  import scala.collection.mutable

  // Abs -----------------------------------
trait WArraysDefs extends scalan.Scalan with WArrays {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WArray._

object WArray extends EntityObject("WArray") {
  import Liftables._
  case class WArrayConst[ST, T](constValue: Array[ST], lT: Liftable[ST, T])
    extends WArray[T] with LiftedConst[Array[ST], WArray[T]] {
    implicit def eT: Elem[T] = lT.eW
    val selfType: Elem[WArray[T]] = wArrayElement(lT.eW)
    val liftable: Liftables.Liftable[Array[ST], WArray[T]] = liftableArray(lT)

    def apply(i: Rep[Int]): Rep[T] = delayInvoke
    def foreach(f: Rep[T => Unit]): Rep[Unit] = delayInvoke
    def exists(p: Rep[T => Boolean]): Rep[Boolean] = delayInvoke
    def forall(p: Rep[T => Boolean]): Rep[Boolean] = delayInvoke
    def filter(p: Rep[T => Boolean]): Rep[WArray[T]] = delayInvoke
    def foldLeft[B](zero: Rep[B], op: Rep[((B, T)) => B]): Rep[B] = delayInvoke
    def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]] = delayInvoke
    def length: Rep[Int] = delayInvoke
    def map[B](f: Rep[T => B]): Rep[WArray[B]] = delayInvoke
    def zip[B](ys: Rep[WArray[B]]): Rep[WArray[(T, B)]] = delayInvoke
  }

  case class LiftableArray[ST, T](lT: Liftable[ST, T]) extends Liftable[Array[ST], WArray[T]] {
    def eW: Elem[WArray[T]] = wArrayElement(lT.eW)
    def sourceClassTag: ClassTag[Array[ST]] = {
      implicit val tagST = lT.eW.sourceClassTag.asInstanceOf[ClassTag[ST]]
      classTag[Array[ST]]
    }
    def lift(x: Array[ST]): Rep[WArray[T]] = WArrayConst(x, lT)
    def unlift(w: Rep[WArray[T]]): Array[ST] = w match {
      case Def(WArrayConst(x: Array[_], l)) if l == lT => x.asInstanceOf[Array[ST]]
      case _ => unliftError(w)
    }
  }

  implicit def liftableArray[ST,T](implicit lT: Liftable[ST,T]): Liftable[Array[ST], WArray[T]] =
    LiftableArray(lT)

  // entityProxy: single proxy for each type family
  implicit def proxyWArray[T](p: Rep[WArray[T]]): WArray[T] = {
    proxyOps[WArray[T]](p)(scala.reflect.classTag[WArray[T]])
  }

  implicit def castWArrayElement[T](elem: Elem[WArray[T]]): WArrayElem[T, WArray[T]] =
    elem.asInstanceOf[WArrayElem[T, WArray[T]]]

  implicit lazy val containerWArray: Functor[WArray] = new Functor[WArray] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[WArray[A]]
    def lift[A](implicit evA: Elem[A]) = element[WArray[A]]
    def unlift[A](implicit eFT: Elem[WArray[A]]) =
      castWArrayElement(eFT).eT
    def getElem[A](fa: Rep[WArray[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: WArrayElem[_,_] => Some(e.asElem[WArray[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[WArray[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class WArrayIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, WArray] {
    lazy val selfType = new ConcreteIsoElem[WArray[A], WArray[B], WArrayIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[WArray[A], WArray[B]]]]
    def cC = container[WArray]
    def from(x: Rep[WArray[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[WArray[A]]) = x.map(innerIso.toFun)
  }

  def wArrayIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(WArrayIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, WArray]]

  // familyElem
  class WArrayElem[T, To <: WArray[T]](implicit _eT: Elem[T])
    extends EntityElem1[T, To, WArray](_eT, container[WArray]) {
    def eT = _eT
    override val liftable: Liftables.Liftable[Array[_], To] =
      liftableArray(_eT.liftable).asLiftable[Array[_], To]

    override protected def collectMethods: Map[Method, MethodDesc] = {
      val wrapCls = classOf[ArrayWrapSpec]
      implicit val tagT = eT.classTag
      val xsCls = classOf[Array[T]]
      val cls = classOf[WArray[T]]
      val spec = new ArrayWrapSpec
      super.collectMethods ++ Seq(
          cls.getMethod("apply", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("apply", xsCls, classOf[Int])),
          cls.getMethod("map", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("map", xsCls, classOf[Function1[_,_]], CtClass)),
          cls.getMethod("foreach", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("foreach", xsCls, classOf[Function1[_,_]])),
          cls.getMethod("exists", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("exists", xsCls, classOf[Function1[_,_]])),
          cls.getMethod("forall", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("forall", xsCls, classOf[Function1[_,_]])),
          cls.getMethod("filter", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("filter", xsCls, classOf[Function1[_,_]])),
          cls.getMethod("foldLeft", SymClass, SymClass) -> WMethodDesc(spec, wrapCls.getMethod("foldLeft", xsCls, classOf[AnyRef], classOf[Function1[_,_]])),
          cls.getMethod("slice", SymClass, SymClass) -> WMethodDesc(spec, wrapCls.getMethod("slice", xsCls, classOf[Int], classOf[Int])),
          cls.getMethod("length") -> WMethodDesc(spec, wrapCls.getMethod("length", xsCls)),
          cls.getMethod("zip", SymClass) -> WMethodDesc(spec, wrapCls.getMethod("zip", xsCls, classOf[Array[_]])),
      )
    }

    override def invokeUnlifted(mc: MethodCall, dataEnv: DataEnv): AnyRef = mc match {
      case WArrayMethods.map(xs, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.selfType)
        super.invokeUnlifted(newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(mc, dataEnv)
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WArray[T]] => convertWArray(x) }
      tryConvert(element[WArray[T]], this, x, conv)
    }

    def convertWArray(x: Rep[WArray[T]]): Rep[To] = {
      x.elem match {
        case _: WArrayElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have WArrayElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wArrayElement[T](implicit eT: Elem[T]): Elem[WArray[T]] =
    cachedElem[WArrayElem[T, WArray[T]]](eT)

  implicit def extendWArrayElement[T](elem: Elem[WArray[T]]): WArrayElem[T,_] = elem.asInstanceOf[WArrayElem[T,_]]

  implicit case object WArrayCompanionElem extends CompanionElem[WArrayCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayCompanionCtor]
    protected def getDefaultRep = RWArray
  }

  abstract class WArrayCompanionCtor extends CompanionDef[WArrayCompanionCtor] with WArrayCompanion {
    def selfType = WArrayCompanionElem
    override def toString = "WArray"
  }
  implicit def proxyWArrayCompanionCtor(p: Rep[WArrayCompanionCtor]): WArrayCompanionCtor =
    proxyOps[WArrayCompanionCtor](p)

  lazy val RWArray: Rep[WArrayCompanionCtor] = new WArrayCompanionCtor {
    def fill[T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]] = {
      implicit val eT = elem.elem.eItem
      mkMethodCall(self,
        this.getClass.getMethod("fill", classOf[Sym], classOf[Sym]),
        List(n, elem),
        true, element[WArray[T]]).asRep[WArray[T]]
    }
  }

  case class ViewWArray[A, B](source: Rep[WArray[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, WArray](wArrayIso(innerIso)) {
    override def toString = s"ViewWArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewWArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object WArrayMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "foreach" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object exists {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "exists" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object forall {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "forall" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "filter" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foldLeft {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(zero, op, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "foldLeft" =>
          Some((receiver, zero, op)).asInstanceOf[Option[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(from, until, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "slice" =>
          Some((receiver, from, until)).asInstanceOf[Option[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[WArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WArrayCompanionMethods {
    object fill {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, elem, _*), _) if receiver.elem == WArrayCompanionElem && method.getName == "fill" =>
          Some((n, elem)).asInstanceOf[Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
} // of object WArray
  registerEntityObject("WArray", WArray)

  object UserTypeWArray {
    def unapply(s: Sym): Option[Iso[_, _]] = {
      s.elem match {
        case e: WArrayElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewWArray[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeWArray(iso: Iso[a, b]) =>
      val newIso = wArrayIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[WArray[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type RepWArray[T] = Rep[WArray[T]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewWArray(Def(view2@ViewWArray(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewWArray(arr, compIso)

    case WArrayMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepWArray[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewWArray(s, iso)
        res
      case (HasViews(source, Def(contIso: WArrayIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        source.asRep[WArray[a]].map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }

  registerModule(WArraysModule)
}

object WArraysModule extends scalan.ModuleInfo("wrappers.scala", "WArrays")
}

trait WArraysModule extends wrappers.scala.impl.WArraysDefs {self: WrappersModule =>}
