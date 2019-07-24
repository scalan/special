package wrappers.scala

import scalan._
import impl._
import special.wrappers.WrappersModule
import special.wrappers.OptionWrapSpec
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WOptionsDefs extends scalan.Scalan with WOptions {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WOption._

object WOption extends EntityObject("WOption") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WOptionConst[SA, A](
        constValue: Option[SA],
        lA: Liftable[SA, A]
      ) extends WOption[A] with LiftedConst[Option[SA], WOption[A]]
        with Def[WOption[A]] with WOptionConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[Option[SA], WOption[A]] = liftableOption(lA)
    val selfType: Elem[WOption[A]] = liftable.eW
  }

  trait WOptionConstMethods[A] extends WOption[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WOptionClass = classOf[WOption[A]]

    override def fold[B](ifEmpty: Rep[Thunk[B]], f: Rep[A => B]): Rep[B] = {
      implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(self,
        WOptionClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(ifEmpty, f),
        true, false, element[B]))
    }

    override def isEmpty: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WOptionClass.getMethod("isEmpty"),
        List(),
        true, false, element[Boolean]))
    }

    override def isDefined: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        WOptionClass.getMethod("isDefined"),
        List(),
        true, false, element[Boolean]))
    }

    override def filter(p: Rep[A => Boolean]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        WOptionClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, false, element[WOption[A]]))
    }

    override def flatMap[B](f: Rep[A => WOption[B]]): Rep[WOption[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[WOption[B]](mkMethodCall(self,
        WOptionClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, false, element[WOption[B]]))
    }

    override def map[B](f: Rep[A => B]): Rep[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(self,
        WOptionClass.getMethod("map", classOf[Sym]),
        List(f),
        true, false, element[WOption[B]]))
    }

    override def getOrElse[B](default: Rep[Thunk[B]]): Rep[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(self,
        WOptionClass.getMethod("getOrElse", classOf[Sym]),
        List(default),
        true, false, element[B]))
    }

    override def get: Rep[A] = {
      asRep[A](mkMethodCall(self,
        WOptionClass.getMethod("get"),
        List(),
        true, false, element[A]))
    }
  }

  case class LiftableOption[SA, A](lA: Liftable[SA, A])
    extends Liftable[Option[SA], WOption[A]] {
    lazy val eW: Elem[WOption[A]] = wOptionElement(lA.eW)
    lazy val sourceType: RType[Option[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[Option[SA]]
    }
    def lift(x: Option[SA]): Rep[WOption[A]] = WOptionConst(x, lA)
    def unlift(w: Rep[WOption[A]]): Option[SA] = w match {
      case Def(WOptionConst(x: Option[_], _lA))
            if _lA == lA => x.asInstanceOf[Option[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableOption[SA, A](implicit lA: Liftable[SA,A]): Liftable[Option[SA], WOption[A]] =
    LiftableOption(lA)

  private val _OptionWrapSpec = new OptionWrapSpec {}
  // entityAdapter for WOption trait
  case class WOptionAdapter[A](source: Rep[WOption[A]])
      extends WOption[A] with Def[WOption[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val selfType: Elem[WOption[A]] = element[WOption[A]]
    override def transform(t: Transformer) = WOptionAdapter[A](t(source))
    private val thisClass = classOf[WOption[A]]

    def fold[B](ifEmpty: Rep[Thunk[B]], f: Rep[A => B]): Rep[B] = {
      implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(ifEmpty, f),
        true, true, element[B]))
    }

    def isEmpty: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isEmpty"),
        List(),
        true, true, element[Boolean]))
    }

    def isDefined: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isDefined"),
        List(),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[WOption[A]]))
    }

    def flatMap[B](f: Rep[A => WOption[B]]): Rep[WOption[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[WOption[B]](mkMethodCall(source,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, true, element[WOption[B]]))
    }

    def map[B](f: Rep[A => B]): Rep[WOption[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WOption[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[WOption[B]]))
    }

    def getOrElse[B](default: Rep[Thunk[B]]): Rep[B] = {
      implicit val eB = default.elem.eItem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym]),
        List(default),
        true, true, element[B]))
    }

    def get: Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("get"),
        List(),
        true, true, element[A]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWOption[A](p: Rep[WOption[A]]): WOption[A] = {
    if (p.rhs.isInstanceOf[WOption[A]@unchecked]) p.rhs.asInstanceOf[WOption[A]]
    else
      WOptionAdapter(p)
  }

  implicit def castWOptionElement[A](elem: Elem[WOption[A]]): WOptionElem[A, WOption[A]] =
    elem.asInstanceOf[WOptionElem[A, WOption[A]]]

  implicit lazy val containerWOption: Functor[WOption] = new Functor[WOption] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[WOption[A]]
    def lift[A](implicit evA: Elem[A]) = element[WOption[A]]
    def unlift[A](implicit eFT: Elem[WOption[A]]) =
      castWOptionElement(eFT).eA
    def getElem[A](fa: Rep[WOption[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: WOptionElem[_,_] => Some(e.asElem[WOption[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[WOption[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class WOptionIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, WOption] {
    lazy val selfType = new ConcreteIsoElem[WOption[A], WOption[B], WOptionIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[WOption[A], WOption[B]]]]
    def cC = container[WOption]
    def from(x: Rep[WOption[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[WOption[A]]) = x.map(innerIso.toFun)
    override def transform(t: Transformer) = WOptionIso(t(innerIso))
  }

  def wOptionIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(WOptionIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, WOption]]

  // familyElem
  class WOptionElem[A, To <: WOption[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, WOption](_eA, container[WOption]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[Option[_], To](liftableOption(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_OptionWrapSpec, classOf[WOption[A]], Set(
        "fold", "isEmpty", "isDefined", "filter", "flatMap", "map", "getOrElse", "get"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[WOption[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WOption[A]] => convertWOption(x) }
      tryConvert(element[WOption[A]], this, x, conv)
    }

    def convertWOption(x: Rep[WOption[A]]): Rep[To] = {
      x.elem match {
        case _: WOptionElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have WOptionElem[_, _], but got $e", x)
      }
    }
  }

  implicit def wOptionElement[A](implicit eA: Elem[A]): Elem[WOption[A]] =
    cachedElemByClass(eA)(classOf[WOptionElem[A, WOption[A]]])

  implicit case object WOptionCompanionElem extends CompanionElem[WOptionCompanionCtor] {
    lazy val tag = weakTypeTag[WOptionCompanionCtor]
  }

  abstract class WOptionCompanionCtor extends CompanionDef[WOptionCompanionCtor] with WOptionCompanion {
    def selfType = WOptionCompanionElem
    override def toString = "WOption"
  }
  implicit def proxyWOptionCompanionCtor(p: Rep[WOptionCompanionCtor]): WOptionCompanionCtor =
    proxyOps[WOptionCompanionCtor](p)

  lazy val RWOption: Rep[WOptionCompanionCtor] = new WOptionCompanionCtor {
    private val thisClass = classOf[WOptionCompanion]
  }

//  case class ViewWOption[A, B](source: Rep[WOption[A]], override val innerIso: Iso[A, B])
//    extends View1[A, B, WOption](wOptionIso(innerIso)) {
//    override def transform(t: Transformer) = ViewWOption(t(source), t(innerIso))
//    override def toString = s"ViewWOption[${innerIso.eTo.name}]($source)"
//    override def equals(other: Any) = other match {
//      case v: ViewWOption[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
//      case _ => false
//    }
//  }

  object WOptionMethods {
    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "fold" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Nullable[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "isEmpty" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isDefined {
      def unapply(d: Def[_]): Nullable[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "isDefined" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "map" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getOrElse" && receiver.elem.isInstanceOf[WOptionElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object get {
      def unapply(d: Def[_]): Nullable[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "get" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WOption[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WOptionCompanionMethods {
  }
} // of object WOption
  registerEntityObject("WOption", WOption)

  // manual fix: UserTypeWOption removed
  // manual fix: unapplyViews removed
  // manual fix: RepWOption removed
  // manual fix: rewriteDef removed

  registerModule(WOptionsModule)
}

object WOptionsModule extends scalan.ModuleInfo("wrappers.scala", "WOptions")
}

trait WOptionsModule extends wrappers.scala.impl.WOptionsDefs {self: WrappersModule =>}
