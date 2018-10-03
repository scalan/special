package wrappers.scala

import scalan._
import impl._
import special.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  import special.wrappers.OptionWrapSpec

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
      ) extends WOption[A] with LiftedConst[Option[SA], WOption[A]] {
    implicit def eA: Elem[A] = lA.eW
    val liftable: Liftable[Option[SA], WOption[A]] = liftableOption(lA)
    val selfType: Elem[WOption[A]] = liftable.eW
    @External def fold[B](ifEmpty: Rep[Thunk[B]], f: Rep[scala.Function1[A, B]]): Rep[B] = delayInvoke
    @External def isEmpty: Rep[Boolean] = delayInvoke
    @External def isDefined: Rep[Boolean] = delayInvoke
    @External def filter(p: Rep[scala.Function1[A, Boolean]]): Rep[WOption[A]] = delayInvoke
    @External def flatMap[B](f: Rep[scala.Function1[A, WOption[B]]]): Rep[WOption[B]] = delayInvoke
    @External def map[B](f: Rep[scala.Function1[A, B]]): Rep[WOption[B]] = delayInvoke
    @External def getOrElse[B](default: Rep[Thunk[B]]): Rep[B] = delayInvoke
    @External def get: Rep[A] = delayInvoke
  }

  case class LiftableOption[SA, A](lA: Liftable[SA, A])
    extends Liftable[Option[SA], WOption[A]] {
    lazy val eW: Elem[WOption[A]] = wOptionElement(lA.eW)
    lazy val sourceClassTag: ClassTag[Option[SA]] = {
      implicit val tagSA = lA.eW.sourceClassTag.asInstanceOf[ClassTag[SA]]
      classTag[Option[SA]]
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

  private val _OptionWrapSpec = new OptionWrapSpec
  // entityProxy: single proxy for each type family
  implicit def proxyWOption[A](p: Rep[WOption[A]]): WOption[A] = {
    proxyOps[WOption[A]](p)(scala.reflect.classTag[WOption[A]])
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
  }

  def wOptionIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(WOptionIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, WOption]]

  // familyElem
  class WOptionElem[A, To <: WOption[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, WOption](_eA, container[WOption]) {
    def eA = _eA

    override val liftable = liftableOption(_eA.liftable).asLiftable[Option[_], To]

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
        case _: WOptionElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have WOptionElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wOptionElement[A](implicit eA: Elem[A]): Elem[WOption[A]] =
    cachedElem[WOptionElem[A, WOption[A]]](eA)

  implicit case object WOptionCompanionElem extends CompanionElem[WOptionCompanionCtor] {
    lazy val tag = weakTypeTag[WOptionCompanionCtor]
    protected def getDefaultRep = RWOption
  }

  abstract class WOptionCompanionCtor extends CompanionDef[WOptionCompanionCtor] with WOptionCompanion {
    def selfType = WOptionCompanionElem
    override def toString = "WOption"
  }
  implicit def proxyWOptionCompanionCtor(p: Rep[WOptionCompanionCtor]): WOptionCompanionCtor =
    proxyOps[WOptionCompanionCtor](p)

  lazy val RWOption: Rep[WOptionCompanionCtor] = new WOptionCompanionCtor {
  }

  case class ViewWOption[A, B](source: Rep[WOption[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, WOption](wOptionIso(innerIso)) {
    override def toString = s"ViewWOption[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewWOption[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object WOptionMethods {
    object fold {
      def unapply(d: Def[_]): Option[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(ifEmpty, f, _*), _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "fold" =>
          Some((receiver, ifEmpty, f)).asInstanceOf[Option[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[WOption[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isDefined {
      def unapply(d: Def[_]): Option[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "isDefined" =>
          Some(receiver).asInstanceOf[Option[Rep[WOption[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "filter" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): ValOpt[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "map" =>
          Some((receiver, args(0))).asInstanceOf[ValOpt[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => ValOpt.None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Option[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(default, _*), _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "getOrElse" =>
          Some((receiver, default)).asInstanceOf[Option[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[Thunk[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object get {
      def unapply(d: Def[_]): Option[Rep[WOption[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WOptionElem[_, _]] && method.getName == "get" =>
          Some(receiver).asInstanceOf[Option[Rep[WOption[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[WOption[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object WOptionCompanionMethods {
  }
} // of object WOption
  registerEntityObject("WOption", WOption)

  object UserTypeWOption {
    def unapply(s: Sym): Option[Iso[_, _]] = {
      s.elem match {
        case e: WOptionElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewWOption[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeWOption(iso: Iso[a, b]) =>
      val newIso = wOptionIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[WOption[b]], newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type RepWOption[A] = Rep[WOption[A]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewWOption(Def(view2@ViewWOption(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewWOption(arr, compIso)

    case WOptionMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepWOption[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewWOption(s, iso)
        res
      case (HasViews(source, Def(contIso: WOptionIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        source.asRep[WOption[a]].map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }

  registerModule(WOptionsModule)
}

object WOptionsModule extends scalan.ModuleInfo("wrappers.scala", "WOptions")
}

trait WOptionsModule extends wrappers.scala.impl.WOptionsDefs {self: WrappersModule =>}
