package scalan.dynamic

import java.lang.reflect.Method
import scalan.{Lazy, TypeDesc, _}
import scalan.universe.api.TypesApi
import scalan.universe.api.UniverseUtils._
import scalan.util.CollectionUtil._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait SpecializationsDefs extends Specializations {
  self: Scalan =>
import IsoUR._
import Converter._
import IsoFunc._
import IsoFuncBase._

object IsoFunc extends EntityObject("IsoFunc") {
  // entityAdapter for IsoFunc trait
  case class IsoFuncAdapter[T, R, M](source: Rep[IsoFunc[T, R, M]])
      extends IsoFunc[T, R, M] with Def[IsoFunc[T, R, M]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R];
implicit lazy val eM = source.elem.typeArgs("M")._1.asElem[M]

    val selfType: Elem[IsoFunc[T, R, M]] = element[IsoFunc[T, R, M]]
    override def transform(t: Transformer) = IsoFuncAdapter[T, R, M](t(source))
    private val thisClass = classOf[IsoFunc[T, R, M]]

    def func: Rep[T => R] = {
      asRep[T => R](mkMethodCall(source,
        thisClass.getMethod("func"),
        List(),
        true, true, element[T => R]))
    }

    def metric: Rep[T => M] = {
      asRep[T => M](mkMethodCall(source,
        thisClass.getMethod("metric"),
        List(),
        true, true, element[T => M]))
    }

    def apply(x: Rep[T]): Rep[R] = {
      asRep[R](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(x),
        true, true, element[R]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyIsoFunc[T, R, M](p: Rep[IsoFunc[T, R, M]]): IsoFunc[T, R, M] = {
    if (p.rhs.isInstanceOf[IsoFunc[T, R, M]@unchecked]) p.rhs.asInstanceOf[IsoFunc[T, R, M]]
    else
      IsoFuncAdapter(p)
  }

  // familyElem
  class IsoFuncElem[T, R, M, To <: IsoFunc[T, R, M]](implicit _eT: Elem[T], _eR: Elem[R], _eM: Elem[M])
    extends EntityElem[To] {
    def eT = _eT
    def eR = _eR
    def eM = _eM

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFunc[T, R, M]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[IsoFunc[T, R, M]] => convertIsoFunc(x) }
      tryConvert(element[IsoFunc[T, R, M]], this, x, conv)
    }

    def convertIsoFunc(x: Rep[IsoFunc[T, R, M]]): Rep[To] = {
      x.elem match {
        case _: IsoFuncElem[_, _, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have IsoFuncElem[_, _, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def isoFuncElement[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]): Elem[IsoFunc[T, R, M]] =
    cachedElemByClass(eT, eR, eM)(classOf[IsoFuncElem[T, R, M, IsoFunc[T, R, M]]])

  implicit case object IsoFuncCompanionElem extends CompanionElem[IsoFuncCompanionCtor] {
    lazy val tag = weakTypeTag[IsoFuncCompanionCtor]
    protected def getDefaultRep = RIsoFunc
  }

  abstract class IsoFuncCompanionCtor extends CompanionDef[IsoFuncCompanionCtor] {
    def selfType = IsoFuncCompanionElem
    override def toString = "IsoFunc"
  }
  implicit def proxyIsoFuncCompanionCtor(p: Rep[IsoFuncCompanionCtor]): IsoFuncCompanionCtor =
    proxyOps[IsoFuncCompanionCtor](p)

  lazy val RIsoFunc: Rep[IsoFuncCompanionCtor] = new IsoFuncCompanionCtor {
  }

  object IsoFuncMethods {
    object func {
      def unapply(d: Def[_]): Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "func" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object metric {
      def unapply(d: Def[_]): Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "metric" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[IsoFunc[T, R, M]] forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IsoFuncElem[_, _, _, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IsoFunc[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object IsoFunc
  registerEntityObject("IsoFunc", IsoFunc)

object IsoFuncBase extends EntityObject("IsoFuncBase") {
  case class IsoFuncBaseCtor[T, R, M]
      (override val func: Rep[T => R], override val metric: Rep[T => M])
    extends IsoFuncBase[T, R, M](func, metric) with Def[IsoFuncBase[T, R, M]] {
    implicit lazy val eT = func.elem.eDom;
implicit lazy val eR = func.elem.eRange;
implicit lazy val eM = metric.elem.eRange

    lazy val selfType = element[IsoFuncBase[T, R, M]]
    override def transform(t: Transformer) = IsoFuncBaseCtor[T, R, M](t(func), t(metric))
  }
  // elem for concrete class
  class IsoFuncBaseElem[T, R, M](val iso: Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]])(implicit override val eT: Elem[T], override val eR: Elem[R], override val eM: Elem[M])
    extends IsoFuncElem[T, R, M, IsoFuncBase[T, R, M]]
    with ConcreteElem[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] {
    override lazy val parent: Option[Elem[_]] = Some(isoFuncElement(element[T], element[R], element[M]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
    override def convertIsoFunc(x: Rep[IsoFunc[T, R, M]]) = RIsoFuncBase(x.func, x.metric)
    override def getDefaultRep = RIsoFuncBase(constFun[T, R](element[R].defaultRepValue), constFun[T, M](element[M].defaultRepValue))
    override lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFuncBase[T, R, M]]
    }
  }

  // state representation type
  type IsoFuncBaseData[T, R, M] = (T => R, T => M)

  // 3) Iso for concrete class
  class IsoFuncBaseIso[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M])
    extends EntityIso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] with Def[IsoFuncBaseIso[T, R, M]] {
    override def transform(t: Transformer) = new IsoFuncBaseIso[T, R, M]()(eT, eR, eM)
    private lazy val _safeFrom = fun { p: Rep[IsoFuncBase[T, R, M]] => (p.func, p.metric) }
    override def from(p: Rep[IsoFuncBase[T, R, M]]) =
      tryConvert[IsoFuncBase[T, R, M], (T => R, T => M)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(T => R, T => M)]) = {
      val Pair(func, metric) = p
      RIsoFuncBase(func, metric)
    }
    lazy val eFrom = pairElement(element[T => R], element[T => M])
    lazy val eTo = new IsoFuncBaseElem[T, R, M](self)
    lazy val selfType = new IsoFuncBaseIsoElem[T, R, M](eT, eR, eM)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eT
      case 1 => eR
      case 2 => eM
    }
  }
  case class IsoFuncBaseIsoElem[T, R, M](eT: Elem[T], eR: Elem[R], eM: Elem[M]) extends Elem[IsoFuncBaseIso[T, R, M]] {
    def getDefaultRep = reifyObject(new IsoFuncBaseIso[T, R, M]()(eT, eR, eM))
    lazy val tag = {
      implicit val tagT = eT.tag
      implicit val tagR = eR.tag
      implicit val tagM = eM.tag
      weakTypeTag[IsoFuncBaseIso[T, R, M]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IsoFuncBaseCompanionCtor extends CompanionDef[IsoFuncBaseCompanionCtor] {
    def selfType = IsoFuncBaseCompanionElem
    override def toString = "IsoFuncBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[T, R, M](p: Rep[IsoFuncBaseData[T, R, M]]): Rep[IsoFuncBase[T, R, M]] = {
      implicit val eT = p._1.elem.eDom;
implicit val eR = p._1.elem.eRange;
implicit val eM = p._2.elem.eRange
      isoIsoFuncBase[T, R, M].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[T, R, M](func: Rep[T => R], metric: Rep[T => M]): Rep[IsoFuncBase[T, R, M]] =
      mkIsoFuncBase(func, metric)

    def unapply[T, R, M](p: Rep[IsoFunc[T, R, M]]) = unmkIsoFuncBase(p)
  }
  lazy val IsoFuncBaseRep: Rep[IsoFuncBaseCompanionCtor] = new IsoFuncBaseCompanionCtor
  lazy val RIsoFuncBase: IsoFuncBaseCompanionCtor = proxyIsoFuncBaseCompanion(IsoFuncBaseRep)
  implicit def proxyIsoFuncBaseCompanion(p: Rep[IsoFuncBaseCompanionCtor]): IsoFuncBaseCompanionCtor = {
    if (p.rhs.isInstanceOf[IsoFuncBaseCompanionCtor])
      p.rhs.asInstanceOf[IsoFuncBaseCompanionCtor]
    else
      proxyOps[IsoFuncBaseCompanionCtor](p)
  }

  implicit case object IsoFuncBaseCompanionElem extends CompanionElem[IsoFuncBaseCompanionCtor] {
    lazy val tag = weakTypeTag[IsoFuncBaseCompanionCtor]
    protected def getDefaultRep = IsoFuncBaseRep
  }

  implicit def proxyIsoFuncBase[T, R, M](p: Rep[IsoFuncBase[T, R, M]]): IsoFuncBase[T, R, M] =
    proxyOps[IsoFuncBase[T, R, M]](p)

  implicit class ExtendedIsoFuncBase[T, R, M](p: Rep[IsoFuncBase[T, R, M]]) {
    def toData: Rep[IsoFuncBaseData[T, R, M]] = {
      implicit val eT = p.func.elem.eDom;
implicit val eR = p.func.elem.eRange;
implicit val eM = p.metric.elem.eRange
      isoIsoFuncBase(eT, eR, eM).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIsoFuncBase[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]): Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] =
    reifyObject(new IsoFuncBaseIso[T, R, M]()(eT, eR, eM))

  def mkIsoFuncBase[T, R, M]
    (func: Rep[T => R], metric: Rep[T => M]): Rep[IsoFuncBase[T, R, M]] = {
    new IsoFuncBaseCtor[T, R, M](func, metric)
  }
  def unmkIsoFuncBase[T, R, M](p: Rep[IsoFunc[T, R, M]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IsoFuncBaseElem[T, R, M] @unchecked =>
      Some((asRep[IsoFuncBase[T, R, M]](p).func, asRep[IsoFuncBase[T, R, M]](p).metric))
    case _ =>
      None
  }

    object IsoFuncBaseMethods {
    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IsoFuncBaseElem[_, _, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IsoFuncBase[T, R, M]], Rep[T]) forSome {type T; type R; type M}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method toString

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
  }
} // of object IsoFuncBase
  registerEntityObject("IsoFuncBase", IsoFuncBase)

  registerModule(SpecializationsModule)
}

object SpecializationsModule extends scalan.ModuleInfo("scalan.dynamic", "Specializations")
}

