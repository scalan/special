package scalan.dynamic

import java.lang.reflect.Method
import scalan.{Lazy, _}
import scalan.universe.api.TypesApi
import scalan.universe.api.UniverseUtils._
import scalan.util.CollectionUtil._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait SpecializationsDefs extends Specializations {
  self: ScalanEx =>
import IsoUR._
import Converter._
import IsoFunc._
import IsoFuncBase._

object IsoFunc extends EntityObject("IsoFunc") {
  private val IsoFuncClass = classOf[IsoFunc[_, _, _]]

  // entityAdapter for IsoFunc trait
  case class IsoFuncAdapter[T, R, M](source: Ref[IsoFunc[T, R, M]])
      extends IsoFunc[T, R, M]
      with Def[IsoFunc[T, R, M]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R];
implicit lazy val eM = source.elem.typeArgs("M")._1.asElem[M]

    val resultType: Elem[IsoFunc[T, R, M]] = element[IsoFunc[T, R, M]]
    override def transform(t: Transformer) = IsoFuncAdapter[T, R, M](t(source))

    def func: Ref[T => R] = {
      asRep[T => R](mkMethodCall(source,
        IsoFuncClass.getMethod("func"),
        WrappedArray.empty,
        true, true, element[T => R]))
    }

    def metric: Ref[T => M] = {
      asRep[T => M](mkMethodCall(source,
        IsoFuncClass.getMethod("metric"),
        WrappedArray.empty,
        true, true, element[T => M]))
    }

    def apply(x: Ref[T]): Ref[R] = {
      asRep[R](mkMethodCall(source,
        IsoFuncClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](x),
        true, true, element[R]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefIsoFunc[T, R, M](p: Ref[IsoFunc[T, R, M]]): IsoFunc[T, R, M] = {
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

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
  }

  implicit def isoFuncElement[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M]): Elem[IsoFunc[T, R, M]] =
    cachedElemByClass(eT, eR, eM)(classOf[IsoFuncElem[T, R, M, IsoFunc[T, R, M]]])

  implicit case object IsoFuncCompanionElem extends CompanionElem[IsoFuncCompanionCtor]

  abstract class IsoFuncCompanionCtor extends CompanionDef[IsoFuncCompanionCtor] {
    def resultType = IsoFuncCompanionElem
    override def toString = "IsoFunc"
  }
  implicit def unrefIsoFuncCompanionCtor(p: Ref[IsoFuncCompanionCtor]): IsoFuncCompanionCtor =
    p.rhs.asInstanceOf[IsoFuncCompanionCtor]

  lazy val RIsoFunc: Ref[IsoFuncCompanionCtor] = new IsoFuncCompanionCtor {
  }
} // of object IsoFunc
  registerEntityObject("IsoFunc", IsoFunc)

object IsoFuncBase extends EntityObject("IsoFuncBase") {
  case class IsoFuncBaseCtor[T, R, M]
      (override val func: Ref[T => R], override val metric: Ref[T => M])
    extends IsoFuncBase[T, R, M](func, metric) with Def[IsoFuncBase[T, R, M]] {
    implicit lazy val eT = func.elem.eDom;
implicit lazy val eR = func.elem.eRange;
implicit lazy val eM = metric.elem.eRange

    lazy val resultType = element[IsoFuncBase[T, R, M]]
    override def transform(t: Transformer) = IsoFuncBaseCtor[T, R, M](t(func), t(metric))
  }
  // elem for concrete class
  class IsoFuncBaseElem[T, R, M](val iso: Iso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]])(implicit override val eT: Elem[T], override val eR: Elem[R], override val eM: Elem[M])
    extends IsoFuncElem[T, R, M, IsoFuncBase[T, R, M]]
    with ConcreteElem[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] {
    override lazy val parent: Option[Elem[_]] = Some(isoFuncElement(element[T], element[R], element[M]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
  }

  // state representation type
  type IsoFuncBaseData[T, R, M] = (T => R, T => M)

  // 3) Iso for concrete class
  class IsoFuncBaseIso[T, R, M](implicit eT: Elem[T], eR: Elem[R], eM: Elem[M])
    extends EntityIso[IsoFuncBaseData[T, R, M], IsoFuncBase[T, R, M]] with Def[IsoFuncBaseIso[T, R, M]] {
    override def transform(t: Transformer) = new IsoFuncBaseIso[T, R, M]()(eT, eR, eM)
    private lazy val _safeFrom = fun { p: Ref[IsoFuncBase[T, R, M]] => (p.func, p.metric) }
    override def from(p: Ref[IsoFuncBase[T, R, M]]) =
      tryConvert[IsoFuncBase[T, R, M], (T => R, T => M)](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(T => R, T => M)]) = {
      val Pair(func, metric) = p
      RIsoFuncBase(func, metric)
    }
    lazy val eFrom = pairElement(element[T => R], element[T => M])
    lazy val eTo = new IsoFuncBaseElem[T, R, M](self)
    lazy val resultType = new IsoFuncBaseIsoElem[T, R, M](eT, eR, eM)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eT
      case 1 => eR
      case 2 => eM
    }
  }
  case class IsoFuncBaseIsoElem[T, R, M](eT: Elem[T], eR: Elem[R], eM: Elem[M]) extends Elem[IsoFuncBaseIso[T, R, M]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant), "M" -> (eM -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IsoFuncBaseCompanionCtor extends CompanionDef[IsoFuncBaseCompanionCtor] {
    def resultType = IsoFuncBaseCompanionElem
    override def toString = "IsoFuncBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[T, R, M](p: Ref[IsoFuncBaseData[T, R, M]]): Ref[IsoFuncBase[T, R, M]] = {
      implicit val eT = p._1.elem.eDom;
implicit val eR = p._1.elem.eRange;
implicit val eM = p._2.elem.eRange
      isoIsoFuncBase[T, R, M].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[T, R, M](func: Ref[T => R], metric: Ref[T => M]): Ref[IsoFuncBase[T, R, M]] =
      mkIsoFuncBase(func, metric)

    def unapply[T, R, M](p: Ref[IsoFunc[T, R, M]]) = unmkIsoFuncBase(p)
  }
  lazy val IsoFuncBaseRef: Ref[IsoFuncBaseCompanionCtor] = new IsoFuncBaseCompanionCtor
  lazy val RIsoFuncBase: IsoFuncBaseCompanionCtor = unrefIsoFuncBaseCompanion(IsoFuncBaseRef)
  implicit def unrefIsoFuncBaseCompanion(p: Ref[IsoFuncBaseCompanionCtor]): IsoFuncBaseCompanionCtor = {
    if (p.rhs.isInstanceOf[IsoFuncBaseCompanionCtor])
      p.rhs.asInstanceOf[IsoFuncBaseCompanionCtor]
    else
      unrefDelegate[IsoFuncBaseCompanionCtor](p)
  }

  implicit case object IsoFuncBaseCompanionElem extends CompanionElem[IsoFuncBaseCompanionCtor]

  implicit def unrefIsoFuncBase[T, R, M](p: Ref[IsoFuncBase[T, R, M]]): IsoFuncBase[T, R, M] = {
    if (p.rhs.isInstanceOf[IsoFuncBase[T, R, M]@unchecked])
      p.rhs.asInstanceOf[IsoFuncBase[T, R, M]]
    else
      unrefDelegate[IsoFuncBase[T, R, M]](p)
  }

  implicit class ExtendedIsoFuncBase[T, R, M](p: Ref[IsoFuncBase[T, R, M]]) {
    def toData: Ref[IsoFuncBaseData[T, R, M]] = {
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
    (func: Ref[T => R], metric: Ref[T => M]): Ref[IsoFuncBase[T, R, M]] = {
    new IsoFuncBaseCtor[T, R, M](func, metric)
  }
  def unmkIsoFuncBase[T, R, M](p: Ref[IsoFunc[T, R, M]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IsoFuncBaseElem[T, R, M] @unchecked =>
      Some((asRep[IsoFuncBase[T, R, M]](p).func, asRep[IsoFuncBase[T, R, M]](p).metric))
    case _ =>
      None
  }
} // of object IsoFuncBase
  registerEntityObject("IsoFuncBase", IsoFuncBase)

  registerModule(SpecializationsModule)
}

object SpecializationsModule extends scalan.ModuleInfo("scalan.dynamic", "Specializations")
}

