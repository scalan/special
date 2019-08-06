package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait CostedOptionsDefs extends scalan.Scalan with CostedOptions {
  self: Library =>
import IsoUR._
import Converter._
import CCostedBuilder._
import CostedBuilder._
import CostedOption._
import Size._
import SizeOption._
import WOption._
import CCostedOption._

object CCostedOption extends EntityObject("CCostedOption") {
  case class CCostedOptionCtor[T]
      (override val value: Ref[WOption[T]], override val costOpt: Ref[WOption[Int]], override val sizeOpt: Ref[WOption[Size[T]]], override val accumulatedCost: Ref[Int])
    extends CCostedOption[T](value, costOpt, sizeOpt, accumulatedCost) with Def[CCostedOption[T]] {
    implicit lazy val eT = value.eA
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    lazy val resultType = element[CCostedOption[T]]
    override def transform(t: Transformer) = CCostedOptionCtor[T](t(value), t(costOpt), t(sizeOpt), t(accumulatedCost))
    private val thisClass = classOf[CostedOption[_]]

    override def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        WrappedArray.empty,
        true, false, element[Int]))
    }
  }
  // elem for concrete class
  class CCostedOptionElem[T](val iso: Iso[CCostedOptionData[T], CCostedOption[T]])(implicit override val eT: Elem[T])
    extends CostedOptionElem[T, CCostedOption[T]]
    with ConcreteElem[CCostedOptionData[T], CCostedOption[T]] {
    override lazy val parent: Option[Elem[_]] = Some(costedOptionElement(element[T]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }

  // state representation type
  type CCostedOptionData[T] = (WOption[T], (WOption[Int], (WOption[Size[T]], Int)))

  // 3) Iso for concrete class
  class CCostedOptionIso[T](implicit eT: Elem[T])
    extends EntityIso[CCostedOptionData[T], CCostedOption[T]] with Def[CCostedOptionIso[T]] {
    override def transform(t: Transformer) = new CCostedOptionIso[T]()(eT)
    private lazy val _safeFrom = fun { p: Ref[CCostedOption[T]] => (p.value, p.costOpt, p.sizeOpt, p.accumulatedCost) }
    override def from(p: Ref[CCostedOption[T]]) =
      tryConvert[CCostedOption[T], (WOption[T], (WOption[Int], (WOption[Size[T]], Int)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(WOption[T], (WOption[Int], (WOption[Size[T]], Int)))]) = {
      val Pair(value, Pair(costOpt, Pair(sizeOpt, accumulatedCost))) = p
      RCCostedOption(value, costOpt, sizeOpt, accumulatedCost)
    }
    lazy val eFrom = pairElement(element[WOption[T]], pairElement(element[WOption[Int]], pairElement(element[WOption[Size[T]]], element[Int])))
    lazy val eTo = new CCostedOptionElem[T](self)
    lazy val resultType = new CCostedOptionIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class CCostedOptionIsoElem[T](eT: Elem[T]) extends Elem[CCostedOptionIso[T]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedOptionCompanionCtor extends CompanionDef[CCostedOptionCompanionCtor] with CCostedOptionCompanion {
    def resultType = CCostedOptionCompanionElem
    override def toString = "CCostedOptionCompanion"
    @scalan.OverloadId("fromData")
    def apply[T](p: Ref[CCostedOptionData[T]]): Ref[CCostedOption[T]] = {
      implicit val eT = p._1.eA
      isoCCostedOption[T].to(p)
    }

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[T](value: Ref[WOption[T]], costOpt: Ref[WOption[Int]], sizeOpt: Ref[WOption[Size[T]]], accumulatedCost: Ref[Int]): Ref[CCostedOption[T]] = {
      assertValueIdForOpCost(value, accumulatedCost)
      mkCCostedOption(value, costOpt, sizeOpt, accumulatedCost)
    }

    def unapply[T](p: Ref[CostedOption[T]]) = unmkCCostedOption(p)
  }
  lazy val CCostedOptionRep: Ref[CCostedOptionCompanionCtor] = new CCostedOptionCompanionCtor
  lazy val RCCostedOption: CCostedOptionCompanionCtor = proxyCCostedOptionCompanion(CCostedOptionRep)
  implicit def proxyCCostedOptionCompanion(p: Ref[CCostedOptionCompanionCtor]): CCostedOptionCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedOptionCompanionCtor])
      p.rhs.asInstanceOf[CCostedOptionCompanionCtor]
    else
      proxyOps[CCostedOptionCompanionCtor](p)
  }

  implicit case object CCostedOptionCompanionElem extends CompanionElem[CCostedOptionCompanionCtor]

  implicit def proxyCCostedOption[T](p: Ref[CCostedOption[T]]): CCostedOption[T] = {
    if (p.rhs.isInstanceOf[CCostedOption[T]@unchecked])
      p.rhs.asInstanceOf[CCostedOption[T]]
    else
      proxyOps[CCostedOption[T]](p)
  }

  implicit class ExtendedCCostedOption[T](p: Ref[CCostedOption[T]]) {
    def toData: Ref[CCostedOptionData[T]] = {
      implicit val eT = p.value.eA
      isoCCostedOption(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedOption[T](implicit eT: Elem[T]): Iso[CCostedOptionData[T], CCostedOption[T]] =
    reifyObject(new CCostedOptionIso[T]()(eT))

  def mkCCostedOption[T]
    (value: Ref[WOption[T]], costOpt: Ref[WOption[Int]], sizeOpt: Ref[WOption[Size[T]]], accumulatedCost: Ref[Int]): Ref[CCostedOption[T]] = {
    new CCostedOptionCtor[T](value, costOpt, sizeOpt, accumulatedCost)
  }
  def unmkCCostedOption[T](p: Ref[CostedOption[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedOptionElem[T] @unchecked =>
      Some((asRep[CCostedOption[T]](p).value, asRep[CCostedOption[T]](p).costOpt, asRep[CCostedOption[T]](p).sizeOpt, asRep[CCostedOption[T]](p).accumulatedCost))
    case _ =>
      None
  }
} // of object CCostedOption
  registerEntityObject("CCostedOption", CCostedOption)

  registerModule(CostedOptionsModule)
}

object CostedOptionsModule extends scalan.ModuleInfo("special.collection", "CostedOptions")
}

trait CostedOptionsModule extends special.collection.impl.CostedOptionsDefs {self: Library =>}
