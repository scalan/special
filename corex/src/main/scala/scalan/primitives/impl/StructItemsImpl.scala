package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import OverloadHack.{Overloaded2, Overloaded1}
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait StructItemsDefs extends StructItems {
  self: Structs with ScalanEx =>
import IsoUR._
import Converter._
import StructItem._
import StructItemBase._
import StructKey._

object StructItem extends EntityObject("StructItem") {
  private val StructItemClass = classOf[StructItem[_, _]]

  // entityAdapter for StructItem trait
  case class StructItemAdapter[Val, Schema <: Struct](source: Ref[StructItem[Val, Schema]])
      extends StructItem[Val, Schema]
      with Def[StructItem[Val, Schema]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val];
implicit lazy val eSchema = source.elem.typeArgs("Schema")._1.asElem[Schema]

    val resultType: Elem[StructItem[Val, Schema]] = element[StructItem[Val, Schema]]
    override def transform(t: Transformer) = StructItemAdapter[Val, Schema](t(source))

    def key: Ref[StructKey[Schema]] = {
      asRep[StructKey[Schema]](mkMethodCall(source,
        StructItemClass.getMethod("key"),
        WrappedArray.empty,
        true, true, element[StructKey[Schema]]))
    }

    def value: Ref[Val] = {
      asRep[Val](mkMethodCall(source,
        StructItemClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Val]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyStructItem[Val, Schema <: Struct](p: Ref[StructItem[Val, Schema]]): StructItem[Val, Schema] = {
    if (p.rhs.isInstanceOf[StructItem[Val, Schema]@unchecked]) p.rhs.asInstanceOf[StructItem[Val, Schema]]
    else
      StructItemAdapter(p)
  }

  // familyElem
  class StructItemElem[Val, Schema <: Struct, To <: StructItem[Val, Schema]](implicit _eVal: Elem[Val], _eSchema: Elem[Schema])
    extends EntityElem[To] {
    def eVal = _eVal
    def eSchema = _eSchema

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Covariant), "Schema" -> (eSchema -> scalan.util.Invariant))
  }

  implicit def structItemElement[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Elem[StructItem[Val, Schema]] =
    cachedElemByClass(eVal, eSchema)(classOf[StructItemElem[Val, Schema, StructItem[Val, Schema]]])

  implicit case object StructItemCompanionElem extends CompanionElem[StructItemCompanionCtor]

  abstract class StructItemCompanionCtor extends CompanionDef[StructItemCompanionCtor] {
    def resultType = StructItemCompanionElem
    override def toString = "StructItem"
  }
  implicit def proxyStructItemCompanionCtor(p: Ref[StructItemCompanionCtor]): StructItemCompanionCtor =
    p.rhs.asInstanceOf[StructItemCompanionCtor]

  lazy val RStructItem: Ref[StructItemCompanionCtor] = new StructItemCompanionCtor {
  }
} // of object StructItem
  registerEntityObject("StructItem", StructItem)

object StructItemBase extends EntityObject("StructItemBase") {
  case class StructItemBaseCtor[Val, Schema <: Struct]
      (override val key: Ref[StructKey[Schema]], override val value: Ref[Val])
    extends StructItemBase[Val, Schema](key, value) with Def[StructItemBase[Val, Schema]] {
    implicit lazy val eVal = value.elem;
implicit lazy val eSchema = key.eSchema

    lazy val resultType = element[StructItemBase[Val, Schema]]
    override def transform(t: Transformer) = StructItemBaseCtor[Val, Schema](t(key), t(value))
  }
  // elem for concrete class
  class StructItemBaseElem[Val, Schema <: Struct](val iso: Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]])(implicit override val eVal: Elem[Val], override val eSchema: Elem[Schema])
    extends StructItemElem[Val, Schema, StructItemBase[Val, Schema]]
    with ConcreteElem[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structItemElement(element[Val], element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))
  }

  // state representation type
  type StructItemBaseData[Val, Schema <: Struct] = (StructKey[Schema], Val)

  // 3) Iso for concrete class
  class StructItemBaseIso[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema])
    extends EntityIso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] with Def[StructItemBaseIso[Val, Schema]] {
    override def transform(t: Transformer) = new StructItemBaseIso[Val, Schema]()(eVal, eSchema)
    private lazy val _safeFrom = fun { p: Ref[StructItemBase[Val, Schema]] => (p.key, p.value) }
    override def from(p: Ref[StructItemBase[Val, Schema]]) =
      tryConvert[StructItemBase[Val, Schema], (StructKey[Schema], Val)](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(StructKey[Schema], Val)]) = {
      val Pair(key, value) = p
      RStructItemBase(key, value)
    }
    lazy val eFrom = pairElement(element[StructKey[Schema]], element[Val])
    lazy val eTo = new StructItemBaseElem[Val, Schema](self)
    lazy val resultType = new StructItemBaseIsoElem[Val, Schema](eVal, eSchema)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eVal
      case 1 => eSchema
    }
  }
  case class StructItemBaseIsoElem[Val, Schema <: Struct](eVal: Elem[Val], eSchema: Elem[Schema]) extends Elem[StructItemBaseIso[Val, Schema]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant), "Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class StructItemBaseCompanionCtor extends CompanionDef[StructItemBaseCompanionCtor] {
    def resultType = StructItemBaseCompanionElem
    override def toString = "StructItemBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val, Schema <: Struct](p: Ref[StructItemBaseData[Val, Schema]]): Ref[StructItemBase[Val, Schema]] = {
      implicit val eVal = p._2.elem;
implicit val eSchema = p._1.eSchema
      isoStructItemBase[Val, Schema].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val, Schema <: Struct](key: Ref[StructKey[Schema]], value: Ref[Val]): Ref[StructItemBase[Val, Schema]] =
      mkStructItemBase(key, value)

    def unapply[Val, Schema <: Struct](p: Ref[StructItem[Val, Schema]]) = unmkStructItemBase(p)
  }
  lazy val StructItemBaseRep: Ref[StructItemBaseCompanionCtor] = new StructItemBaseCompanionCtor
  lazy val RStructItemBase: StructItemBaseCompanionCtor = proxyStructItemBaseCompanion(StructItemBaseRep)
  implicit def proxyStructItemBaseCompanion(p: Ref[StructItemBaseCompanionCtor]): StructItemBaseCompanionCtor = {
    if (p.rhs.isInstanceOf[StructItemBaseCompanionCtor])
      p.rhs.asInstanceOf[StructItemBaseCompanionCtor]
    else
      proxyOps[StructItemBaseCompanionCtor](p)
  }

  implicit case object StructItemBaseCompanionElem extends CompanionElem[StructItemBaseCompanionCtor]

  implicit def proxyStructItemBase[Val, Schema <: Struct](p: Ref[StructItemBase[Val, Schema]]): StructItemBase[Val, Schema] = {
    if (p.rhs.isInstanceOf[StructItemBase[Val, Schema]@unchecked])
      p.rhs.asInstanceOf[StructItemBase[Val, Schema]]
    else
      proxyOps[StructItemBase[Val, Schema]](p)
  }

  implicit class ExtendedStructItemBase[Val, Schema <: Struct](p: Ref[StructItemBase[Val, Schema]]) {
    def toData: Ref[StructItemBaseData[Val, Schema]] = {
      implicit val eVal = p.value.elem;
implicit val eSchema = p.key.eSchema
      isoStructItemBase(eVal, eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoStructItemBase[Val, Schema <: Struct](implicit eVal: Elem[Val], eSchema: Elem[Schema]): Iso[StructItemBaseData[Val, Schema], StructItemBase[Val, Schema]] =
    reifyObject(new StructItemBaseIso[Val, Schema]()(eVal, eSchema))

  def mkStructItemBase[Val, Schema <: Struct]
    (key: Ref[StructKey[Schema]], value: Ref[Val]): Ref[StructItemBase[Val, Schema]] = {
    new StructItemBaseCtor[Val, Schema](key, value)
  }
  def unmkStructItemBase[Val, Schema <: Struct](p: Ref[StructItem[Val, Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: StructItemBaseElem[Val, Schema] @unchecked =>
      Some((asRep[StructItemBase[Val, Schema]](p).key, asRep[StructItemBase[Val, Schema]](p).value))
    case _ =>
      None
  }
} // of object StructItemBase
  registerEntityObject("StructItemBase", StructItemBase)

  registerModule(StructItemsModule)
}

object StructItemsModule extends scalan.ModuleInfo("scalan.primitives", "StructItems")
}

