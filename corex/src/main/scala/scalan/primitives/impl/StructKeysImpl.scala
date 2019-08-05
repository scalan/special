package scalan.primitives

import scalan._
import scala.reflect.classTag
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait StructKeysDefs extends StructKeys {
  self: Structs with ScalanEx =>
import IsoUR._
import Converter._
import IndexStructKey._
import NameStructKey._
import StructKey._

object StructKey extends EntityObject("StructKey") {
  private val StructKeyClass = classOf[StructKey[_]]

  // entityAdapter for StructKey trait
  case class StructKeyAdapter[Schema <: Struct](source: Rep[StructKey[Schema]])
      extends StructKey[Schema]
      with Def[StructKey[Schema]] {
    implicit lazy val eSchema = source.elem.typeArgs("Schema")._1.asElem[Schema]

    val resultType: Elem[StructKey[Schema]] = element[StructKey[Schema]]
    override def transform(t: Transformer) = StructKeyAdapter[Schema](t(source))

    def index: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        StructKeyClass.getMethod("index"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def name: Rep[String] = {
      asRep[String](mkMethodCall(source,
        StructKeyClass.getMethod("name"),
        WrappedArray.empty,
        true, true, element[String]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]): StructKey[Schema] = {
    if (p.rhs.isInstanceOf[StructKey[Schema]@unchecked]) p.rhs.asInstanceOf[StructKey[Schema]]
    else
      StructKeyAdapter(p)
  }

  // familyElem
  class StructKeyElem[Schema <: Struct, To <: StructKey[Schema]](implicit _eSchema: Elem[Schema])
    extends EntityElem[To] {
    def eSchema = _eSchema

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }

  implicit def structKeyElement[Schema <: Struct](implicit eSchema: Elem[Schema]): Elem[StructKey[Schema]] =
    cachedElemByClass(eSchema)(classOf[StructKeyElem[Schema, StructKey[Schema]]])

  implicit case object StructKeyCompanionElem extends CompanionElem[StructKeyCompanionCtor]

  abstract class StructKeyCompanionCtor extends CompanionDef[StructKeyCompanionCtor] {
    def resultType = StructKeyCompanionElem
    override def toString = "StructKey"
  }
  implicit def proxyStructKeyCompanionCtor(p: Rep[StructKeyCompanionCtor]): StructKeyCompanionCtor =
    p.rhs.asInstanceOf[StructKeyCompanionCtor]

  lazy val RStructKey: Rep[StructKeyCompanionCtor] = new StructKeyCompanionCtor {
  }
} // of object StructKey
  registerEntityObject("StructKey", StructKey)

object IndexStructKey extends EntityObject("IndexStructKey") {
  case class IndexStructKeyCtor[Schema <: Struct]
      (override val index: Rep[Int])(implicit eSchema: Elem[Schema])
    extends IndexStructKey[Schema](index) with Def[IndexStructKey[Schema]] {
    lazy val resultType = element[IndexStructKey[Schema]]
    override def transform(t: Transformer) = IndexStructKeyCtor[Schema](t(index))(eSchema)
  }
  // elem for concrete class
  class IndexStructKeyElem[Schema <: Struct](val iso: Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, IndexStructKey[Schema]]
    with ConcreteElem[IndexStructKeyData[Schema], IndexStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }

  // state representation type
  type IndexStructKeyData[Schema <: Struct] = Int

  // 3) Iso for concrete class
  class IndexStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[IndexStructKeyData[Schema], IndexStructKey[Schema]] with Def[IndexStructKeyIso[Schema]] {
    override def transform(t: Transformer) = new IndexStructKeyIso[Schema]()(eSchema)
    private lazy val _safeFrom = fun { p: Rep[IndexStructKey[Schema]] => p.index }
    override def from(p: Rep[IndexStructKey[Schema]]) =
      tryConvert[IndexStructKey[Schema], Int](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Int]) = {
      val index = p
      RIndexStructKey(index)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new IndexStructKeyElem[Schema](self)
    lazy val resultType = new IndexStructKeyIsoElem[Schema](eSchema)
    def productArity = 1
    def productElement(n: Int) = eSchema
  }
  case class IndexStructKeyIsoElem[Schema <: Struct](eSchema: Elem[Schema]) extends Elem[IndexStructKeyIso[Schema]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IndexStructKeyCompanionCtor extends CompanionDef[IndexStructKeyCompanionCtor] {
    def resultType = IndexStructKeyCompanionElem
    override def toString = "IndexStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] =
      mkIndexStructKey(index)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKeyRep: Rep[IndexStructKeyCompanionCtor] = new IndexStructKeyCompanionCtor
  lazy val RIndexStructKey: IndexStructKeyCompanionCtor = proxyIndexStructKeyCompanion(IndexStructKeyRep)
  implicit def proxyIndexStructKeyCompanion(p: Rep[IndexStructKeyCompanionCtor]): IndexStructKeyCompanionCtor = {
    if (p.rhs.isInstanceOf[IndexStructKeyCompanionCtor])
      p.rhs.asInstanceOf[IndexStructKeyCompanionCtor]
    else
      proxyOps[IndexStructKeyCompanionCtor](p)
  }

  implicit case object IndexStructKeyCompanionElem extends CompanionElem[IndexStructKeyCompanionCtor]

  implicit def proxyIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]]): IndexStructKey[Schema] = {
    if (p.rhs.isInstanceOf[IndexStructKey[Schema]@unchecked])
      p.rhs.asInstanceOf[IndexStructKey[Schema]]
    else
      proxyOps[IndexStructKey[Schema]](p)
  }

  implicit class ExtendedIndexStructKey[Schema <: Struct](p: Rep[IndexStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[IndexStructKeyData[Schema]] = {
      isoIndexStructKey(eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]] =
    reifyObject(new IndexStructKeyIso[Schema]()(eSchema))

  def mkIndexStructKey[Schema <: Struct]
    (index: Rep[Int])(implicit eSchema: Elem[Schema]): Rep[IndexStructKey[Schema]] = {
    new IndexStructKeyCtor[Schema](index)
  }
  def unmkIndexStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexStructKeyElem[Schema] @unchecked =>
      Some((asRep[IndexStructKey[Schema]](p).index))
    case _ =>
      None
  }
} // of object IndexStructKey
  registerEntityObject("IndexStructKey", IndexStructKey)

object NameStructKey extends EntityObject("NameStructKey") {
  case class NameStructKeyCtor[Schema <: Struct]
      (override val name: Rep[String])(implicit eSchema: Elem[Schema])
    extends NameStructKey[Schema](name) with Def[NameStructKey[Schema]] {
    lazy val resultType = element[NameStructKey[Schema]]
    override def transform(t: Transformer) = NameStructKeyCtor[Schema](t(name))(eSchema)
  }
  // elem for concrete class
  class NameStructKeyElem[Schema <: Struct](val iso: Iso[NameStructKeyData[Schema], NameStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, NameStructKey[Schema]]
    with ConcreteElem[NameStructKeyData[Schema], NameStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }

  // state representation type
  type NameStructKeyData[Schema <: Struct] = String

  // 3) Iso for concrete class
  class NameStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[NameStructKeyData[Schema], NameStructKey[Schema]] with Def[NameStructKeyIso[Schema]] {
    override def transform(t: Transformer) = new NameStructKeyIso[Schema]()(eSchema)
    private lazy val _safeFrom = fun { p: Rep[NameStructKey[Schema]] => p.name }
    override def from(p: Rep[NameStructKey[Schema]]) =
      tryConvert[NameStructKey[Schema], String](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[String]) = {
      val name = p
      RNameStructKey(name)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new NameStructKeyElem[Schema](self)
    lazy val resultType = new NameStructKeyIsoElem[Schema](eSchema)
    def productArity = 1
    def productElement(n: Int) = eSchema
  }
  case class NameStructKeyIsoElem[Schema <: Struct](eSchema: Elem[Schema]) extends Elem[NameStructKeyIso[Schema]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class NameStructKeyCompanionCtor extends CompanionDef[NameStructKeyCompanionCtor] {
    def resultType = NameStructKeyCompanionElem
    override def toString = "NameStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] =
      mkNameStructKey(name)

    def unapply[Schema <: Struct](p: Rep[StructKey[Schema]]) = unmkNameStructKey(p)
  }
  lazy val NameStructKeyRep: Rep[NameStructKeyCompanionCtor] = new NameStructKeyCompanionCtor
  lazy val RNameStructKey: NameStructKeyCompanionCtor = proxyNameStructKeyCompanion(NameStructKeyRep)
  implicit def proxyNameStructKeyCompanion(p: Rep[NameStructKeyCompanionCtor]): NameStructKeyCompanionCtor = {
    if (p.rhs.isInstanceOf[NameStructKeyCompanionCtor])
      p.rhs.asInstanceOf[NameStructKeyCompanionCtor]
    else
      proxyOps[NameStructKeyCompanionCtor](p)
  }

  implicit case object NameStructKeyCompanionElem extends CompanionElem[NameStructKeyCompanionCtor]

  implicit def proxyNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]]): NameStructKey[Schema] = {
    if (p.rhs.isInstanceOf[NameStructKey[Schema]@unchecked])
      p.rhs.asInstanceOf[NameStructKey[Schema]]
    else
      proxyOps[NameStructKey[Schema]](p)
  }

  implicit class ExtendedNameStructKey[Schema <: Struct](p: Rep[NameStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Rep[NameStructKeyData[Schema]] = {
      isoNameStructKey(eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoNameStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[NameStructKeyData[Schema], NameStructKey[Schema]] =
    reifyObject(new NameStructKeyIso[Schema]()(eSchema))

  def mkNameStructKey[Schema <: Struct]
    (name: Rep[String])(implicit eSchema: Elem[Schema]): Rep[NameStructKey[Schema]] = {
    new NameStructKeyCtor[Schema](name)
  }
  def unmkNameStructKey[Schema <: Struct](p: Rep[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: NameStructKeyElem[Schema] @unchecked =>
      Some((asRep[NameStructKey[Schema]](p).name))
    case _ =>
      None
  }
} // of object NameStructKey
  registerEntityObject("NameStructKey", NameStructKey)

  registerModule(StructKeysModule)
}

object StructKeysModule extends scalan.ModuleInfo("scalan.primitives", "StructKeys")
}

