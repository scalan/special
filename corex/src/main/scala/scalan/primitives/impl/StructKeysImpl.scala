package scalan.primitives

import scalan._
import scala.reflect.classTag
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait StructKeysDefs extends StructKeys {
  self: Structs with ScalanEx =>
import IsoUR._
import IndexStructKey._
import NameStructKey._
import StructKey._

object StructKey extends EntityObject("StructKey") {
  private val StructKeyClass = classOf[StructKey[_]]

  // entityAdapter for StructKey trait
  case class StructKeyAdapter[Schema <: Struct](source: Ref[StructKey[Schema]])
      extends StructKey[Schema]
      with Def[StructKey[Schema]] {
    implicit lazy val eSchema = source.elem.typeArgs("Schema")._1.asInstanceOf[Elem[Schema]]

    val resultType: Elem[StructKey[Schema]] = element[StructKey[Schema]]
    override def transform(t: Transformer) = StructKeyAdapter[Schema](t(source))

    def index: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        StructKeyClass.getMethod("index"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def name: Ref[String] = {
      asRep[String](mkMethodCall(source,
        StructKeyClass.getMethod("name"),
        WrappedArray.empty,
        true, true, element[String]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefStructKey[Schema <: Struct](p: Ref[StructKey[Schema]]): StructKey[Schema] = {
    if (p.node.isInstanceOf[StructKey[Schema]@unchecked]) p.node.asInstanceOf[StructKey[Schema]]
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
  implicit def unrefStructKeyCompanionCtor(p: Ref[StructKeyCompanionCtor]): StructKeyCompanionCtor =
    p.node.asInstanceOf[StructKeyCompanionCtor]

  lazy val RStructKey: Ref[StructKeyCompanionCtor] = new StructKeyCompanionCtor {
  }
} // of object StructKey
  registerEntityObject("StructKey", StructKey)

object IndexStructKey extends EntityObject("IndexStructKey") {
  case class IndexStructKeyCtor[Schema <: Struct]
      (override val index: Ref[Int])(implicit eSchema: Elem[Schema])
    extends IndexStructKey[Schema](index) with Def[IndexStructKey[Schema]] {
    lazy val resultType = element[IndexStructKey[Schema]]
    override def transform(t: Transformer) = IndexStructKeyCtor[Schema](t(index))(eSchema)
  }

  // state representation type
  type IndexStructKeyData[Schema <: Struct] = Int

  // elem for concrete class
  class IndexStructKeyElem[Schema <: Struct](val iso: Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, IndexStructKey[Schema]]
    with ConcreteElem[IndexStructKeyData[Schema], IndexStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }

  // 3) Iso for concrete class
  class IndexStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[IndexStructKeyData[Schema], IndexStructKey[Schema]] with Def[IndexStructKeyIso[Schema]] {
    override def transform(t: Transformer) = new IndexStructKeyIso[Schema]()(eSchema)
    private lazy val _safeFrom = fun { p: Ref[IndexStructKey[Schema]] => p.index }
    override def from(p: Ref[IndexStructKey[Schema]]) =
      tryConvert[IndexStructKey[Schema], Int](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Int]) = {
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

  implicit class ExtendedIndexStructKey[Schema <: Struct](p: Ref[IndexStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Ref[IndexStructKeyData[Schema]] = {
      isoIndexStructKey(eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[IndexStructKeyData[Schema], IndexStructKey[Schema]] =
    reifyObject(new IndexStructKeyIso[Schema]()(eSchema))

  // 4) constructor and deconstructor
  class IndexStructKeyCompanionCtor extends CompanionDef[IndexStructKeyCompanionCtor] {
    def resultType = IndexStructKeyCompanionElem
    override def toString = "IndexStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](index: Ref[Int])(implicit eSchema: Elem[Schema]): Ref[IndexStructKey[Schema]] =
      mkIndexStructKey(index)

    def unapply[Schema <: Struct](p: Ref[StructKey[Schema]]) = unmkIndexStructKey(p)
  }
  lazy val IndexStructKeyRef: Ref[IndexStructKeyCompanionCtor] = new IndexStructKeyCompanionCtor
  lazy val RIndexStructKey: IndexStructKeyCompanionCtor = unrefIndexStructKeyCompanion(IndexStructKeyRef)
  implicit def unrefIndexStructKeyCompanion(p: Ref[IndexStructKeyCompanionCtor]): IndexStructKeyCompanionCtor = {
    if (p.node.isInstanceOf[IndexStructKeyCompanionCtor])
      p.node.asInstanceOf[IndexStructKeyCompanionCtor]
    else
      unrefDelegate[IndexStructKeyCompanionCtor](p)
  }

  implicit case object IndexStructKeyCompanionElem extends CompanionElem[IndexStructKeyCompanionCtor]

  implicit def unrefIndexStructKey[Schema <: Struct](p: Ref[IndexStructKey[Schema]]): IndexStructKey[Schema] = {
    if (p.node.isInstanceOf[IndexStructKey[Schema]@unchecked])
      p.node.asInstanceOf[IndexStructKey[Schema]]
    else
      unrefDelegate[IndexStructKey[Schema]](p)
  }

  def mkIndexStructKey[Schema <: Struct]
    (index: Ref[Int])(implicit eSchema: Elem[Schema]): Ref[IndexStructKey[Schema]] = {
    new IndexStructKeyCtor[Schema](index)
  }
  def unmkIndexStructKey[Schema <: Struct](p: Ref[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexStructKeyElem[Schema] @unchecked =>
      Some((asRep[IndexStructKey[Schema]](p).index))
    case _ =>
      None
  }
} // of object IndexStructKey
  registerEntityObject("IndexStructKey", IndexStructKey)

object NameStructKey extends EntityObject("NameStructKey") {
  case class NameStructKeyCtor[Schema <: Struct]
      (override val name: Ref[String])(implicit eSchema: Elem[Schema])
    extends NameStructKey[Schema](name) with Def[NameStructKey[Schema]] {
    lazy val resultType = element[NameStructKey[Schema]]
    override def transform(t: Transformer) = NameStructKeyCtor[Schema](t(name))(eSchema)
  }

  // state representation type
  type NameStructKeyData[Schema <: Struct] = String

  // elem for concrete class
  class NameStructKeyElem[Schema <: Struct](val iso: Iso[NameStructKeyData[Schema], NameStructKey[Schema]])(implicit override val eSchema: Elem[Schema])
    extends StructKeyElem[Schema, NameStructKey[Schema]]
    with ConcreteElem[NameStructKeyData[Schema], NameStructKey[Schema]] {
    override lazy val parent: Option[Elem[_]] = Some(structKeyElement(element[Schema]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Schema" -> (eSchema -> scalan.util.Invariant))
  }

  // 3) Iso for concrete class
  class NameStructKeyIso[Schema <: Struct](implicit eSchema: Elem[Schema])
    extends EntityIso[NameStructKeyData[Schema], NameStructKey[Schema]] with Def[NameStructKeyIso[Schema]] {
    override def transform(t: Transformer) = new NameStructKeyIso[Schema]()(eSchema)
    private lazy val _safeFrom = fun { p: Ref[NameStructKey[Schema]] => p.name }
    override def from(p: Ref[NameStructKey[Schema]]) =
      tryConvert[NameStructKey[Schema], String](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[String]) = {
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

  implicit class ExtendedNameStructKey[Schema <: Struct](p: Ref[NameStructKey[Schema]])(implicit eSchema: Elem[Schema]) {
    def toData: Ref[NameStructKeyData[Schema]] = {
      isoNameStructKey(eSchema).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoNameStructKey[Schema <: Struct](implicit eSchema: Elem[Schema]): Iso[NameStructKeyData[Schema], NameStructKey[Schema]] =
    reifyObject(new NameStructKeyIso[Schema]()(eSchema))

  // 4) constructor and deconstructor
  class NameStructKeyCompanionCtor extends CompanionDef[NameStructKeyCompanionCtor] {
    def resultType = NameStructKeyCompanionElem
    override def toString = "NameStructKeyCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Schema <: Struct](name: Ref[String])(implicit eSchema: Elem[Schema]): Ref[NameStructKey[Schema]] =
      mkNameStructKey(name)

    def unapply[Schema <: Struct](p: Ref[StructKey[Schema]]) = unmkNameStructKey(p)
  }
  lazy val NameStructKeyRef: Ref[NameStructKeyCompanionCtor] = new NameStructKeyCompanionCtor
  lazy val RNameStructKey: NameStructKeyCompanionCtor = unrefNameStructKeyCompanion(NameStructKeyRef)
  implicit def unrefNameStructKeyCompanion(p: Ref[NameStructKeyCompanionCtor]): NameStructKeyCompanionCtor = {
    if (p.node.isInstanceOf[NameStructKeyCompanionCtor])
      p.node.asInstanceOf[NameStructKeyCompanionCtor]
    else
      unrefDelegate[NameStructKeyCompanionCtor](p)
  }

  implicit case object NameStructKeyCompanionElem extends CompanionElem[NameStructKeyCompanionCtor]

  implicit def unrefNameStructKey[Schema <: Struct](p: Ref[NameStructKey[Schema]]): NameStructKey[Schema] = {
    if (p.node.isInstanceOf[NameStructKey[Schema]@unchecked])
      p.node.asInstanceOf[NameStructKey[Schema]]
    else
      unrefDelegate[NameStructKey[Schema]](p)
  }

  def mkNameStructKey[Schema <: Struct]
    (name: Ref[String])(implicit eSchema: Elem[Schema]): Ref[NameStructKey[Schema]] = {
    new NameStructKeyCtor[Schema](name)
  }
  def unmkNameStructKey[Schema <: Struct](p: Ref[StructKey[Schema]]) = p.elem.asInstanceOf[Elem[_]] match {
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

