package scalan.primitives

import scalan._
import scala.reflect.classTag

trait StructKeys extends ViewsModule with Entities with BaseEx { self: Structs with ScalanEx =>
  import IsoUR._
  
  type SKey[S <: Struct] = Ref[StructKey[S]]
  trait StructKey[Schema <: Struct] extends Def[StructKey[Schema]] {
    def eSchema: Elem[Schema]
    def index: Ref[Int]
    def name: Ref[String]
  }
  @Isospec
  abstract class IndexStructKey[Schema <: Struct]
      (val index: Ref[Int])
      (implicit val eSchema: Elem[Schema]) extends StructKey[Schema] {
    def name: Ref[String] = {
      val i = valueFromRep(index)
      eSchema.fieldNames(i)
    }
    override def toString = s"${eSchema.fieldsString}[$index]"
  }

  @Isospec
  abstract class NameStructKey[Schema <: Struct]
      (val name: Ref[String])
      (implicit val eSchema: Elem[Schema]) extends StructKey[Schema] {
    def index: Ref[Int] = {
      val n = valueFromRep(name)
      eSchema.findFieldIndex(n)
    }
    override def toString = s"${eSchema.fieldsString}.$name"
  }

}

trait StructKeysModule extends impl.StructKeysDefs {self: Structs with ScalanEx =>
  type KSet = Ref[KeySet]
  trait KeySet {
    def keys: Seq[String]
  }
  class KeySetCompanion {
    def apply(names: Seq[String]) = keyset_create(names)
  }
  val KeySet: KeySetCompanion = new KeySetCompanion

  case class KeySetSeq(keys: Seq[String]) extends KeySet

  val KeySetRType = RType.fromClassTag(classTag[KeySet])

  class KeySetElem extends BaseElemLiftable[KeySet](KeySetSeq(Seq()), KeySetRType)
  implicit lazy val KeySetElement: Elem[KeySet] = new KeySetElem
  def keyset_create(keys: Seq[String]): Ref[KeySet] = KeySetDef(keys)
  case class KeySetDef(keys: Seq[String]) extends BaseDef[KeySet] {
    override def transform(t: Transformer) = KeySetDef(keys)
    override def toString = s"KeySet(${keys.mkString(",")})"
  }
}

