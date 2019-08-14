package scalan.primitives

import scala.annotation.unchecked.uncheckedVariance
import scalan._
import scala.reflect.runtime.universe._
import OverloadHack.{Overloaded2, Overloaded1}

trait StructItems extends ViewsModule with Entities { self: Structs with ScalanEx =>
  trait StructItem[@uncheckedVariance +Val, Schema <: Struct] extends Def[StructItem[Val @uncheckedVariance, Schema]] {
    def eVal: Elem[Val @uncheckedVariance]
    def eSchema: Elem[Schema]
    def key: Ref[StructKey[Schema]]
    def value: Ref[Val]
  }

  @Isospec
  abstract class StructItemBase[Val, Schema <: Struct]
        (val key: Ref[StructKey[Schema]], val value: Ref[Val])
    extends StructItem[Val, Schema]

}

trait StructItemsModule extends impl.StructItemsDefs { self: Structs with ScalanEx =>
  import StructKey._
  import IndexStructKey._
  import StructItem._
  import StructItemBase._

  def struct_getItem[S <: Struct](s: Ref[S], i: Int)(implicit o1: Overloaded1): Ref[StructItem[_,S]] = {
    val value = s.getUntyped(i)
    val eS = s.elem
    val key = RIndexStructKey[S](i)(eS)
    RStructItemBase(key, value)
  }

  def struct_setItem[S <: Struct](s: Ref[S], i: Ref[Int], v: Ref[_]): Ref[S] = {
    updateField(s, s.elem.fieldNames(valueFromRep(i)), v)
  }

  trait StructItemFunctor[S <: Struct] extends Functor[({type f[x] = StructItem[x,S]})#f] {
    implicit def eS: Elem[S]
    def lift[T](implicit eT: Elem[T]) = element[StructItem[T,S]]
    def unlift[T](implicit eFT: Elem[StructItem[T,S]]) = eFT.asInstanceOf[StructItemElem[T,S,_]].eVal
    def unapply[T](e: Elem[_]) = e match {
      case e: StructItemElem[_, _, _] => Some(asElem[StructItem[T,S]](e))
      case _ => None
    }
    def map[A,B](xs: Ref[StructItem[A,S]])(f: Ref[A] => Ref[B]) = {
      val res = f(xs.value)
      implicit val eB = res.elem
      RStructItemBase(xs.key, res)
    }
  }
  implicit def structItemContainer[S <: Struct : Elem]: Functor[({type f[x] = StructItem[x,S]})#f] =
    new StructItemFunctor[S] { def eS = element[S] }

  implicit class StructElemExtensionsForStructItem[S <: Struct](eS: Elem[S]) {
    def getItemElem[V](i: Int): Elem[StructItem[V, S]] = {
      val eV = asElem[V](eS(i))
      structItemElement(eV, eS)
    }
    def getItemElem[V](fieldName: String): Elem[StructItem[V, S]] = {
      val eV = asElem[V](eS(fieldName))
      structItemElement(eV, eS)
    }
  }

  implicit class StructExtensionsForStructItem[S <: Struct](s: Ref[S]) {
    def getItem[A](i: Int): Ref[StructItem[A, S]] = {
      val item = struct_getItem(s, i)
      item.asInstanceOf[Ref[StructItem[A,S]]]
    }
    def getItem[A](i: Ref[Int]): Ref[StructItem[A, S]] = struct_getItem(s, i).asInstanceOf[Ref[StructItem[A,S]]]
    def getItem[A](k: Ref[StructKey[S]])(implicit o: Overloaded2): Ref[StructItem[A,S]] = struct_getItem(s, k.index).asInstanceOf[Ref[StructItem[A,S]]]
    def setItem(i: Ref[Int], v: Ref[_]): Ref[S] = struct_setItem(s, i, v)
    def setItem(k: Ref[StructKey[S]], v: Ref[_])(implicit o: Overloaded2): Ref[S] = struct_setItem(s, k.index, v)
  }

  def struct_getItem[S <: Struct](s: Ref[S], i: Ref[Int]): Ref[StructItem[_,S]] =
    i match {
      case Def(Const(i: Int)) => struct_getItem(s, i)
      case _ =>
        ???
    }
}

