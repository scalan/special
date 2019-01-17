package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CollsOverArraysDefs extends scalan.Scalan with CollsOverArrays {
  self: Library =>
import IsoUR._
import Converter._
import CReplColl._
import Coll._
import CollBuilder._
import CollOverArray._
import CollOverArrayBuilder._
import Monoid._
import MonoidBuilder._
import MonoidBuilderInst._
import PairColl._
import PairOfCols._
import ReplColl._
import WArray._

object CollOverArray extends EntityObject("CollOverArray") {
  case class CollOverArrayCtor[A]
      (override val arr: Rep[WArray[A]])
    extends CollOverArray[A](arr) with Def[CollOverArray[A]] {
    implicit lazy val eA = arr.eT

    lazy val selfType = element[CollOverArray[A]]
    override def transform(t: Transformer) = CollOverArrayCtor[A](t(arr))
    private val thisClass = classOf[Coll[_]]

    override def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, false, element[A]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    override def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, false, element[Coll[A]]))
    }

    override def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(self,
        thisClass.getMethod("indices"),
        List(),
        true, false, element[Coll[Int]]))
    }

    override def flatMap[B](f: Rep[A => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(self,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, false, element[Coll[B]]))
    }

    override def segmentLength(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def indexWhere(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def lastIndexWhere(p: Rep[A => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, false, element[Int]))
    }

    override def partition(pred: Rep[A => Boolean]): Rep[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(self,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, false, element[(Coll[A], Coll[A])]))
    }

    override def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, false, element[Coll[A]]))
    }

    override def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, false, element[Coll[A]]))
    }

    override def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, false, element[Coll[A]]))
    }

    override def mapReduce[K, V](m: Rep[A => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(self,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, false, element[Coll[(K, V)]]))
    }
  }
  // elem for concrete class
  class CollOverArrayElem[A](val iso: Iso[CollOverArrayData[A], CollOverArray[A]])(implicit override val eA: Elem[A])
    extends CollElem[A, CollOverArray[A]]
    with ConcreteElem1[A, CollOverArrayData[A], CollOverArray[A], Coll] {
    override lazy val parent: Option[Elem[_]] = Some(collElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertColl(x: Rep[Coll[A]]) = RCollOverArray(x.arr)
    override def getDefaultRep = RCollOverArray(element[WArray[A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CollOverArray[A]]
    }
  }

  // state representation type
  type CollOverArrayData[A] = WArray[A]

  // 3) Iso for concrete class
  class CollOverArrayIso[A](implicit eA: Elem[A])
    extends EntityIso[CollOverArrayData[A], CollOverArray[A]] with Def[CollOverArrayIso[A]] {
    override def transform(t: Transformer) = new CollOverArrayIso[A]()(eA)
    private lazy val _safeFrom = fun { p: Rep[CollOverArray[A]] => p.arr }
    override def from(p: Rep[CollOverArray[A]]) =
      tryConvert[CollOverArray[A], WArray[A]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WArray[A]]) = {
      val arr = p
      RCollOverArray(arr)
    }
    lazy val eFrom = element[WArray[A]]
    lazy val eTo = new CollOverArrayElem[A](self)
    lazy val selfType = new CollOverArrayIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class CollOverArrayIsoElem[A](eA: Elem[A]) extends Elem[CollOverArrayIso[A]] {
    def getDefaultRep = reifyObject(new CollOverArrayIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CollOverArrayIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CollOverArrayCompanionCtor extends CompanionDef[CollOverArrayCompanionCtor] with CollOverArrayCompanion {
    def selfType = CollOverArrayCompanionElem
    override def toString = "CollOverArrayCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A](arr: Rep[WArray[A]]): Rep[CollOverArray[A]] =
      mkCollOverArray(arr)

    def unapply[A](p: Rep[Coll[A]]) = unmkCollOverArray(p)
  }
  lazy val CollOverArrayRep: Rep[CollOverArrayCompanionCtor] = new CollOverArrayCompanionCtor
  lazy val RCollOverArray: CollOverArrayCompanionCtor = proxyCollOverArrayCompanion(CollOverArrayRep)
  implicit def proxyCollOverArrayCompanion(p: Rep[CollOverArrayCompanionCtor]): CollOverArrayCompanionCtor = {
    if (p.rhs.isInstanceOf[CollOverArrayCompanionCtor])
      p.rhs.asInstanceOf[CollOverArrayCompanionCtor]
    else
      proxyOps[CollOverArrayCompanionCtor](p)
  }

  implicit case object CollOverArrayCompanionElem extends CompanionElem[CollOverArrayCompanionCtor] {
    lazy val tag = weakTypeTag[CollOverArrayCompanionCtor]
    protected def getDefaultRep = CollOverArrayRep
  }

  implicit def proxyCollOverArray[A](p: Rep[CollOverArray[A]]): CollOverArray[A] =
    proxyOps[CollOverArray[A]](p)

  implicit class ExtendedCollOverArray[A](p: Rep[CollOverArray[A]]) {
    def toData: Rep[CollOverArrayData[A]] = {
      implicit val eA = p.arr.eT
      isoCollOverArray(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCollOverArray[A](implicit eA: Elem[A]): Iso[CollOverArrayData[A], CollOverArray[A]] =
    reifyObject(new CollOverArrayIso[A]()(eA))

  def mkCollOverArray[A]
    (arr: Rep[WArray[A]]): Rep[CollOverArray[A]] = {
    new CollOverArrayCtor[A](arr)
  }
  def unmkCollOverArray[A](p: Rep[Coll[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CollOverArrayElem[A] @unchecked =>
      Some((asRep[CollOverArray[A]](p).arr))
    case _ =>
      None
  }

    object CollOverArrayMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CollOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CollOverArray[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CollOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[CollOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CollOverArray[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CollOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Rep[CollOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "indices" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CollOverArray[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CollOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object segmentLength {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "segmentLength" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "indexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lastIndexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "lastIndexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object partition {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "partition" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object patch {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "patch" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updated {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "updated" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "updateMany" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArray[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayElem[_]] && method.getName == "mapReduce" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArray[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArray[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CollOverArrayCompanionMethods {
  }
} // of object CollOverArray
  registerEntityObject("CollOverArray", CollOverArray)

object CollOverArrayBuilder extends EntityObject("CollOverArrayBuilder") {
  case class CollOverArrayBuilderCtor
      ()
    extends CollOverArrayBuilder() with Def[CollOverArrayBuilder] {
    lazy val selfType = element[CollOverArrayBuilder]
    override def transform(t: Transformer) = CollOverArrayBuilderCtor()
    private val thisClass = classOf[CollBuilder]

    override def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, false, element[Coll[T]]))
    }

    override def fromArray[T](arr: Rep[WArray[T]]): Rep[Coll[T]] = {
      implicit val eT = arr.eT
      asRep[Coll[T]](mkMethodCall(self,
        thisClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, false, element[Coll[T]]))
    }

    override def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(self,
        thisClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, false, element[Coll[T]]))
    }

    override def xor(left: Rep[Coll[Byte]], right: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, false, element[Coll[Byte]]))
    }

    override def emptyColl[T](implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        thisClass.getMethod("emptyColl", classOf[Elem[_]]),
        List(cT),
        true, false, element[Coll[T]]))
    }
  }
  // elem for concrete class
  class CollOverArrayBuilderElem(val iso: Iso[CollOverArrayBuilderData, CollOverArrayBuilder])
    extends CollBuilderElem[CollOverArrayBuilder]
    with ConcreteElem[CollOverArrayBuilderData, CollOverArrayBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(collBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCollBuilder(x: Rep[CollBuilder]) = RCollOverArrayBuilder()
    override def getDefaultRep = RCollOverArrayBuilder()
    override lazy val tag = {
      weakTypeTag[CollOverArrayBuilder]
    }
  }

  // state representation type
  type CollOverArrayBuilderData = Unit

  // 3) Iso for concrete class
  class CollOverArrayBuilderIso
    extends EntityIso[CollOverArrayBuilderData, CollOverArrayBuilder] with Def[CollOverArrayBuilderIso] {
    override def transform(t: Transformer) = new CollOverArrayBuilderIso()
    private lazy val _safeFrom = fun { p: Rep[CollOverArrayBuilder] => () }
    override def from(p: Rep[CollOverArrayBuilder]) =
      tryConvert[CollOverArrayBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RCollOverArrayBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new CollOverArrayBuilderElem(self)
    lazy val selfType = new CollOverArrayBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CollOverArrayBuilderIsoElem() extends Elem[CollOverArrayBuilderIso] {
    def getDefaultRep = reifyObject(new CollOverArrayBuilderIso())
    lazy val tag = {
      weakTypeTag[CollOverArrayBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CollOverArrayBuilderCompanionCtor extends CompanionDef[CollOverArrayBuilderCompanionCtor] with CollOverArrayBuilderCompanion {
    def selfType = CollOverArrayBuilderCompanionElem
    override def toString = "CollOverArrayBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CollOverArrayBuilderData]): Rep[CollOverArrayBuilder] = {
      isoCollOverArrayBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[CollOverArrayBuilder] =
      mkCollOverArrayBuilder()

    def unapply(p: Rep[CollBuilder]) = unmkCollOverArrayBuilder(p)
  }
  lazy val CollOverArrayBuilderRep: Rep[CollOverArrayBuilderCompanionCtor] = new CollOverArrayBuilderCompanionCtor
  lazy val RCollOverArrayBuilder: CollOverArrayBuilderCompanionCtor = proxyCollOverArrayBuilderCompanion(CollOverArrayBuilderRep)
  implicit def proxyCollOverArrayBuilderCompanion(p: Rep[CollOverArrayBuilderCompanionCtor]): CollOverArrayBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[CollOverArrayBuilderCompanionCtor])
      p.rhs.asInstanceOf[CollOverArrayBuilderCompanionCtor]
    else
      proxyOps[CollOverArrayBuilderCompanionCtor](p)
  }

  implicit case object CollOverArrayBuilderCompanionElem extends CompanionElem[CollOverArrayBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CollOverArrayBuilderCompanionCtor]
    protected def getDefaultRep = CollOverArrayBuilderRep
  }

  implicit def proxyCollOverArrayBuilder(p: Rep[CollOverArrayBuilder]): CollOverArrayBuilder =
    proxyOps[CollOverArrayBuilder](p)

  implicit class ExtendedCollOverArrayBuilder(p: Rep[CollOverArrayBuilder]) {
    def toData: Rep[CollOverArrayBuilderData] = {
      isoCollOverArrayBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCollOverArrayBuilder: Iso[CollOverArrayBuilderData, CollOverArrayBuilder] =
    reifyObject(new CollOverArrayBuilderIso())

  def mkCollOverArrayBuilder
    (): Rep[CollOverArrayBuilder] = {
    new CollOverArrayBuilderCtor()
  }
  def unmkCollOverArrayBuilder(p: Rep[CollBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CollOverArrayBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object CollOverArrayBuilderMethods {
    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[CollOverArrayBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "Monoids" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CollOverArrayBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CollOverArrayBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pairColl {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "pairColl" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "fromArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Rep[WArray[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "replicate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object emptyColl {
      def unapply(d: Def[_]): Nullable[(Rep[CollOverArrayBuilder], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollOverArrayBuilderElem] && method.getName == "emptyColl" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollOverArrayBuilder], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollOverArrayBuilder], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CollOverArrayBuilderCompanionMethods {
  }
} // of object CollOverArrayBuilder
  registerEntityObject("CollOverArrayBuilder", CollOverArrayBuilder)

object PairOfCols extends EntityObject("PairOfCols") {
  case class PairOfColsCtor[L, R]
      (override val ls: Rep[Coll[L]], override val rs: Rep[Coll[R]])
    extends PairOfCols[L, R](ls, rs) with Def[PairOfCols[L, R]] {
    implicit lazy val eL = ls.eA;
implicit lazy val eR = rs.eA
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[PairOfCols[L, R]]
    override def transform(t: Transformer) = PairOfColsCtor[L, R](t(ls), t(rs))
    private val thisClass = classOf[Coll[_]]

    override def getOrElse(i: Rep[Int], default: Rep[(L, R)]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, false, element[(L, R)]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, (L, R))) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    override def sum(m: Rep[Monoid[(L, R)]]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(self,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, false, element[(L, R)]))
    }

    override def flatMap[B](f: Rep[((L, R)) => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(self,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, false, element[Coll[B]]))
    }

    override def segmentLength(p: Rep[((L, R)) => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def indexWhere(p: Rep[((L, R)) => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def lastIndexWhere(p: Rep[((L, R)) => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, false, element[Int]))
    }

    override def partition(pred: Rep[((L, R)) => Boolean]): Rep[(Coll[(L, R)], Coll[(L, R)])] = {
      asRep[(Coll[(L, R)], Coll[(L, R)])](mkMethodCall(self,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, false, element[(Coll[(L, R)], Coll[(L, R)])](pairElement(collElement(pairElement(eL,eR)), collElement(pairElement(eL,eR))))))
    }

    override def patch(from: Rep[Int], patch: Rep[Coll[(L, R)]], replaced: Rep[Int]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(self,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, false, element[Coll[(L, R)]]))
    }

    override def updated(index: Rep[Int], elem: Rep[(L, R)]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(self,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, false, element[Coll[(L, R)]]))
    }

    override def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(self,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, false, element[Coll[(L, R)]]))
    }

    override def mapReduce[K, V](m: Rep[((L, R)) => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(self,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, false, element[Coll[(K, V)]]))
    }
  }
  // elem for concrete class
  class PairOfColsElem[L, R](val iso: Iso[PairOfColsData[L, R], PairOfCols[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends PairCollElem[L, R, PairOfCols[L, R]]
    with ConcreteElem[PairOfColsData[L, R], PairOfCols[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(pairCollElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertPairColl(x: Rep[PairColl[L, R]]) = RPairOfCols(x.ls, x.rs)
    override def getDefaultRep = RPairOfCols(element[Coll[L]].defaultRepValue, element[Coll[R]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairOfCols[L, R]]
    }
  }

  // state representation type
  type PairOfColsData[L, R] = (Coll[L], Coll[R])

  // 3) Iso for concrete class
  class PairOfColsIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[PairOfColsData[L, R], PairOfCols[L, R]] with Def[PairOfColsIso[L, R]] {
    override def transform(t: Transformer) = new PairOfColsIso[L, R]()(eL, eR)
    private lazy val _safeFrom = fun { p: Rep[PairOfCols[L, R]] => (p.ls, p.rs) }
    override def from(p: Rep[PairOfCols[L, R]]) =
      tryConvert[PairOfCols[L, R], (Coll[L], Coll[R])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Coll[L], Coll[R])]) = {
      val Pair(ls, rs) = p
      RPairOfCols(ls, rs)
    }
    lazy val eFrom = pairElement(element[Coll[L]], element[Coll[R]])
    lazy val eTo = new PairOfColsElem[L, R](self)
    lazy val selfType = new PairOfColsIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class PairOfColsIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[PairOfColsIso[L, R]] {
    def getDefaultRep = reifyObject(new PairOfColsIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairOfColsIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairOfColsCompanionCtor extends CompanionDef[PairOfColsCompanionCtor] with PairOfColsCompanion {
    def selfType = PairOfColsCompanionElem
    override def toString = "PairOfColsCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[PairOfColsData[L, R]]): Rep[PairOfCols[L, R]] = {
      implicit val eL = p._1.eA;
implicit val eR = p._2.eA
      isoPairOfCols[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](ls: Rep[Coll[L]], rs: Rep[Coll[R]]): Rep[PairOfCols[L, R]] =
      mkPairOfCols(ls, rs)

    def unapply[L, R](p: Rep[PairColl[L, R]]) = unmkPairOfCols(p)
  }
  lazy val PairOfColsRep: Rep[PairOfColsCompanionCtor] = new PairOfColsCompanionCtor
  lazy val RPairOfCols: PairOfColsCompanionCtor = proxyPairOfColsCompanion(PairOfColsRep)
  implicit def proxyPairOfColsCompanion(p: Rep[PairOfColsCompanionCtor]): PairOfColsCompanionCtor = {
    if (p.rhs.isInstanceOf[PairOfColsCompanionCtor])
      p.rhs.asInstanceOf[PairOfColsCompanionCtor]
    else
      proxyOps[PairOfColsCompanionCtor](p)
  }

  implicit case object PairOfColsCompanionElem extends CompanionElem[PairOfColsCompanionCtor] {
    lazy val tag = weakTypeTag[PairOfColsCompanionCtor]
    protected def getDefaultRep = PairOfColsRep
  }

  implicit def proxyPairOfCols[L, R](p: Rep[PairOfCols[L, R]]): PairOfCols[L, R] =
    proxyOps[PairOfCols[L, R]](p)

  implicit class ExtendedPairOfCols[L, R](p: Rep[PairOfCols[L, R]]) {
    def toData: Rep[PairOfColsData[L, R]] = {
      implicit val eL = p.ls.eA;
implicit val eR = p.rs.eA
      isoPairOfCols(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoPairOfCols[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[PairOfColsData[L, R], PairOfCols[L, R]] =
    reifyObject(new PairOfColsIso[L, R]()(eL, eR))

  def mkPairOfCols[L, R]
    (ls: Rep[Coll[L]], rs: Rep[Coll[R]]): Rep[PairOfCols[L, R]] = {
    new PairOfColsCtor[L, R](ls, rs)
  }
  def unmkPairOfCols[L, R](p: Rep[PairColl[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairOfColsElem[L, R] @unchecked =>
      Some((asRep[PairOfCols[L, R]](p).ls, asRep[PairOfCols[L, R]](p).rs))
    case _ =>
      None
  }

    object PairOfColsMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[(L, R)]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[(L, R)]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[(L, R)]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[B]]) forSome {type L; type R; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[B]]) forSome {type L; type R; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[B]]) forSome {type L; type R; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "indices" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Coll[B]]) forSome {type L; type R; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Coll[B]]) forSome {type L; type R; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Coll[B]]) forSome {type L; type R; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object segmentLength {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "segmentLength" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "indexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lastIndexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "lastIndexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object partition {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "partition" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object patch {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Coll[(L, R)]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "patch" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Coll[(L, R)]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Coll[(L, R)]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updated {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "updated" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[Int]], Rep[Coll[(L, R)]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "updateMany" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[Int]], Rep[Coll[(L, R)]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Coll[Int]], Rep[Coll[(L, R)]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => (K, V)], Rep[((V, V)) => V]) forSome {type L; type R; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "mapReduce" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => (K, V)], Rep[((V, V)) => V]) forSome {type L; type R; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => (K, V)], Rep[((V, V)) => V]) forSome {type L; type R; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PairOfColsCompanionMethods {
  }
} // of object PairOfCols
  registerEntityObject("PairOfCols", PairOfCols)

object CReplColl extends EntityObject("CReplColl") {
  case class CReplCollCtor[A]
      (override val value: Rep[A], override val length: Rep[Int])
    extends CReplColl[A](value, length) with Def[CReplColl[A]] {
    implicit lazy val eA = value.elem

    lazy val selfType = element[CReplColl[A]]
    override def transform(t: Transformer) = CReplCollCtor[A](t(value), t(length))
    private val thisClass = classOf[ReplColl[_]]

    override def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, false, element[A]))
    }

    override def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(self,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, false, element[Unit]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    override def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, false, element[Coll[A]]))
    }

    override def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, false, element[Coll[A]]))
    }

    override def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(self,
        thisClass.getMethod("indices"),
        List(),
        true, false, element[Coll[Int]]))
    }

    override def flatMap[B](f: Rep[A => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(self,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, false, element[Coll[B]]))
    }

    override def segmentLength(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def indexWhere(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def lastIndexWhere(p: Rep[A => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, false, element[Int]))
    }

    override def partition(pred: Rep[A => Boolean]): Rep[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(self,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, false, element[(Coll[A], Coll[A])]))
    }

    override def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, false, element[Coll[A]]))
    }

    override def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, false, element[Coll[A]]))
    }

    override def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, false, element[Coll[A]]))
    }

    override def mapReduce[K, V](m: Rep[A => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(self,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, false, element[Coll[(K, V)]]))
    }
  }
  // elem for concrete class
  class CReplCollElem[A](val iso: Iso[CReplCollData[A], CReplColl[A]])(implicit override val eA: Elem[A])
    extends ReplCollElem[A, CReplColl[A]]
    with ConcreteElem[CReplCollData[A], CReplColl[A]] {
    override lazy val parent: Option[Elem[_]] = Some(replCollElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertReplColl(x: Rep[ReplColl[A]]) = RCReplColl(x.value, x.length)
    override def getDefaultRep = RCReplColl(element[A].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CReplColl[A]]
    }
  }

  // state representation type
  type CReplCollData[A] = (A, Int)

  // 3) Iso for concrete class
  class CReplCollIso[A](implicit eA: Elem[A])
    extends EntityIso[CReplCollData[A], CReplColl[A]] with Def[CReplCollIso[A]] {
    override def transform(t: Transformer) = new CReplCollIso[A]()(eA)
    private lazy val _safeFrom = fun { p: Rep[CReplColl[A]] => (p.value, p.length) }
    override def from(p: Rep[CReplColl[A]]) =
      tryConvert[CReplColl[A], (A, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(A, Int)]) = {
      val Pair(value, length) = p
      RCReplColl(value, length)
    }
    lazy val eFrom = pairElement(element[A], element[Int])
    lazy val eTo = new CReplCollElem[A](self)
    lazy val selfType = new CReplCollIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class CReplCollIsoElem[A](eA: Elem[A]) extends Elem[CReplCollIso[A]] {
    def getDefaultRep = reifyObject(new CReplCollIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CReplCollIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CReplCollCompanionCtor extends CompanionDef[CReplCollCompanionCtor] with CReplCollCompanion {
    def selfType = CReplCollCompanionElem
    override def toString = "CReplCollCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[CReplCollData[A]]): Rep[CReplColl[A]] = {
      implicit val eA = p._1.elem
      isoCReplColl[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A](value: Rep[A], length: Rep[Int]): Rep[CReplColl[A]] =
      mkCReplColl(value, length)

    def unapply[A](p: Rep[ReplColl[A]]) = unmkCReplColl(p)
  }
  lazy val CReplCollRep: Rep[CReplCollCompanionCtor] = new CReplCollCompanionCtor
  lazy val RCReplColl: CReplCollCompanionCtor = proxyCReplCollCompanion(CReplCollRep)
  implicit def proxyCReplCollCompanion(p: Rep[CReplCollCompanionCtor]): CReplCollCompanionCtor = {
    if (p.rhs.isInstanceOf[CReplCollCompanionCtor])
      p.rhs.asInstanceOf[CReplCollCompanionCtor]
    else
      proxyOps[CReplCollCompanionCtor](p)
  }

  implicit case object CReplCollCompanionElem extends CompanionElem[CReplCollCompanionCtor] {
    lazy val tag = weakTypeTag[CReplCollCompanionCtor]
    protected def getDefaultRep = CReplCollRep
  }

  implicit def proxyCReplColl[A](p: Rep[CReplColl[A]]): CReplColl[A] =
    proxyOps[CReplColl[A]](p)

  implicit class ExtendedCReplColl[A](p: Rep[CReplColl[A]]) {
    def toData: Rep[CReplCollData[A]] = {
      implicit val eA = p.value.elem
      isoCReplColl(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCReplColl[A](implicit eA: Elem[A]): Iso[CReplCollData[A], CReplColl[A]] =
    reifyObject(new CReplCollIso[A]()(eA))

  def mkCReplColl[A]
    (value: Rep[A], length: Rep[Int]): Rep[CReplColl[A]] = {
    new CReplCollCtor[A](value, length)
  }
  def unmkCReplColl[A](p: Rep[ReplColl[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CReplCollElem[A] @unchecked =>
      Some((asRep[CReplColl[A]](p).value, asRep[CReplColl[A]](p).length))
    case _ =>
      None
  }

    object CReplCollMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CReplColl[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[CReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CReplColl[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Rep[CReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "indices" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CReplColl[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object segmentLength {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "segmentLength" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "indexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lastIndexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "lastIndexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object partition {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "partition" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object patch {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "patch" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updated {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "updated" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "updateMany" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Nullable[(Rep[CReplColl[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplCollElem[_]] && method.getName == "mapReduce" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplColl[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplColl[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CReplCollCompanionMethods {
  }
} // of object CReplColl
  registerEntityObject("CReplColl", CReplColl)

  registerModule(CollsOverArraysModule)
}

object CollsOverArraysModule extends scalan.ModuleInfo("special.collection", "CollsOverArrays")
}

trait CollsOverArraysModule extends special.collection.impl.CollsOverArraysDefs {self: Library =>}
