package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CollsDefs extends scalan.Scalan with Colls {
  self: Library =>
import IsoUR._
import Converter._
import Coll._
import CollBuilder._
import Monoid._
import MonoidBuilder._
import PairColl._
import WArray._
import WOption._
import ReplColl._

object Coll extends EntityObject("Coll") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SColl[A] = special.collection.Coll[A]
  case class CollConst[SA, A](
        constValue: SColl[SA],
        lA: Liftable[SA, A]
      ) extends Coll[A] with LiftedConst[SColl[SA], Coll[A]]
        with Def[Coll[A]] with CollConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[SColl[SA], Coll[A]] = liftableColl(lA)
    val selfType: Elem[Coll[A]] = liftable.eW
  }

  trait CollConstMethods[A] extends Coll[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val CollClass = classOf[Coll[A]]

    override def builder: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(self,
        CollClass.getMethod("builder"),
        List(),
        true, false, element[CollBuilder]))
    }

    override def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(self,
        CollClass.getMethod("arr"),
        List(),
        true, false, element[WArray[A]]))
    }

    override def length: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("length"),
        List(),
        true, false, element[Int]))
    }

    override def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, false, element[A]))
    }

    override def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, false, element[A]))
    }

    override def map[B](f: Rep[A => B]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("map", classOf[Sym]),
        List(f),
        true, false, element[Coll[B]]))
    }

    override def zip[B](ys: Rep[Coll[B]]): Rep[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(self,
        CollClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, false, element[Coll[(A, B)]]))
    }

    override def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(self,
        CollClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, false, element[Unit]))
    }

    override def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    override def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    override def filter(p: Rep[A => Boolean]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, false, element[Coll[A]]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        CollClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    override def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(self,
        CollClass.getMethod("indices"),
        List(),
        true, false, element[Coll[Int]]))
    }

    override def flatMap[B](f: Rep[A => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, false, element[Coll[B]]))
    }

    override def segmentLength(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def find(p: Rep[A => Boolean]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        CollClass.getMethod("find", classOf[Sym]),
        List(p),
        true, false, element[WOption[A]]))
    }

    override def indexWhere(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, false, element[Int]))
    }

    override def indexOf(elem: Rep[A], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        List(elem, from),
        true, false, element[Int]))
    }

    override def lastIndexWhere(p: Rep[A => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, false, element[Int]))
    }

    override def partition(pred: Rep[A => Boolean]): Rep[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(self,
        CollClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, false, element[(Coll[A], Coll[A])]))
    }

    override def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, false, element[Coll[A]]))
    }

    override def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, false, element[Coll[A]]))
    }

    override def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, false, element[Coll[A]]))
    }

    override def mapReduce[K, V](m: Rep[A => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(self,
        CollClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, false, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Rep[A => K]): Rep[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(self,
        CollClass.getMethod("groupBy", classOf[Sym]),
        List(key),
        true, false, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Rep[A => K], proj: Rep[A => V]): Rep[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(self,
        CollClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        List(key, proj),
        true, false, element[Coll[(K, Coll[V])]]))
    }

    override def unionSet(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("unionSet", classOf[Sym]),
        List(that),
        true, false, element[Coll[A]]))
    }

    override def diff(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("diff", classOf[Sym]),
        List(that),
        true, false, element[Coll[A]]))
    }

    override def intersect(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("intersect", classOf[Sym]),
        List(that),
        true, false, element[Coll[A]]))
    }

    override def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, false, element[A]))
    }

    override def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, false, element[Coll[A]]))
    }

    override def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("append", classOf[Sym]),
        List(other),
        true, false, element[Coll[A]]))
    }

    override def reverse: Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("reverse"),
        List(),
        true, false, element[Coll[A]]))
    }
  }

  case class LiftableColl[SA, A](lA: Liftable[SA, A])
    extends Liftable[SColl[SA], Coll[A]] {
    lazy val eW: Elem[Coll[A]] = collElement(lA.eW)
    lazy val sourceType: RType[SColl[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[SColl[SA]]
    }
    def lift(x: SColl[SA]): Rep[Coll[A]] = CollConst(x, lA)
    def unlift(w: Rep[Coll[A]]): SColl[SA] = w match {
      case Def(CollConst(x: SColl[_], _lA))
            if _lA == lA => x.asInstanceOf[SColl[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableColl[SA, A](implicit lA: Liftable[SA,A]): Liftable[SColl[SA], Coll[A]] =
    LiftableColl(lA)

  // entityAdapter for Coll trait
  case class CollAdapter[A](source: Rep[Coll[A]])
      extends Coll[A] with Def[Coll[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val selfType: Elem[Coll[A]] = element[Coll[A]]
    override def transform(t: Transformer) = CollAdapter[A](t(source))
    private val thisClass = classOf[Coll[A]]

    def builder: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CollBuilder]))
    }

    def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[A]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[A]))
    }

    def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[A]))
    }

    def map[B](f: Rep[A => B]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    def zip[B](ys: Rep[Coll[B]]): Rep[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[Coll[(A, B)]]))
    }

    def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Coll[A]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        thisClass.getMethod("indices"),
        List(),
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Rep[A => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def find(p: Rep[A => Boolean]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        thisClass.getMethod("find", classOf[Sym]),
        List(p),
        true, true, element[WOption[A]]))
    }

    def indexWhere(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Rep[A], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        List(elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Rep[A => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, true, element[Int]))
    }

    def partition(pred: Rep[A => Boolean]): Rep[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(source,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, true, element[(Coll[A], Coll[A])]))
    }

    def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, true, element[Coll[A]]))
    }

    def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, true, element[Coll[A]]))
    }

    def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, true, element[Coll[A]]))
    }

    def mapReduce[K, V](m: Rep[A => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, true, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Rep[A => K]): Rep[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(source,
        thisClass.getMethod("groupBy", classOf[Sym]),
        List(key),
        true, true, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Rep[A => K], proj: Rep[A => V]): Rep[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        thisClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        List(key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("unionSet", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    override def diff(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("diff", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    override def intersect(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("intersect", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[A]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Coll[A]]))
    }

    def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Coll[A]]))
    }

    def reverse: Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("reverse"),
        List(),
        true, true, element[Coll[A]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyColl[A](p: Rep[Coll[A]]): Coll[A] = {
    if (p.rhs.isInstanceOf[Coll[A]@unchecked]) p.rhs.asInstanceOf[Coll[A]]
    else
      CollAdapter(p)
  }

  implicit def castCollElement[A](elem: Elem[Coll[A]]): CollElem[A, Coll[A]] =
    elem.asInstanceOf[CollElem[A, Coll[A]]]

  implicit lazy val containerColl: Functor[Coll] = new Functor[Coll] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Coll[A]]
    def lift[A](implicit evA: Elem[A]) = element[Coll[A]]
    def unlift[A](implicit eFT: Elem[Coll[A]]) =
      castCollElement(eFT).eA
    def getElem[A](fa: Rep[Coll[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: CollElem[_,_] => Some(e.asElem[Coll[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[Coll[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class CollIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, Coll] {
    lazy val selfType = new ConcreteIsoElem[Coll[A], Coll[B], CollIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[Coll[A], Coll[B]]]]
    def cC = container[Coll]
    def from(x: Rep[Coll[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[Coll[A]]) = x.map(innerIso.toFun)
    override def transform(t: Transformer) = CollIso(t(innerIso))
  }

  def collIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(CollIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, Coll]]

  // familyElem
  class CollElem[A, To <: Coll[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, Coll](_eA, container[Coll]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = liftableColl(_eA.liftable).asLiftable[SColl[_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Coll[A]], classOf[SColl[_]], Set(
        "builder", "arr", "length", "apply", "getOrElse", "map", "zip", "foreach", "exists", "forall", "filter", "where", "fold", "indices", "flatMap", "segmentLength", "find", "indexWhere", "indexOf", "lastIndexWhere", "partition", "patch", "updated", "updateMany", "mapReduce", "groupBy", "groupByProjecting", "unionSet", "diff", "intersect", "sum", "slice", "append", "reverse"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Coll[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Coll[A]] => convertColl(x) }
      tryConvert(element[Coll[A]], this, x, conv)
    }

    def convertColl(x: Rep[Coll[A]]): Rep[To] = {
      x.elem match {
        case _: CollElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CollElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def collElement[A](implicit eA: Elem[A]): Elem[Coll[A]] =
    cachedElem[CollElem[A, Coll[A]]](eA)

  implicit case object CollCompanionElem extends CompanionElem[CollCompanionCtor] {
    lazy val tag = weakTypeTag[CollCompanionCtor]
    protected def getDefaultRep = RColl
  }

  abstract class CollCompanionCtor extends CompanionDef[CollCompanionCtor] with CollCompanion {
    def selfType = CollCompanionElem
    override def toString = "Coll"
  }
  implicit def proxyCollCompanionCtor(p: Rep[CollCompanionCtor]): CollCompanionCtor =
    proxyOps[CollCompanionCtor](p)

  lazy val RColl: Rep[CollCompanionCtor] = new CollCompanionCtor {
    private val thisClass = classOf[CollCompanion]
  }

  case class ViewColl[A, B](source: Rep[Coll[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, Coll](collIso(innerIso)) {
    override def transform(t: Transformer) = ViewColl(t(source), t(innerIso))
    override def toString = s"ViewColl[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewColl[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object CollMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Coll[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Coll[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Coll[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object where {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "where" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Rep[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "indices" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Coll[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object segmentLength {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "segmentLength" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object find {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "find" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "indexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object indexOf {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "indexOf" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lastIndexWhere {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "lastIndexWhere" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object partition {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "partition" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object patch {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "patch" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Int], Rep[Coll[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updated {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "updated" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object updateMany {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "updateMany" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[Int]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "mapReduce" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => (K, V)], Rep[((V, V)) => V]) forSome {type A; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupBy {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => K]) forSome {type A; type K}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "groupBy" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => K]) forSome {type A; type K}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => K]) forSome {type A; type K}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupByProjecting {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[A => K], Rep[A => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "groupByProjecting" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[A => K], Rep[A => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[A => K], Rep[A => V]) forSome {type A; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object unionSet {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "unionSet" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object diff {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "diff" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intersect {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "intersect" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Coll[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object reverse {
      def unapply(d: Def[_]): Nullable[Rep[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollElem[_, _]] && method.getName == "reverse" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Coll[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CollCompanionMethods {
  }
} // of object Coll
  registerEntityObject("Coll", Coll)

  object UserTypeColl {
    def unapply(s: Sym): Option[Iso[_, _]] = {
      s.elem match {
        case e: CollElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewColl[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeColl(iso: Iso[a, b]) =>
      val newIso = collIso(iso)
      val repr = reifyObject(UnpackView(asRep[Coll[b]](s), newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type RepColl[A] = Rep[Coll[A]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewColl(Def(view2@ViewColl(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewColl(arr, compIso)

    case CollMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepColl[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = asRep[a => c](f)
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewColl(s, iso)
        res
      case (HasViews(source, Def(contIso: CollIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = asRep[b => c](f)
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        asRep[Coll[a]](source).map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }

object PairColl extends EntityObject("PairColl") {
  // entityAdapter for PairColl trait
  case class PairCollAdapter[L, R](source: Rep[PairColl[L, R]])
      extends PairColl[L, R] with Def[PairColl[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[PairColl[L, R]] = element[PairColl[L, R]]
    override def transform(t: Transformer) = PairCollAdapter[L, R](t(source))
    private val thisClass = classOf[PairColl[L, R]]

    def ls: Rep[Coll[L]] = {
      asRep[Coll[L]](mkMethodCall(source,
        thisClass.getMethod("ls"),
        List(),
        true, true, element[Coll[L]]))
    }

    def rs: Rep[Coll[R]] = {
      asRep[Coll[R]](mkMethodCall(source,
        thisClass.getMethod("rs"),
        List(),
        true, true, element[Coll[R]]))
    }

    def builder: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CollBuilder]))
    }

    def arr: Rep[WArray[(L, R)]] = {
      asRep[WArray[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[(L, R)]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def apply(i: Rep[Int]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[(L, R)]))
    }

    def getOrElse(i: Rep[Int], default: Rep[(L, R)]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[(L, R)]))
    }

    def map[B](f: Rep[((L, R)) => B]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    // manual fix
    def zip[B](ys: Rep[Coll[B]]): Rep[Coll[((L, R), B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[((L, R), B)]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[Coll[((L, R), B)]](collElement(pairElement(pairElement(eL, eR), eB)))))
    }

    def foreach(f: Rep[((L, R)) => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[((L, R)) => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[((L, R)) => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[((L, R)) => Boolean]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Coll[(L, R)]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, (L, R))) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        thisClass.getMethod("indices"),
        List(),
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Rep[((L, R)) => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Rep[((L, R)) => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def find(p: Rep[((L, R)) => Boolean]): Rep[WOption[(L, R)]] = {
      asRep[WOption[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("find", classOf[Sym]),
        List(p),
        true, true, element[WOption[(L, R)]]))
    }

    def indexWhere(p: Rep[((L, R)) => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Rep[(L, R)], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        List(elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Rep[((L, R)) => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, true, element[Int]))
    }

    // manual fix
    def partition(pred: Rep[((L, R)) => Boolean]): Rep[(Coll[(L, R)], Coll[(L, R)])] = {
      asRep[(Coll[(L, R)], Coll[(L, R)])](mkMethodCall(source,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, true, element[(Coll[(L, R)], Coll[(L, R)])](pairElement(collElement(pairElement(eL,eR)), collElement(pairElement(eL,eR))))))
    }

    def patch(from: Rep[Int], patch: Rep[Coll[(L, R)]], replaced: Rep[Int]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, true, element[Coll[(L, R)]]))
    }

    def updated(index: Rep[Int], elem: Rep[(L, R)]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, true, element[Coll[(L, R)]]))
    }

    def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, true, element[Coll[(L, R)]]))
    }

    def mapReduce[K, V](m: Rep[((L, R)) => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, true, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Rep[((L, R)) => K]): Rep[Coll[(K, Coll[(L, R)])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[(L, R)])]](mkMethodCall(source,
        thisClass.getMethod("groupBy", classOf[Sym]),
        List(key),
        true, true, element[Coll[(K, Coll[(L, R)])]](collElement(pairElement(eK, collElement(pairElement(eL, eR)))))))
    }

    override def groupByProjecting[K, V](key: Rep[((L, R)) => K], proj: Rep[((L, R)) => V]): Rep[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        thisClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        List(key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("unionSet", classOf[Sym]),
        List(that),
        true, true, element[Coll[(L, R)]]))
    }

    override def diff(that: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("diff", classOf[Sym]),
        List(that),
        true, true, element[Coll[(L, R)]]))
    }

    override def intersect(that: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("intersect", classOf[Sym]),
        List(that),
        true, true, element[Coll[(L, R)]]))
    }

    def sum(m: Rep[Monoid[(L, R)]]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[(L, R)]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Coll[(L, R)]]))
    }

    def append(other: Rep[Coll[(L, R)]]): Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Coll[(L, R)]]))
    }

    def reverse: Rep[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("reverse"),
        List(),
        true, true, element[Coll[(L, R)]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyPairColl[L, R](p: Rep[PairColl[L, R]]): PairColl[L, R] = {
    if (p.rhs.isInstanceOf[PairColl[L, R]@unchecked]) p.rhs.asInstanceOf[PairColl[L, R]]
    else
      PairCollAdapter(p)
  }

  // familyElem
  class PairCollElem[L, R, To <: PairColl[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CollElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(collElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairColl[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[PairColl[L, R]] => convertPairColl(x) }
      tryConvert(element[PairColl[L, R]], this, x, conv)
    }

    def convertPairColl(x: Rep[PairColl[L, R]]): Rep[To] = {
      x.elem match {
        case _: PairCollElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have PairCollElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def pairCollElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[PairColl[L, R]] =
    cachedElem[PairCollElem[L, R, PairColl[L, R]]](eL, eR)

  implicit case object PairCollCompanionElem extends CompanionElem[PairCollCompanionCtor] {
    lazy val tag = weakTypeTag[PairCollCompanionCtor]
    protected def getDefaultRep = RPairColl
  }

  abstract class PairCollCompanionCtor extends CompanionDef[PairCollCompanionCtor] with PairCollCompanion {
    def selfType = PairCollCompanionElem
    override def toString = "PairColl"
  }
  implicit def proxyPairCollCompanionCtor(p: Rep[PairCollCompanionCtor]): PairCollCompanionCtor =
    proxyOps[PairCollCompanionCtor](p)

  lazy val RPairColl: Rep[PairCollCompanionCtor] = new PairCollCompanionCtor {
    private val thisClass = classOf[PairCollCompanion]
  }

  object PairCollMethods {
    object ls {
      def unapply(d: Def[_]): Nullable[Rep[PairColl[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollElem[_, _, _]] && method.getName == "ls" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairColl[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairColl[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object rs {
      def unapply(d: Def[_]): Nullable[Rep[PairColl[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairCollElem[_, _, _]] && method.getName == "rs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairColl[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairColl[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PairCollCompanionMethods {
  }
} // of object PairColl
  registerEntityObject("PairColl", PairColl)

object ReplColl extends EntityObject("ReplColl") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SReplColl[A] = special.collection.ReplColl[A]
  case class ReplCollConst[SA, A](
        constValue: SReplColl[SA],
        lA: Liftable[SA, A]
      ) extends ReplColl[A] with LiftedConst[SReplColl[SA], ReplColl[A]]
        with Def[ReplColl[A]] with ReplCollConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[SReplColl[SA], ReplColl[A]] = liftableReplColl(lA)
    val selfType: Elem[ReplColl[A]] = liftable.eW
  }

  trait ReplCollConstMethods[A] extends ReplColl[A] with CollConstMethods[A] { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val ReplCollClass = classOf[ReplColl[A]]

    override def value: Rep[A] = {
      asRep[A](mkMethodCall(self,
        ReplCollClass.getMethod("value"),
        List(),
        true, false, element[A]))
    }

    override def length: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        ReplCollClass.getMethod("length"),
        List(),
        true, false, element[Int]))
    }

    override def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        ReplCollClass.getMethod("append", classOf[Sym]),
        List(other),
        true, false, element[Coll[A]]))
    }
  }

  case class LiftableReplColl[SA, A](lA: Liftable[SA, A])
    extends Liftable[SReplColl[SA], ReplColl[A]] {
    lazy val eW: Elem[ReplColl[A]] = replCollElement(lA.eW)
    lazy val sourceType: RType[SReplColl[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[SReplColl[SA]]
    }
    def lift(x: SReplColl[SA]): Rep[ReplColl[A]] = ReplCollConst(x, lA)
    def unlift(w: Rep[ReplColl[A]]): SReplColl[SA] = w match {
      case Def(ReplCollConst(x: SReplColl[_], _lA))
            if _lA == lA => x.asInstanceOf[SReplColl[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableReplColl[SA, A](implicit lA: Liftable[SA,A]): Liftable[SReplColl[SA], ReplColl[A]] =
    LiftableReplColl(lA)

  // entityAdapter for ReplColl trait
  case class ReplCollAdapter[A](source: Rep[ReplColl[A]])
      extends ReplColl[A] with Def[ReplColl[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val selfType: Elem[ReplColl[A]] = element[ReplColl[A]]
    override def transform(t: Transformer) = ReplCollAdapter[A](t(source))
    private val thisClass = classOf[ReplColl[A]]

    def value: Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[A]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def append(other: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Coll[A]]))
    }

    def builder: Rep[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CollBuilder]))
    }

    def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[A]]))
    }

    def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[A]))
    }

    def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[A]))
    }

    def map[B](f: Rep[A => B]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    def zip[B](ys: Rep[Coll[B]]): Rep[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[Coll[(A, B)]]))
    }

    def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Coll[A]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def indices: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        thisClass.getMethod("indices"),
        List(),
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Rep[A => Coll[B]]): Rep[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asElem[B]
      asRep[Coll[B]](mkMethodCall(source,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def find(p: Rep[A => Boolean]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        thisClass.getMethod("find", classOf[Sym]),
        List(p),
        true, true, element[WOption[A]]))
    }

    def indexWhere(p: Rep[A => Boolean], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        List(p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Rep[A], from: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        List(elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Rep[A => Boolean], end: Rep[Int]): Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        List(p, end),
        true, true, element[Int]))
    }

    def partition(pred: Rep[A => Boolean]): Rep[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(source,
        thisClass.getMethod("partition", classOf[Sym]),
        List(pred),
        true, true, element[(Coll[A], Coll[A])]))
    }

    def patch(from: Rep[Int], patch: Rep[Coll[A]], replaced: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(from, patch, replaced),
        true, true, element[Coll[A]]))
    }

    def updated(index: Rep[Int], elem: Rep[A]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        List(index, elem),
        true, true, element[Coll[A]]))
    }

    def updateMany(indexes: Rep[Coll[Int]], values: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        List(indexes, values),
        true, true, element[Coll[A]]))
    }

    def mapReduce[K, V](m: Rep[A => (K, V)], r: Rep[((V, V)) => V]): Rep[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        thisClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        List(m, r),
        true, true, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Rep[A => K]): Rep[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(source,
        thisClass.getMethod("groupBy", classOf[Sym]),
        List(key),
        true, true, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Rep[A => K], proj: Rep[A => V]): Rep[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        thisClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        List(key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("unionSet", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    override def diff(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("diff", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    override def intersect(that: Rep[Coll[A]]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("intersect", classOf[Sym]),
        List(that),
        true, true, element[Coll[A]]))
    }

    def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[A]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Coll[A]]))
    }

    def reverse: Rep[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("reverse"),
        List(),
        true, true, element[Coll[A]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyReplColl[A](p: Rep[ReplColl[A]]): ReplColl[A] = {
    if (p.rhs.isInstanceOf[ReplColl[A]@unchecked]) p.rhs.asInstanceOf[ReplColl[A]]
    else
      ReplCollAdapter(p)
  }

  // familyElem
  class ReplCollElem[A, To <: ReplColl[A]](implicit _eA: Elem[A])
    extends CollElem[A, To] {
    override def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = liftableReplColl(_eA.liftable).asLiftable[SReplColl[_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[ReplColl[A]], classOf[SReplColl[_]], Set(
        "value", "length", "append"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(collElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ReplColl[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ReplColl[A]] => convertReplColl(x) }
      tryConvert(element[ReplColl[A]], this, x, conv)
    }

    def convertReplColl(x: Rep[ReplColl[A]]): Rep[To] = {
      x.elem match {
        case _: ReplCollElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have ReplCollElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def replCollElement[A](implicit eA: Elem[A]): Elem[ReplColl[A]] =
    cachedElem[ReplCollElem[A, ReplColl[A]]](eA)

  implicit case object ReplCollCompanionElem extends CompanionElem[ReplCollCompanionCtor] {
    lazy val tag = weakTypeTag[ReplCollCompanionCtor]
    protected def getDefaultRep = RReplColl
  }

  abstract class ReplCollCompanionCtor extends CompanionDef[ReplCollCompanionCtor] with ReplCollCompanion {
    def selfType = ReplCollCompanionElem
    override def toString = "ReplColl"
  }
  implicit def proxyReplCollCompanionCtor(p: Rep[ReplCollCompanionCtor]): ReplCollCompanionCtor =
    proxyOps[ReplCollCompanionCtor](p)

  lazy val RReplColl: Rep[ReplCollCompanionCtor] = new ReplCollCompanionCtor {
    private val thisClass = classOf[ReplCollCompanion]
  }

  object ReplCollMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[ReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReplCollElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ReplColl[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[ReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReplCollElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ReplColl[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[ReplColl[A]], Rep[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ReplCollElem[_, _]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ReplColl[A]], Rep[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ReplColl[A]], Rep[Coll[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ReplCollCompanionMethods {
  }
} // of object ReplColl
  registerEntityObject("ReplColl", ReplColl)

object CollBuilder extends EntityObject("CollBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SCollBuilder = special.collection.CollBuilder
  case class CollBuilderConst(
        constValue: SCollBuilder
      ) extends CollBuilder with LiftedConst[SCollBuilder, CollBuilder]
        with Def[CollBuilder] with CollBuilderConstMethods {
    val liftable: Liftable[SCollBuilder, CollBuilder] = LiftableCollBuilder
    val selfType: Elem[CollBuilder] = liftable.eW
  }

  trait CollBuilderConstMethods extends CollBuilder  { thisConst: Def[_] =>

    private val CollBuilderClass = classOf[CollBuilder]

    override def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(self,
        CollBuilderClass.getMethod("Monoids"),
        List(),
        true, false, element[MonoidBuilder]))
    }

    override def pairColl[A, B](as: Rep[Coll[A]], bs: Rep[Coll[B]]): Rep[PairColl[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairColl[A, B]](mkMethodCall(self,
        CollBuilderClass.getMethod("pairColl", classOf[Sym], classOf[Sym]),
        List(as, bs),
        true, false, element[PairColl[A, B]]))
    }

    override def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, false, element[Coll[T]]))
    }

    override def unzip[A, B](xs: Rep[Coll[(A, B)]]): Rep[(Coll[A], Coll[B])] = {
      implicit val eA = xs.eA.eFst
implicit val eB = xs.eA.eSnd
      asRep[(Coll[A], Coll[B])](mkMethodCall(self,
        CollBuilderClass.getMethod("unzip", classOf[Sym]),
        List(xs),
        true, false, element[(Coll[A], Coll[B])]))
    }

    override def xor(left: Rep[Coll[Byte]], right: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        CollBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, false, element[Coll[Byte]]))
    }

    override def fromArray[T](arr: Rep[WArray[T]]): Rep[Coll[T]] = {
      implicit val eT = arr.eT
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, false, element[Coll[T]]))
    }

    override def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, false, element[Coll[T]]))
    }

    override def emptyColl[T](implicit tT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("emptyColl", classOf[Elem[_]]),
        List(tT),
        true, false, element[Coll[T]]))
    }

    override def outerJoin[K, L, R, O](left: Rep[Coll[(K, L)]], right: Rep[Coll[(K, R)]])(l: Rep[((K, L)) => O], r: Rep[((K, R)) => O], inner: Rep[((K, (L, R))) => O]): Rep[Coll[(K, O)]] = {
      implicit val eK = left.eA.eFst
implicit val eL = left.eA.eSnd
implicit val eR = right.eA.eSnd
implicit val eO = l.elem.eRange
      asRep[Coll[(K, O)]](mkMethodCall(self,
        CollBuilderClass.getMethod("outerJoin", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(left, right, l, r, inner),
        true, false, element[Coll[(K, O)]]))
    }

    override def flattenColl[A](coll: Rep[Coll[Coll[A]]]): Rep[Coll[A]] = {
      implicit val eA = coll.eA.typeArgs("A")._1.asElem[A]
      asRep[Coll[A]](mkMethodCall(self,
        CollBuilderClass.getMethod("flattenColl", classOf[Sym]),
        List(coll),
        true, false, element[Coll[A]]))
    }
  }

  implicit object LiftableCollBuilder
    extends Liftable[SCollBuilder, CollBuilder] {
    lazy val eW: Elem[CollBuilder] = collBuilderElement
    lazy val sourceType: RType[SCollBuilder] = {
      RType[SCollBuilder]
    }
    def lift(x: SCollBuilder): Rep[CollBuilder] = CollBuilderConst(x)
    def unlift(w: Rep[CollBuilder]): SCollBuilder = w match {
      case Def(CollBuilderConst(x: SCollBuilder))
            => x.asInstanceOf[SCollBuilder]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for CollBuilder trait
  case class CollBuilderAdapter(source: Rep[CollBuilder])
      extends CollBuilder with Def[CollBuilder] {
    val selfType: Elem[CollBuilder] = element[CollBuilder]
    override def transform(t: Transformer) = CollBuilderAdapter(t(source))
    private val thisClass = classOf[CollBuilder]

    def Monoids: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        thisClass.getMethod("Monoids"),
        List(),
        true, true, element[MonoidBuilder]))
    }

    def pairColl[A, B](as: Rep[Coll[A]], bs: Rep[Coll[B]]): Rep[PairColl[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairColl[A, B]](mkMethodCall(source,
        thisClass.getMethod("pairColl", classOf[Sym], classOf[Sym]),
        List(as, bs),
        true, true, element[PairColl[A, B]]))
    }

    def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, true, element[Coll[T]]))
    }

    override def unzip[A, B](xs: Rep[Coll[(A, B)]]): Rep[(Coll[A], Coll[B])] = {
      implicit val eA = xs.eA.eFst
implicit val eB = xs.eA.eSnd
      asRep[(Coll[A], Coll[B])](mkMethodCall(source,
        thisClass.getMethod("unzip", classOf[Sym]),
        List(xs),
        true, true, element[(Coll[A], Coll[B])]))
    }

    def xor(left: Rep[Coll[Byte]], right: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, true, element[Coll[Byte]]))
    }

    def fromArray[T](arr: Rep[WArray[T]]): Rep[Coll[T]] = {
      implicit val eT = arr.eT
      asRep[Coll[T]](mkMethodCall(source,
        thisClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, true, element[Coll[T]]))
    }

    def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(source,
        thisClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, true, element[Coll[T]]))
    }

    def emptyColl[T](implicit tT: Elem[T]): Rep[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        thisClass.getMethod("emptyColl", classOf[Elem[_]]),
        List(tT),
        true, true, element[Coll[T]]))
    }

    def outerJoin[K, L, R, O](left: Rep[Coll[(K, L)]], right: Rep[Coll[(K, R)]])(l: Rep[((K, L)) => O], r: Rep[((K, R)) => O], inner: Rep[((K, (L, R))) => O]): Rep[Coll[(K, O)]] = {
      implicit val eK = left.eA.eFst
implicit val eL = left.eA.eSnd
implicit val eR = right.eA.eSnd
implicit val eO = l.elem.eRange
      asRep[Coll[(K, O)]](mkMethodCall(source,
        thisClass.getMethod("outerJoin", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(left, right, l, r, inner),
        true, true, element[Coll[(K, O)]]))
    }

    def flattenColl[A](coll: Rep[Coll[Coll[A]]]): Rep[Coll[A]] = {
      implicit val eA = coll.eA.typeArgs("A")._1.asElem[A]
      asRep[Coll[A]](mkMethodCall(source,
        thisClass.getMethod("flattenColl", classOf[Sym]),
        List(coll),
        true, true, element[Coll[A]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCollBuilder(p: Rep[CollBuilder]): CollBuilder = {
    if (p.rhs.isInstanceOf[CollBuilder@unchecked]) p.rhs.asInstanceOf[CollBuilder]
    else
      CollBuilderAdapter(p)
  }

  // familyElem
  class CollBuilderElem[To <: CollBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = LiftableCollBuilder.asLiftable[SCollBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[CollBuilder], classOf[SCollBuilder], Set(
        "Monoids", "pairColl", "fromItems", "unzip", "xor", "fromArray", "replicate", "emptyColl", "outerJoin", "flattenColl"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CollBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CollBuilder] => convertCollBuilder(x) }
      tryConvert(element[CollBuilder], this, x, conv)
    }

    def convertCollBuilder(x: Rep[CollBuilder]): Rep[To] = {
      x.elem match {
        case _: CollBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CollBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val collBuilderElement: Elem[CollBuilder] =
    new CollBuilderElem[CollBuilder]

  implicit case object CollBuilderCompanionElem extends CompanionElem[CollBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CollBuilderCompanionCtor]
    protected def getDefaultRep = RCollBuilder
  }

  abstract class CollBuilderCompanionCtor extends CompanionDef[CollBuilderCompanionCtor] with CollBuilderCompanion {
    def selfType = CollBuilderCompanionElem
    override def toString = "CollBuilder"
  }
  implicit def proxyCollBuilderCompanionCtor(p: Rep[CollBuilderCompanionCtor]): CollBuilderCompanionCtor =
    proxyOps[CollBuilderCompanionCtor](p)

  lazy val RCollBuilder: Rep[CollBuilderCompanionCtor] = new CollBuilderCompanionCtor {
    private val thisClass = classOf[CollBuilderCompanion]
  }

  object CollBuilderMethods {
    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[CollBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "Monoids" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CollBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CollBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object pairColl {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "pairColl" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Coll[A]], Rep[Coll[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object unzip {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Coll[(A, B)]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "unzip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Coll[(A, B)]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Coll[(A, B)]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[WArray[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "fromArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[WArray[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[WArray[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "replicate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Int], Rep[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object emptyColl {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "emptyColl" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object outerJoin {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Coll[(K, L)]], Rep[Coll[(K, R)]], Rep[((K, L)) => O], Rep[((K, R)) => O], Rep[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "outerJoin" =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Coll[(K, L)]], Rep[Coll[(K, R)]], Rep[((K, L)) => O], Rep[((K, R)) => O], Rep[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Coll[(K, L)]], Rep[Coll[(K, R)]], Rep[((K, L)) => O], Rep[((K, R)) => O], Rep[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flattenColl {
      def unapply(d: Def[_]): Nullable[(Rep[CollBuilder], Rep[Coll[Coll[A]]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CollBuilderElem[_]] && method.getName == "flattenColl" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CollBuilder], Rep[Coll[Coll[A]]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CollBuilder], Rep[Coll[Coll[A]]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CollBuilderCompanionMethods {
  }
} // of object CollBuilder
  registerEntityObject("CollBuilder", CollBuilder)

  registerModule(CollsModule)
}

object CollsModule extends scalan.ModuleInfo("special.collection", "Colls")
}

trait CollsModule extends special.collection.impl.CollsDefs {self: Library =>}
