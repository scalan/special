package scalan

import java.lang.reflect.Method

import scalan.primitives.TypeSum

import scala.language.higherKinds

trait Views extends TypeDescs with MethodCalls with TypeSum { self: ViewsModule with Scalan =>
  import IsoUR._

  type Iso[From, To] = Ref[IsoUR[From, To]]

  // TODO try to find a way to generate eTo such that equals and hashCode can refer to it (see commented code)
  trait IsoUR[From, To] extends Def[IsoUR[From, To]] {
    def eFrom: Elem[From]
    def eTo: Elem[To]
    def from(p: Ref[To]): Ref[From]
    def to(p: Ref[From]): Ref[To]
    override def toString = s"${eFrom.name} <-> ${eTo.name}"
    override def equals(other: Any): Boolean =
      !!!(s"Iso.equals must be overridden in $getClass. Make sure the outer reference is ignored when overriding.")
    override lazy val hashCode = 41 * eFrom.hashCode // + eTo.hashCode
    def isIdentity: Boolean = false
    lazy val fromFun = fun { x: Ref[To] => from(x) }(Lazy(eTo))
    lazy val toFun = fun { x: Ref[From] => to(x) }(Lazy(eFrom))
  }

  @Isospec
  abstract class IdentityIso[A](implicit val eA: Elem[A]) extends IsoUR[A, A] {
    def eFrom: Elem[A] = eA
    def eTo: Elem[A] = eA
    def from(x: Ref[A]) = x
    def to(x: Ref[A]) = x
    override def isIdentity = true
    override def equals(other: Any) = other match {
      case i: Views#IdentityIso[_] => (this eq i) || (eFrom == i.eFrom)
      case _ => false
    }
  }
  implicit class IsoOps[A,B](iso: Iso[A,B]) {
    def >>[C](iso2: Iso[B,C]): Iso[A,C] = composeIso(iso2, iso)
  }
  implicit class AnyIsoOps(iso: Iso[_, _]) {
    def asIso[C,D] = iso.asInstanceOf[Iso[C,D]]
  }

  // TODO we can get eA1 etc. from iso1 and iso2, but this won't work as default arguments
  // because this creates a compiler-generated companion object and conflicts with `def PairIsoUR`
  // in ViewsImpl.scala
  @Isospec
  abstract class PairIso[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])
    extends IsoUR[(A1, A2), (B1, B2)] {
    implicit def eA1: Elem[A1]; implicit def eA2: Elem[A2]; implicit def eB1: Elem[B1]; implicit def eB2: Elem[B2]

    // null is used since the only reason this exists is performance
    // TODO consider removing completely
    var fromCacheKey: Ref[(B1,B2)] = null.asInstanceOf[Ref[(B1,B2)]]
    var fromCacheValue: Ref[(A1,A2)] = null.asInstanceOf[Ref[(A1,A2)]]
    var toCacheKey: Ref[(A1,A2)] = null.asInstanceOf[Ref[(A1,A2)]]
    var toCacheValue: Ref[(B1,B2)] = null.asInstanceOf[Ref[(B1,B2)]]

    def from(b: Ref[(B1, B2)]) = {
      if (cachePairs) {
        // b is not null, so the condition includes fromCacheKey == null
        if (b != fromCacheKey) {
          fromCacheKey = b
          fromCacheValue = Pair(iso1.from(b._1), iso2.from(b._2))
        }
        fromCacheValue
      } else
        Pair(iso1.from(b._1), iso2.from(b._2))
    }

    def to(a: Ref[(A1, A2)]) = {
      if (cachePairs) {
        // a is not null, so the condition includes toCacheKey == null
        if (a != toCacheKey) {
          toCacheKey = a
          toCacheValue = Pair(iso1.to(a._1), iso2.to(a._2))
        }
        toCacheValue
      } else
        Pair(iso1.to(a._1), iso2.to(a._2))
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#PairIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }
  trait PairIsoCompanion

  @Isospec
  abstract class AbsorbFirstUnitIso[A2,B2](val iso2: Iso[A2, B2]) extends IsoUR[A2, (Unit, B2)] {
    implicit def eA2: Elem[A2]; implicit def eB2: Elem[B2]
    def from(b: Ref[(Unit, B2)]) = {
      iso2.from(b._2)
    }
    def to(a: Ref[A2]) = {
      Pair((), iso2.to(a))
    }
    override def isIdentity = false
    override def equals(other: Any) = other match {
      case i: Views#AbsorbFirstUnitIso[_, _] => (this eq i) || (iso2 == i.iso2)
      case _ => false
    }
  }

  @Isospec
  abstract class AbsorbSecondUnitIso[A1,B1](val iso1: Iso[A1, B1]) extends IsoUR[A1, (B1,Unit)] {
    implicit def eA1: Elem[A1]; implicit def eB1: Elem[B1]
    def from(b: Ref[(B1,Unit)]) = {
      iso1.from(b._1)
    }
    def to(a: Ref[A1]) = {
      Pair(iso1.to(a), ())
    }
    override def isIdentity = false
    override def equals(other: Any) = other match {
      case i: Views#AbsorbSecondUnitIso[_, _] => (this eq i) || (iso1 == i.iso1)
      case _ => false
    }
  }

  @Isospec
  abstract class SumIso[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])
    extends IsoUR[A1 | A2, B1 | B2] {
    implicit def eA1: Elem[A1]; implicit def eA2: Elem[A2]; implicit def eB1: Elem[B1]; implicit def eB2: Elem[B2]
    def from(b: Ref[B1 | B2]) =
      b.mapSumBy(iso1.fromFun, iso2.fromFun)
    def to(a: Ref[A1 | A2]) =
      a.mapSumBy(iso1.toFun, iso2.toFun)
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#SumIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  @Isospec
  abstract class ComposeIso[A, B, C](val iso2: Iso[B, C], val iso1: Iso[A, B])
    extends IsoUR[A, C] {
    def eFrom: Elem[A] = iso1.eFrom
    def eTo: Elem[C] = iso2.eTo
    def from(c: Ref[C]) = iso1.from(iso2.from(c))
    def to(a: Ref[A]) = iso2.to(iso1.to(a))
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#ComposeIso[_, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  @Isospec
  abstract class FuncIso[A, B, C, D](val iso1: Iso[A, B], val iso2: Iso[C, D])
    extends IsoUR[A => C, B => D] {
    implicit def eA: Elem[A]; implicit def eB: Elem[B]; implicit def eC: Elem[C]; implicit def eD: Elem[D]
    def from(f: Ref[B => D]): Ref[A => C] = {
      fun { b => iso2.from(f(iso1.to(b))) }
    }
    def to(f: Ref[A => C]): Ref[B => D] = {
      fun { a => iso2.to(f(iso1.from(a))) }
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#FuncIso[_, _, _, _] => (this eq i) || (iso1 == i.iso1 && iso2 == i.iso2)
      case _ => false
    }
  }

  type Iso1[A, B, C[_]] = Ref[Iso1UR[A, B, C]]

  trait Iso1UR[A, B, C[_]]
    extends IsoUR[C[A], C[B]] {
    def innerIso: Iso[A, B]
    implicit def cC: Cont[C]
    implicit def eA: Elem[A] = innerIso.eFrom
    implicit def eB: Elem[B] = innerIso.eTo
    lazy val eFrom: Elem[C[A]] = cC.lift(innerIso.eFrom)
    lazy val eTo: Elem[C[B]] = cC.lift(innerIso.eTo)
    override def isIdentity = innerIso.isIdentity
    override def equals(other: Any) = other match {
      case i: Views#Iso1UR[_, _, _] => (this eq i) || (cC == i.cC && eA == i.eA && eB == i.eB)
      case _ => false
    }
  }

  @Isospec
  abstract class ThunkIso[A,B](val innerIso: Iso[A,B]) extends Iso1UR[A, B, Thunk] {
    def cC = container[Thunk]
    def from(x: Th[B]) = x.map(innerIso.fromFun)
    def to(x: Th[A]) = x.map(innerIso.toFun)
  }
}

trait ViewsModule extends impl.ViewsDefs { self: Scalan =>
  import IsoUR._
  import Iso1UR._
  import IdentityIso._
  import PairIso._
  import AbsorbFirstUnitIso._
  import AbsorbSecondUnitIso._
  import SumIso._
  import ComposeIso._
  import FuncIso._
  import ThunkIso._
  /**
    * The base type of all isos for user-defined types
    */
  trait EntityIso[From, To] extends IsoUR[From, To] with Product {
    override def canEqual(other: Any) = getClass == other.getClass
    override def equals(other: Any) = other match {
      case i: ViewsModule#EntityIso[_, _] =>
        // Comparing productArity is unnecessary since it should be equal when the classes are equal and
        // in case it isn't, we do little extra work
        (this eq i) || (getClass == i.getClass && productIterator.sameElements(i.productIterator))
      case _ => false
    }
  }

  implicit def viewElement[From, To](implicit iso: Iso[From, To]): Elem[To] = iso.eTo // always ask elem from IsoUR

  trait ViewElem[From, To] extends Elem[To] { _: scala.Equals =>
    def iso: Iso[From, To]

  }

  object ViewElem {
    def unapply[From, To](ve: ViewElem[From, To]): Option[IsoUR[From, To]] = Some(ve.iso)
  }

  trait ViewElem1[A,From,To,C[_]] extends ViewElem[From, To] { _: scala.Equals =>
    def eItem: Elem[A]
    def cont: Cont[C]
  }

  class ConcreteIsoElem[From, To, IsoType <: IsoUR[From, To]](_eFrom: => Elem[From], _eTo: => Elem[To]) extends IsoURElem[From, To, IsoType]()(_eFrom, _eTo)


  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] = RIdentityIso[A]()(elem)

  def pairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[(A1, A2), (B1, B2)] =
    RPairIso[A1, A2, B1, B2](iso1, iso2)

  def sumIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[A1 | A2, B1 | B2] =
    RSumIso[A1, A2, B1, B2](iso1, iso2)

  def composeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Iso[A, C] = {
    if (iso2.isIdentity)
      iso1
    else if (iso1.isIdentity)
      iso2
    else
      (iso2.node, iso1.node) match {
        case (iso2d: PairIso[b1, b2, c1, c2], iso1d: PairIso[a1, a2, _, _]) =>
          val composedIso1 = composeIso(iso2d.iso1, iso1d.iso1.asInstanceOf[Iso[a1, b1]])
          val composedIso2 = composeIso(iso2d.iso2, iso1d.iso2.asInstanceOf[Iso[a2, b2]])
          pairIso(composedIso1, composedIso2)
        case _ => RComposeIso[A, B, C](iso2, iso1)
      }
  }.asInstanceOf[Iso[A, C]]


  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[A => C, B => D] =
    RFuncIso[A, B, C, D](iso1, iso2)

  def thunkIso[A,B](iso: Iso[A, B]) = RThunkIso[A, B](iso).asInstanceOf[Iso1[A, B, Thunk]]

  type Unpacked[T] = (Ref[Source], Iso[Source, T]) forSome { type Source }
  type UnpackedLambdaResult[T,R] = (Ref[T => R], Iso[Source, R]) forSome { type Source }

  type UnpackTester = Elem[_] => Boolean

  protected val initialUnpackTesters: Set[UnpackTester] = Set.empty
  protected var unpackTesters: Set[UnpackTester] = initialUnpackTesters

  def addUnpackTester(tester: UnpackTester): Unit =
    unpackTesters += tester
  def removeUnpackTester(tester: UnpackTester): Unit =
    unpackTesters -= tester

  def shouldUnpack(e: Elem[_]) = unpackTesters.exists(_(e))

  def defaultUnpackTester(e: Elem[_]) = true //e match { case pe: PairElem[_,_] => false case _ => true }

  abstract class View[From, To] extends Def[To] {
    def source: Ref[From]
    def iso: Iso[From, To]
    implicit lazy val resultType = iso.eTo
  }

  case class UnpackView[A, B](view: Ref[B], iso: Iso[A, B]) extends Def[A] {
    implicit def resultType = iso.eFrom
    override def transform(t: Transformer) = UnpackView(t(view), t(iso))
  }

  abstract class View1[A, B, C[_]](val iso: Iso1[A,B,C]) extends View[C[A], C[B]] {
    def innerIso = iso.innerIso
  }

  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]

  case class PairView[A1, A2, B1, B2](source: Ref[(A1, A2)], override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])
      extends View2[A1, A2, B1, B2, Tuple2]()(iso1, iso2) {
    lazy val iso = pairIso(iso1, iso2)
    override def transform(t: Transformer) = PairView(t(source), t(iso1), t(iso2))
  }

  case class SumView[A1, A2, B1, B2](source: Ref[A1|A2])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, | ] {
    lazy val iso = sumIso(iso1, iso2)
    override def transform(t: Transformer) = SumView(t(source))(t(iso1), t(iso2))
  }

}

