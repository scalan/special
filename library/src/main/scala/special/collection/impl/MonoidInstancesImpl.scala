package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait MonoidInstancesDefs extends scalan.Scalan with MonoidInstances {
  self: Library =>
import IsoUR._
import Converter._
import IntPlusMonoid._
import LongPlusMonoid._
import Monoid._
import MonoidBuilder._
import PairMonoid._
import MonoidBuilderInst._

object MonoidBuilderInst extends EntityObject("MonoidBuilderInst") {
  case class MonoidBuilderInstCtor
      ()
    extends MonoidBuilderInst() with Def[MonoidBuilderInst] {
    lazy val selfType = element[MonoidBuilderInst]
    override def transform(t: Transformer) = MonoidBuilderInstCtor()
  }
  // elem for concrete class
  class MonoidBuilderInstElem(val iso: Iso[MonoidBuilderInstData, MonoidBuilderInst])
    extends MonoidBuilderElem[MonoidBuilderInst]
    with ConcreteElem[MonoidBuilderInstData, MonoidBuilderInst] {
    override lazy val parent: Option[Elem[_]] = Some(monoidBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertMonoidBuilder(x: Rep[MonoidBuilder]) = RMonoidBuilderInst()
    override def getDefaultRep = RMonoidBuilderInst()
    override lazy val tag = {
      weakTypeTag[MonoidBuilderInst]
    }
  }

  // state representation type
  type MonoidBuilderInstData = Unit

  // 3) Iso for concrete class
  class MonoidBuilderInstIso
    extends EntityIso[MonoidBuilderInstData, MonoidBuilderInst] with Def[MonoidBuilderInstIso] {
    override def transform(t: Transformer) = new MonoidBuilderInstIso()
    private lazy val _safeFrom = fun { p: Rep[MonoidBuilderInst] => () }
    override def from(p: Rep[MonoidBuilderInst]) =
      tryConvert[MonoidBuilderInst, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RMonoidBuilderInst()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new MonoidBuilderInstElem(self)
    lazy val selfType = new MonoidBuilderInstIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class MonoidBuilderInstIsoElem() extends Elem[MonoidBuilderInstIso] {
    def getDefaultRep = reifyObject(new MonoidBuilderInstIso())
    lazy val tag = {
      weakTypeTag[MonoidBuilderInstIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class MonoidBuilderInstCompanionCtor extends CompanionDef[MonoidBuilderInstCompanionCtor] with MonoidBuilderInstCompanion {
    def selfType = MonoidBuilderInstCompanionElem
    override def toString = "MonoidBuilderInstCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[MonoidBuilderInstData]): Rep[MonoidBuilderInst] = {
      isoMonoidBuilderInst.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[MonoidBuilderInst] =
      mkMonoidBuilderInst()

    def unapply(p: Rep[MonoidBuilder]) = unmkMonoidBuilderInst(p)
  }
  lazy val MonoidBuilderInstRep: Rep[MonoidBuilderInstCompanionCtor] = new MonoidBuilderInstCompanionCtor
  lazy val RMonoidBuilderInst: MonoidBuilderInstCompanionCtor = proxyMonoidBuilderInstCompanion(MonoidBuilderInstRep)
  implicit def proxyMonoidBuilderInstCompanion(p: Rep[MonoidBuilderInstCompanionCtor]): MonoidBuilderInstCompanionCtor = {
    if (p.rhs.isInstanceOf[MonoidBuilderInstCompanionCtor])
      p.rhs.asInstanceOf[MonoidBuilderInstCompanionCtor]
    else
      proxyOps[MonoidBuilderInstCompanionCtor](p)
  }

  implicit case object MonoidBuilderInstCompanionElem extends CompanionElem[MonoidBuilderInstCompanionCtor] {
    lazy val tag = weakTypeTag[MonoidBuilderInstCompanionCtor]
    protected def getDefaultRep = MonoidBuilderInstRep
  }

  implicit def proxyMonoidBuilderInst(p: Rep[MonoidBuilderInst]): MonoidBuilderInst =
    proxyOps[MonoidBuilderInst](p)

  implicit class ExtendedMonoidBuilderInst(p: Rep[MonoidBuilderInst]) {
    def toData: Rep[MonoidBuilderInstData] = {
      isoMonoidBuilderInst.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMonoidBuilderInst: Iso[MonoidBuilderInstData, MonoidBuilderInst] =
    reifyObject(new MonoidBuilderInstIso())

  def mkMonoidBuilderInst
    (): Rep[MonoidBuilderInst] = {
    new MonoidBuilderInstCtor()
  }
  def unmkMonoidBuilderInst(p: Rep[MonoidBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MonoidBuilderInstElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object MonoidBuilderInstMethods {
    object pairMonoid {
      def unapply(d: Def[_]): Nullable[(Rep[MonoidBuilderInst], Rep[Monoid[A]], Rep[Monoid[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[MonoidBuilderInstElem] && method.getName == "pairMonoid" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[MonoidBuilderInst], Rep[Monoid[A]], Rep[Monoid[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[MonoidBuilderInst], Rep[Monoid[A]], Rep[Monoid[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object intPlusMonoid {
      def unapply(d: Def[_]): Nullable[Rep[MonoidBuilderInst]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MonoidBuilderInstElem] && method.getName == "intPlusMonoid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MonoidBuilderInst]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MonoidBuilderInst]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longPlusMonoid {
      def unapply(d: Def[_]): Nullable[Rep[MonoidBuilderInst]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MonoidBuilderInstElem] && method.getName == "longPlusMonoid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MonoidBuilderInst]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MonoidBuilderInst]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object MonoidBuilderInstCompanionMethods {
  }
} // of object MonoidBuilderInst
  registerEntityObject("MonoidBuilderInst", MonoidBuilderInst)

object IntPlusMonoid extends EntityObject("IntPlusMonoid") {
  case class IntPlusMonoidCtor
      (override val zero: Rep[Int])
    extends IntPlusMonoid(zero) with Def[IntPlusMonoid] {
    override lazy val eT: Elem[Int] = implicitly[Elem[Int]]
    lazy val selfType = element[IntPlusMonoid]
    override def transform(t: Transformer) = IntPlusMonoidCtor(t(zero))
  }
  // elem for concrete class
  class IntPlusMonoidElem(val iso: Iso[IntPlusMonoidData, IntPlusMonoid])
    extends MonoidElem[Int, IntPlusMonoid]
    with ConcreteElem[IntPlusMonoidData, IntPlusMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(IntElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertMonoid(x: Rep[Monoid[Int]]) = RIntPlusMonoid(x.zero)
    override def getDefaultRep = RIntPlusMonoid(0)
    override lazy val tag = {
      weakTypeTag[IntPlusMonoid]
    }
  }

  // state representation type
  type IntPlusMonoidData = Int

  // 3) Iso for concrete class
  class IntPlusMonoidIso
    extends EntityIso[IntPlusMonoidData, IntPlusMonoid] with Def[IntPlusMonoidIso] {
    override def transform(t: Transformer) = new IntPlusMonoidIso()
    private lazy val _safeFrom = fun { p: Rep[IntPlusMonoid] => p.zero }
    override def from(p: Rep[IntPlusMonoid]) =
      tryConvert[IntPlusMonoid, Int](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Int]) = {
      val zero = p
      RIntPlusMonoid(zero)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new IntPlusMonoidElem(self)
    lazy val selfType = new IntPlusMonoidIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class IntPlusMonoidIsoElem() extends Elem[IntPlusMonoidIso] {
    def getDefaultRep = reifyObject(new IntPlusMonoidIso())
    lazy val tag = {
      weakTypeTag[IntPlusMonoidIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class IntPlusMonoidCompanionCtor extends CompanionDef[IntPlusMonoidCompanionCtor] with IntPlusMonoidCompanion {
    def selfType = IntPlusMonoidCompanionElem
    override def toString = "IntPlusMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Rep[Int]): Rep[IntPlusMonoid] =
      mkIntPlusMonoid(zero)

    def unapply(p: Rep[Monoid[Int]]) = unmkIntPlusMonoid(p)
  }
  lazy val IntPlusMonoidRep: Rep[IntPlusMonoidCompanionCtor] = new IntPlusMonoidCompanionCtor
  lazy val RIntPlusMonoid: IntPlusMonoidCompanionCtor = proxyIntPlusMonoidCompanion(IntPlusMonoidRep)
  implicit def proxyIntPlusMonoidCompanion(p: Rep[IntPlusMonoidCompanionCtor]): IntPlusMonoidCompanionCtor = {
    if (p.rhs.isInstanceOf[IntPlusMonoidCompanionCtor])
      p.rhs.asInstanceOf[IntPlusMonoidCompanionCtor]
    else
      proxyOps[IntPlusMonoidCompanionCtor](p)
  }

  implicit case object IntPlusMonoidCompanionElem extends CompanionElem[IntPlusMonoidCompanionCtor] {
    lazy val tag = weakTypeTag[IntPlusMonoidCompanionCtor]
    protected def getDefaultRep = IntPlusMonoidRep
  }

  implicit def proxyIntPlusMonoid(p: Rep[IntPlusMonoid]): IntPlusMonoid =
    proxyOps[IntPlusMonoid](p)

  implicit class ExtendedIntPlusMonoid(p: Rep[IntPlusMonoid]) {
    def toData: Rep[IntPlusMonoidData] = {
      isoIntPlusMonoid.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIntPlusMonoid: Iso[IntPlusMonoidData, IntPlusMonoid] =
    reifyObject(new IntPlusMonoidIso())

  def mkIntPlusMonoid
    (zero: Rep[Int]): Rep[IntPlusMonoid] = {
    new IntPlusMonoidCtor(zero)
  }
  def unmkIntPlusMonoid(p: Rep[Monoid[Int]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntPlusMonoidElem @unchecked =>
      Some((asRep[IntPlusMonoid](p).zero))
    case _ =>
      None
  }

    object IntPlusMonoidMethods {
    object plus {
      def unapply(d: Def[_]): Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IntPlusMonoidElem] && method.getName == "plus" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object power {
      def unapply(d: Def[_]): Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IntPlusMonoidElem] && method.getName == "power" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IntPlusMonoid], Rep[Int], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object IntPlusMonoidCompanionMethods {
  }
} // of object IntPlusMonoid
  registerEntityObject("IntPlusMonoid", IntPlusMonoid)

object LongPlusMonoid extends EntityObject("LongPlusMonoid") {
  case class LongPlusMonoidCtor
      (override val zero: Rep[Long])
    extends LongPlusMonoid(zero) with Def[LongPlusMonoid] {
    override lazy val eT: Elem[Long] = implicitly[Elem[Long]]
    lazy val selfType = element[LongPlusMonoid]
    override def transform(t: Transformer) = LongPlusMonoidCtor(t(zero))
  }
  // elem for concrete class
  class LongPlusMonoidElem(val iso: Iso[LongPlusMonoidData, LongPlusMonoid])
    extends MonoidElem[Long, LongPlusMonoid]
    with ConcreteElem[LongPlusMonoidData, LongPlusMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(LongElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertMonoid(x: Rep[Monoid[Long]]) = RLongPlusMonoid(x.zero)
    override def getDefaultRep = RLongPlusMonoid(0l)
    override lazy val tag = {
      weakTypeTag[LongPlusMonoid]
    }
  }

  // state representation type
  type LongPlusMonoidData = Long

  // 3) Iso for concrete class
  class LongPlusMonoidIso
    extends EntityIso[LongPlusMonoidData, LongPlusMonoid] with Def[LongPlusMonoidIso] {
    override def transform(t: Transformer) = new LongPlusMonoidIso()
    private lazy val _safeFrom = fun { p: Rep[LongPlusMonoid] => p.zero }
    override def from(p: Rep[LongPlusMonoid]) =
      tryConvert[LongPlusMonoid, Long](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Long]) = {
      val zero = p
      RLongPlusMonoid(zero)
    }
    lazy val eFrom = element[Long]
    lazy val eTo = new LongPlusMonoidElem(self)
    lazy val selfType = new LongPlusMonoidIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class LongPlusMonoidIsoElem() extends Elem[LongPlusMonoidIso] {
    def getDefaultRep = reifyObject(new LongPlusMonoidIso())
    lazy val tag = {
      weakTypeTag[LongPlusMonoidIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class LongPlusMonoidCompanionCtor extends CompanionDef[LongPlusMonoidCompanionCtor] with LongPlusMonoidCompanion {
    def selfType = LongPlusMonoidCompanionElem
    override def toString = "LongPlusMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Rep[Long]): Rep[LongPlusMonoid] =
      mkLongPlusMonoid(zero)

    def unapply(p: Rep[Monoid[Long]]) = unmkLongPlusMonoid(p)
  }
  lazy val LongPlusMonoidRep: Rep[LongPlusMonoidCompanionCtor] = new LongPlusMonoidCompanionCtor
  lazy val RLongPlusMonoid: LongPlusMonoidCompanionCtor = proxyLongPlusMonoidCompanion(LongPlusMonoidRep)
  implicit def proxyLongPlusMonoidCompanion(p: Rep[LongPlusMonoidCompanionCtor]): LongPlusMonoidCompanionCtor = {
    if (p.rhs.isInstanceOf[LongPlusMonoidCompanionCtor])
      p.rhs.asInstanceOf[LongPlusMonoidCompanionCtor]
    else
      proxyOps[LongPlusMonoidCompanionCtor](p)
  }

  implicit case object LongPlusMonoidCompanionElem extends CompanionElem[LongPlusMonoidCompanionCtor] {
    lazy val tag = weakTypeTag[LongPlusMonoidCompanionCtor]
    protected def getDefaultRep = LongPlusMonoidRep
  }

  implicit def proxyLongPlusMonoid(p: Rep[LongPlusMonoid]): LongPlusMonoid =
    proxyOps[LongPlusMonoid](p)

  implicit class ExtendedLongPlusMonoid(p: Rep[LongPlusMonoid]) {
    def toData: Rep[LongPlusMonoidData] = {
      isoLongPlusMonoid.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoLongPlusMonoid: Iso[LongPlusMonoidData, LongPlusMonoid] =
    reifyObject(new LongPlusMonoidIso())

  def mkLongPlusMonoid
    (zero: Rep[Long]): Rep[LongPlusMonoid] = {
    new LongPlusMonoidCtor(zero)
  }
  def unmkLongPlusMonoid(p: Rep[Monoid[Long]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: LongPlusMonoidElem @unchecked =>
      Some((asRep[LongPlusMonoid](p).zero))
    case _ =>
      None
  }

    object LongPlusMonoidMethods {
    object plus {
      def unapply(d: Def[_]): Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[LongPlusMonoidElem] && method.getName == "plus" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object power {
      def unapply(d: Def[_]): Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[LongPlusMonoidElem] && method.getName == "power" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[LongPlusMonoid], Rep[Long], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object LongPlusMonoidCompanionMethods {
  }
} // of object LongPlusMonoid
  registerEntityObject("LongPlusMonoid", LongPlusMonoid)

object PairMonoid extends EntityObject("PairMonoid") {
  case class PairMonoidCtor[A, B]
      (override val m1: Rep[Monoid[A]], override val m2: Rep[Monoid[B]])
    extends PairMonoid[A, B](m1, m2) with Def[PairMonoid[A, B]] {
    implicit lazy val eA = m1.eT;
implicit lazy val eB = m2.eT
    override lazy val eT: Elem[(A, B)] = implicitly[Elem[(A, B)]]
    lazy val selfType = element[PairMonoid[A, B]]
    override def transform(t: Transformer) = PairMonoidCtor[A, B](t(m1), t(m2))
  }
  // elem for concrete class
  class PairMonoidElem[A, B](val iso: Iso[PairMonoidData[A, B], PairMonoid[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MonoidElem[(A, B), PairMonoid[A, B]]
    with ConcreteElem[PairMonoidData[A, B], PairMonoid[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(pairElement(element[A],element[B])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override def convertMonoid(x: Rep[Monoid[(A, B)]]) = // Converter is not generated by meta
!!!("Cannot convert from Monoid to PairMonoid: missing fields List(m1, m2)")
    override def getDefaultRep = RPairMonoid(element[Monoid[A]].defaultRepValue, element[Monoid[B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMonoid[A, B]]
    }
  }

  // state representation type
  type PairMonoidData[A, B] = (Monoid[A], Monoid[B])

  // 3) Iso for concrete class
  class PairMonoidIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[PairMonoidData[A, B], PairMonoid[A, B]] with Def[PairMonoidIso[A, B]] {
    override def transform(t: Transformer) = new PairMonoidIso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Rep[PairMonoid[A, B]] => (p.m1, p.m2) }
    override def from(p: Rep[PairMonoid[A, B]]) =
      tryConvert[PairMonoid[A, B], (Monoid[A], Monoid[B])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Monoid[A], Monoid[B])]) = {
      val Pair(m1, m2) = p
      RPairMonoid(m1, m2)
    }
    lazy val eFrom = pairElement(element[Monoid[A]], element[Monoid[B]])
    lazy val eTo = new PairMonoidElem[A, B](self)
    lazy val selfType = new PairMonoidIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class PairMonoidIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[PairMonoidIso[A, B]] {
    def getDefaultRep = reifyObject(new PairMonoidIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMonoidIso[A, B]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairMonoidCompanionCtor extends CompanionDef[PairMonoidCompanionCtor] with PairMonoidCompanion {
    def selfType = PairMonoidCompanionElem
    override def toString = "PairMonoidCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[PairMonoidData[A, B]]): Rep[PairMonoid[A, B]] = {
      implicit val eA = p._1.eT;
implicit val eB = p._2.eT
      isoPairMonoid[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](m1: Rep[Monoid[A]], m2: Rep[Monoid[B]]): Rep[PairMonoid[A, B]] =
      mkPairMonoid(m1, m2)

    def unapply[A, B](p: Rep[Monoid[(A, B)]]) = unmkPairMonoid(p)
  }
  lazy val PairMonoidRep: Rep[PairMonoidCompanionCtor] = new PairMonoidCompanionCtor
  lazy val RPairMonoid: PairMonoidCompanionCtor = proxyPairMonoidCompanion(PairMonoidRep)
  implicit def proxyPairMonoidCompanion(p: Rep[PairMonoidCompanionCtor]): PairMonoidCompanionCtor = {
    if (p.rhs.isInstanceOf[PairMonoidCompanionCtor])
      p.rhs.asInstanceOf[PairMonoidCompanionCtor]
    else
      proxyOps[PairMonoidCompanionCtor](p)
  }

  implicit case object PairMonoidCompanionElem extends CompanionElem[PairMonoidCompanionCtor] {
    lazy val tag = weakTypeTag[PairMonoidCompanionCtor]
    protected def getDefaultRep = PairMonoidRep
  }

  implicit def proxyPairMonoid[A, B](p: Rep[PairMonoid[A, B]]): PairMonoid[A, B] =
    proxyOps[PairMonoid[A, B]](p)

  implicit class ExtendedPairMonoid[A, B](p: Rep[PairMonoid[A, B]]) {
    def toData: Rep[PairMonoidData[A, B]] = {
      implicit val eA = p.m1.eT;
implicit val eB = p.m2.eT
      isoPairMonoid(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoPairMonoid[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairMonoidData[A, B], PairMonoid[A, B]] =
    reifyObject(new PairMonoidIso[A, B]()(eA, eB))

  def mkPairMonoid[A, B]
    (m1: Rep[Monoid[A]], m2: Rep[Monoid[B]]): Rep[PairMonoid[A, B]] = {
    new PairMonoidCtor[A, B](m1, m2)
  }
  def unmkPairMonoid[A, B](p: Rep[Monoid[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairMonoidElem[A, B] @unchecked =>
      Some((asRep[PairMonoid[A, B]](p).m1, asRep[PairMonoid[A, B]](p).m2))
    case _ =>
      None
  }

    object PairMonoidMethods {
    object zero {
      def unapply(d: Def[_]): Nullable[Rep[PairMonoid[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMonoidElem[_, _]] && method.getName == "zero" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairMonoid[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairMonoid[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object plus {
      def unapply(d: Def[_]): Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[(A, B)]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairMonoidElem[_, _]] && method.getName == "plus" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[(A, B)]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[(A, B)]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object power {
      def unapply(d: Def[_]): Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairMonoidElem[_, _]] && method.getName == "power" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[Int]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairMonoid[A, B]], Rep[(A, B)], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PairMonoidCompanionMethods {
  }
} // of object PairMonoid
  registerEntityObject("PairMonoid", PairMonoid)

  registerModule(MonoidInstancesModule)
}

object MonoidInstancesModule extends scalan.ModuleInfo("special.collection", "MonoidInstances")
}

trait MonoidInstancesModule extends special.collection.impl.MonoidInstancesDefs {self: Library =>}
