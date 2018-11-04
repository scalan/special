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
import MonoidBuilderInst._

object MonoidBuilderInst extends EntityObject("MonoidBuilderInst") {
  case class MonoidBuilderInstCtor
      ()
    extends MonoidBuilderInst() with Def[MonoidBuilderInst] {
    lazy val selfType = element[MonoidBuilderInst]
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

  registerModule(MonoidInstancesModule)
}

object MonoidInstancesModule extends scalan.ModuleInfo("special.collection", "MonoidInstances")
}

trait MonoidInstancesModule extends special.collection.impl.MonoidInstancesDefs {self: Library =>}
