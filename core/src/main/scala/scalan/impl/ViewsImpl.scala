package scalan

import java.lang.reflect.Method
import scala.language.higherKinds
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait ViewsDefs extends Views {
  self: ViewsModule with Scalan =>
import IsoUR._
import Converter._
import AbsorbFirstUnitIso._
import AbsorbSecondUnitIso._
import ComposeIso._
import ConverterIso._
import FuncIso._
import IdentityIso._
import Iso1UR._
import PairIso._
import SumIso._
import ThunkIso._

object IsoUR extends EntityObject("IsoUR") {
  private val IsoURClass = classOf[IsoUR[_, _]]

  // entityAdapter for IsoUR trait
  case class IsoURAdapter[From, To](source: Ref[IsoUR[From, To]])
      extends IsoUR[From, To]
      with Def[IsoUR[From, To]] {
    implicit lazy val eFrom = source.elem.typeArgs("From")._1.asElem[From];
implicit lazy val eTo = source.elem.typeArgs("To")._1.asElem[To]

    val resultType: Elem[IsoUR[From, To]] = element[IsoUR[From, To]]
    override def transform(t: Transformer) = IsoURAdapter[From, To](t(source))

    def from(p: Ref[To]): Ref[From] = {
      asRep[From](mkMethodCall(source,
        IsoURClass.getMethod("from", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[From]))
    }

    def to(p: Ref[From]): Ref[To] = {
      asRep[To](mkMethodCall(source,
        IsoURClass.getMethod("to", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[To]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefIsoUR[From, To](p: Ref[IsoUR[From, To]]): IsoUR[From, To] = {
    if (p.rhs.isInstanceOf[IsoUR[From, To]@unchecked]) p.rhs.asInstanceOf[IsoUR[From, To]]
    else
      IsoURAdapter(p)
  }

  // familyElem
  class IsoURElem[From, To, To0 <: IsoUR[From, To]](implicit _eFrom: Elem[From], _eTo: Elem[To])
    extends EntityElem[To0] {
    def eFrom = _eFrom
    def eTo = _eTo

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("From" -> (eFrom -> scalan.util.Invariant), "To" -> (eTo -> scalan.util.Invariant))
  }

  implicit def isoURElement[From, To](implicit eFrom: Elem[From], eTo: Elem[To]): Elem[IsoUR[From, To]] =
    cachedElemByClass(eFrom, eTo)(classOf[IsoURElem[From, To, IsoUR[From, To]]])

  implicit case object IsoURCompanionElem extends CompanionElem[IsoURCompanionCtor]

  abstract class IsoURCompanionCtor extends CompanionDef[IsoURCompanionCtor] {
    def resultType = IsoURCompanionElem
    override def toString = "IsoUR"
  }
  implicit def unrefIsoURCompanionCtor(p: Ref[IsoURCompanionCtor]): IsoURCompanionCtor =
    p.rhs.asInstanceOf[IsoURCompanionCtor]

  lazy val RIsoUR: Ref[IsoURCompanionCtor] = new IsoURCompanionCtor {
  }
} // of object IsoUR
  registerEntityObject("IsoUR", IsoUR)

object Iso1UR extends EntityObject("Iso1UR") {
  private val Iso1URClass = classOf[Iso1UR[_, _, C] forSome {type C[_]}]

  // entityAdapter for Iso1UR trait
  case class Iso1URAdapter[A, B, C[_]](source: Ref[Iso1UR[A, B, C]])
      extends Iso1UR[A, B, C]
      with Def[Iso1UR[A, B, C]] {
    implicit override lazy val eA = source.elem.typeArgs("A")._1.asElem[A];
implicit override lazy val eB = source.elem.typeArgs("B")._1.asElem[B];
implicit lazy val cC = source.elem.typeArgs("C")._1.asCont[C]

    val resultType: Elem[Iso1UR[A, B, C]] = element[Iso1UR[A, B, C]]
    override def transform(t: Transformer) = Iso1URAdapter[A, B, C](t(source))

    def innerIso: Iso[A, B] = {
      asRep[IsoUR[A, B]](mkMethodCall(source,
        Iso1URClass.getMethod("innerIso"),
        WrappedArray.empty,
        true, true, element[IsoUR[A, B]]))
    }

    def from(p: Ref[C[B]]): Ref[C[A]] = {
      asRep[C[A]](mkMethodCall(source,
        Iso1URClass.getMethod("from", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[C[A]]))
    }

    def to(p: Ref[C[A]]): Ref[C[B]] = {
      asRep[C[B]](mkMethodCall(source,
        Iso1URClass.getMethod("to", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[C[B]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefIso1UR[A, B, C[_]](p: Ref[Iso1UR[A, B, C]]): Iso1UR[A, B, C] = {
    if (p.rhs.isInstanceOf[Iso1UR[A, B, C]@unchecked]) p.rhs.asInstanceOf[Iso1UR[A, B, C]]
    else
      Iso1URAdapter(p)
  }

  // familyElem
  class Iso1URElem[A, B, C[_], To <: Iso1UR[A, B, C]](implicit _eA: Elem[A], _eB: Elem[B], _cC: Cont[C])
    extends IsoURElem[C[A], C[B], To] {
    def eA = _eA
    def eB = _eB
    def cC = _cC

    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[C[A]], element[C[B]]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (cC -> scalan.util.Invariant))
  }

  implicit def iso1URElement[A, B, C[_]](implicit eA: Elem[A], eB: Elem[B], cC: Cont[C]): Elem[Iso1UR[A, B, C]] =
    cachedElemByClass(eA, eB, cC)(classOf[Iso1URElem[A, B, C, Iso1UR[A, B, C]]])

  implicit case object Iso1URCompanionElem extends CompanionElem[Iso1URCompanionCtor]

  abstract class Iso1URCompanionCtor extends CompanionDef[Iso1URCompanionCtor] {
    def resultType = Iso1URCompanionElem
    override def toString = "Iso1UR"
  }
  implicit def unrefIso1URCompanionCtor(p: Ref[Iso1URCompanionCtor]): Iso1URCompanionCtor =
    p.rhs.asInstanceOf[Iso1URCompanionCtor]

  lazy val RIso1UR: Ref[Iso1URCompanionCtor] = new Iso1URCompanionCtor {
  }
} // of object Iso1UR
  registerEntityObject("Iso1UR", Iso1UR)

object IdentityIso extends EntityObject("IdentityIso") {
  case class IdentityIsoCtor[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso[A]() with Def[IdentityIso[A]] {
    lazy val resultType = element[IdentityIso[A]]
    override def transform(t: Transformer) = IdentityIsoCtor[A]()(eA)
  }
  // elem for concrete class
  class IdentityIsoElem[A](val iso: Iso[IdentityIsoData[A], IdentityIso[A]])(implicit val eA: Elem[A])
    extends IsoURElem[A, A, IdentityIso[A]]
    with ConcreteElem[IdentityIsoData[A], IdentityIso[A]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  // state representation type
  type IdentityIsoData[A] = Unit

  // 3) Iso for concrete class
  class IdentityIsoIso[A](implicit eA: Elem[A])
    extends EntityIso[IdentityIsoData[A], IdentityIso[A]] with Def[IdentityIsoIso[A]] {
    override def transform(t: Transformer) = new IdentityIsoIso[A]()(eA)
    private lazy val _safeFrom = fun { p: Ref[IdentityIso[A]] => () }
    override def from(p: Ref[IdentityIso[A]]) =
      tryConvert[IdentityIso[A], Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Unit]) = {
      val unit = p
      RIdentityIso()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityIsoElem[A](self)
    lazy val resultType = new IdentityIsoIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class IdentityIsoIsoElem[A](eA: Elem[A]) extends Elem[IdentityIsoIso[A]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IdentityIsoCompanionCtor extends CompanionDef[IdentityIsoCompanionCtor] {
    def resultType = IdentityIsoCompanionElem
    override def toString = "IdentityIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Ref[IdentityIsoData[A]])(implicit eA: Elem[A]): Ref[IdentityIso[A]] = {
      isoIdentityIso[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eA: Elem[A]): Ref[IdentityIso[A]] =
      mkIdentityIso()

    def unapply[A](p: Ref[IsoUR[A, A]]) = unmkIdentityIso(p)
  }
  lazy val IdentityIsoRef: Ref[IdentityIsoCompanionCtor] = new IdentityIsoCompanionCtor
  lazy val RIdentityIso: IdentityIsoCompanionCtor = unrefIdentityIsoCompanion(IdentityIsoRef)
  implicit def unrefIdentityIsoCompanion(p: Ref[IdentityIsoCompanionCtor]): IdentityIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[IdentityIsoCompanionCtor])
      p.rhs.asInstanceOf[IdentityIsoCompanionCtor]
    else
      unrefDelegate[IdentityIsoCompanionCtor](p)
  }

  implicit case object IdentityIsoCompanionElem extends CompanionElem[IdentityIsoCompanionCtor]

  implicit def unrefIdentityIso[A](p: Ref[IdentityIso[A]]): IdentityIso[A] = {
    if (p.rhs.isInstanceOf[IdentityIso[A]@unchecked])
      p.rhs.asInstanceOf[IdentityIso[A]]
    else
      unrefDelegate[IdentityIso[A]](p)
  }

  implicit class ExtendedIdentityIso[A](p: Ref[IdentityIso[A]])(implicit eA: Elem[A]) {
    def toData: Ref[IdentityIsoData[A]] = {
      isoIdentityIso(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityIso[A](implicit eA: Elem[A]): Iso[IdentityIsoData[A], IdentityIso[A]] =
    reifyObject(new IdentityIsoIso[A]()(eA))

  def mkIdentityIso[A]
    ()(implicit eA: Elem[A]): Ref[IdentityIso[A]] = {
    new IdentityIsoCtor[A]()
  }
  def unmkIdentityIso[A](p: Ref[IsoUR[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityIsoElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }
} // of object IdentityIso
  registerEntityObject("IdentityIso", IdentityIso)

object PairIso extends EntityObject("PairIso") {
  case class PairIsoCtor[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])
    extends PairIso[A1, A2, B1, B2](iso1, iso2) with Def[PairIso[A1, A2, B1, B2]] {
    implicit lazy val eA1 = iso1.eFrom;
implicit lazy val eA2 = iso2.eFrom;
implicit lazy val eB1 = iso1.eTo;
implicit lazy val eB2 = iso2.eTo
    override lazy val eFrom: Elem[(A1, A2)] = implicitly[Elem[(A1, A2)]]
override lazy val eTo: Elem[(B1, B2)] = implicitly[Elem[(B1, B2)]]
    lazy val resultType = element[PairIso[A1, A2, B1, B2]]
    override def transform(t: Transformer) = PairIsoCtor[A1, A2, B1, B2](t(iso1), t(iso2))
  }
  // elem for concrete class
  class PairIsoElem[A1, A2, B1, B2](val iso: Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[(A1, A2), (B1, B2), PairIso[A1, A2, B1, B2]]
    with ConcreteElem[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }

  // state representation type
  type PairIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class PairIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] with Def[PairIsoIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Ref[PairIso[A1, A2, B1, B2]] => (p.iso1, p.iso2) }
    override def from(p: Ref[PairIso[A1, A2, B1, B2]]) =
      tryConvert[PairIso[A1, A2, B1, B2], (IsoUR[A1, B1], IsoUR[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      RPairIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new PairIsoElem[A1, A2, B1, B2](self)
    lazy val resultType = new PairIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class PairIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[PairIsoIso[A1, A2, B1, B2]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairIsoCompanionCtor extends CompanionDef[PairIsoCompanionCtor] with PairIsoCompanion {
    def resultType = PairIsoCompanionElem
    override def toString = "PairIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Ref[PairIsoData[A1, A2, B1, B2]]): Ref[PairIso[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eFrom;
implicit val eA2 = p._2.eFrom;
implicit val eB1 = p._1.eTo;
implicit val eB2 = p._2.eTo
      isoPairIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Ref[PairIso[A1, A2, B1, B2]] =
      mkPairIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Ref[IsoUR[(A1, A2), (B1, B2)]]) = unmkPairIso(p)
  }
  lazy val PairIsoRef: Ref[PairIsoCompanionCtor] = new PairIsoCompanionCtor
  lazy val RPairIso: PairIsoCompanionCtor = unrefPairIsoCompanion(PairIsoRef)
  implicit def unrefPairIsoCompanion(p: Ref[PairIsoCompanionCtor]): PairIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[PairIsoCompanionCtor])
      p.rhs.asInstanceOf[PairIsoCompanionCtor]
    else
      unrefDelegate[PairIsoCompanionCtor](p)
  }

  implicit case object PairIsoCompanionElem extends CompanionElem[PairIsoCompanionCtor]

  implicit def unrefPairIso[A1, A2, B1, B2](p: Ref[PairIso[A1, A2, B1, B2]]): PairIso[A1, A2, B1, B2] = {
    if (p.rhs.isInstanceOf[PairIso[A1, A2, B1, B2]@unchecked])
      p.rhs.asInstanceOf[PairIso[A1, A2, B1, B2]]
    else
      unrefDelegate[PairIso[A1, A2, B1, B2]](p)
  }

  implicit class ExtendedPairIso[A1, A2, B1, B2](p: Ref[PairIso[A1, A2, B1, B2]]) {
    def toData: Ref[PairIsoData[A1, A2, B1, B2]] = {
      implicit val eA1 = p.iso1.eFrom;
implicit val eA2 = p.iso2.eFrom;
implicit val eB1 = p.iso1.eTo;
implicit val eB2 = p.iso2.eTo
      isoPairIso(eA1, eA2, eB1, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoPairIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] =
    reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  def mkPairIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Ref[PairIso[A1, A2, B1, B2]] = {
    new PairIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkPairIso[A1, A2, B1, B2](p: Ref[IsoUR[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[PairIso[A1, A2, B1, B2]](p).iso1, asRep[PairIso[A1, A2, B1, B2]](p).iso2))
    case _ =>
      None
  }
} // of object PairIso
  registerEntityObject("PairIso", PairIso)

object AbsorbFirstUnitIso extends EntityObject("AbsorbFirstUnitIso") {
  case class AbsorbFirstUnitIsoCtor[A2, B2]
      (override val iso2: Iso[A2, B2])
    extends AbsorbFirstUnitIso[A2, B2](iso2) with Def[AbsorbFirstUnitIso[A2, B2]] {
    implicit lazy val eA2 = iso2.eFrom;
implicit lazy val eB2 = iso2.eTo
    override lazy val eFrom: Elem[A2] = eA2
override lazy val eTo: Elem[(Unit, B2)] = implicitly[Elem[(Unit, B2)]]
    lazy val resultType = element[AbsorbFirstUnitIso[A2, B2]]
    override def transform(t: Transformer) = AbsorbFirstUnitIsoCtor[A2, B2](t(iso2))
  }
  // elem for concrete class
  class AbsorbFirstUnitIsoElem[A2, B2](val iso: Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]])(implicit val eA2: Elem[A2], val eB2: Elem[B2])
    extends IsoURElem[A2, (Unit, B2), AbsorbFirstUnitIso[A2, B2]]
    with ConcreteElem[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A2], pairElement(UnitElement,element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }

  // state representation type
  type AbsorbFirstUnitIsoData[A2, B2] = IsoUR[A2, B2]

  // 3) Iso for concrete class
  class AbsorbFirstUnitIsoIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2])
    extends EntityIso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] with Def[AbsorbFirstUnitIsoIso[A2, B2]] {
    override def transform(t: Transformer) = new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2)
    private lazy val _safeFrom = fun { p: Ref[AbsorbFirstUnitIso[A2, B2]] => p.iso2 }
    override def from(p: Ref[AbsorbFirstUnitIso[A2, B2]]) =
      tryConvert[AbsorbFirstUnitIso[A2, B2], IsoUR[A2, B2]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[IsoUR[A2, B2]]) = {
      val iso2 = p
      RAbsorbFirstUnitIso(iso2)
    }
    lazy val eFrom = element[IsoUR[A2, B2]]
    lazy val eTo = new AbsorbFirstUnitIsoElem[A2, B2](self)
    lazy val resultType = new AbsorbFirstUnitIsoIsoElem[A2, B2](eA2, eB2)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA2
      case 1 => eB2
    }
  }
  case class AbsorbFirstUnitIsoIsoElem[A2, B2](eA2: Elem[A2], eB2: Elem[B2]) extends Elem[AbsorbFirstUnitIsoIso[A2, B2]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbFirstUnitIsoCompanionCtor extends CompanionDef[AbsorbFirstUnitIsoCompanionCtor] {
    def resultType = AbsorbFirstUnitIsoCompanionElem
    override def toString = "AbsorbFirstUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A2, B2](iso2: Iso[A2, B2]): Ref[AbsorbFirstUnitIso[A2, B2]] =
      mkAbsorbFirstUnitIso(iso2)

    def unapply[A2, B2](p: Ref[IsoUR[A2, (Unit, B2)]]) = unmkAbsorbFirstUnitIso(p)
  }
  lazy val AbsorbFirstUnitIsoRef: Ref[AbsorbFirstUnitIsoCompanionCtor] = new AbsorbFirstUnitIsoCompanionCtor
  lazy val RAbsorbFirstUnitIso: AbsorbFirstUnitIsoCompanionCtor = unrefAbsorbFirstUnitIsoCompanion(AbsorbFirstUnitIsoRef)
  implicit def unrefAbsorbFirstUnitIsoCompanion(p: Ref[AbsorbFirstUnitIsoCompanionCtor]): AbsorbFirstUnitIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[AbsorbFirstUnitIsoCompanionCtor])
      p.rhs.asInstanceOf[AbsorbFirstUnitIsoCompanionCtor]
    else
      unrefDelegate[AbsorbFirstUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbFirstUnitIsoCompanionElem extends CompanionElem[AbsorbFirstUnitIsoCompanionCtor]

  implicit def unrefAbsorbFirstUnitIso[A2, B2](p: Ref[AbsorbFirstUnitIso[A2, B2]]): AbsorbFirstUnitIso[A2, B2] = {
    if (p.rhs.isInstanceOf[AbsorbFirstUnitIso[A2, B2]@unchecked])
      p.rhs.asInstanceOf[AbsorbFirstUnitIso[A2, B2]]
    else
      unrefDelegate[AbsorbFirstUnitIso[A2, B2]](p)
  }

  implicit class ExtendedAbsorbFirstUnitIso[A2, B2](p: Ref[AbsorbFirstUnitIso[A2, B2]]) {
    def toData: Ref[AbsorbFirstUnitIsoData[A2, B2]] = {
      implicit val eA2 = p.iso2.eFrom;
implicit val eB2 = p.iso2.eTo
      isoAbsorbFirstUnitIso(eA2, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbFirstUnitIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2]): Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] =
    reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))

  def mkAbsorbFirstUnitIso[A2, B2]
    (iso2: Iso[A2, B2]): Ref[AbsorbFirstUnitIso[A2, B2]] = {
    new AbsorbFirstUnitIsoCtor[A2, B2](iso2)
  }
  def unmkAbsorbFirstUnitIso[A2, B2](p: Ref[IsoUR[A2, (Unit, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbFirstUnitIsoElem[A2, B2] @unchecked =>
      Some((asRep[AbsorbFirstUnitIso[A2, B2]](p).iso2))
    case _ =>
      None
  }
} // of object AbsorbFirstUnitIso
  registerEntityObject("AbsorbFirstUnitIso", AbsorbFirstUnitIso)

object AbsorbSecondUnitIso extends EntityObject("AbsorbSecondUnitIso") {
  case class AbsorbSecondUnitIsoCtor[A1, B1]
      (override val iso1: Iso[A1, B1])
    extends AbsorbSecondUnitIso[A1, B1](iso1) with Def[AbsorbSecondUnitIso[A1, B1]] {
    implicit lazy val eA1 = iso1.eFrom;
implicit lazy val eB1 = iso1.eTo
    override lazy val eFrom: Elem[A1] = eA1
override lazy val eTo: Elem[(B1, Unit)] = implicitly[Elem[(B1, Unit)]]
    lazy val resultType = element[AbsorbSecondUnitIso[A1, B1]]
    override def transform(t: Transformer) = AbsorbSecondUnitIsoCtor[A1, B1](t(iso1))
  }
  // elem for concrete class
  class AbsorbSecondUnitIsoElem[A1, B1](val iso: Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]])(implicit val eA1: Elem[A1], val eB1: Elem[B1])
    extends IsoURElem[A1, (B1, Unit), AbsorbSecondUnitIso[A1, B1]]
    with ConcreteElem[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A1], pairElement(element[B1],UnitElement)))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))
  }

  // state representation type
  type AbsorbSecondUnitIsoData[A1, B1] = IsoUR[A1, B1]

  // 3) Iso for concrete class
  class AbsorbSecondUnitIsoIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1])
    extends EntityIso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] with Def[AbsorbSecondUnitIsoIso[A1, B1]] {
    override def transform(t: Transformer) = new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1)
    private lazy val _safeFrom = fun { p: Ref[AbsorbSecondUnitIso[A1, B1]] => p.iso1 }
    override def from(p: Ref[AbsorbSecondUnitIso[A1, B1]]) =
      tryConvert[AbsorbSecondUnitIso[A1, B1], IsoUR[A1, B1]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[IsoUR[A1, B1]]) = {
      val iso1 = p
      RAbsorbSecondUnitIso(iso1)
    }
    lazy val eFrom = element[IsoUR[A1, B1]]
    lazy val eTo = new AbsorbSecondUnitIsoElem[A1, B1](self)
    lazy val resultType = new AbsorbSecondUnitIsoIsoElem[A1, B1](eA1, eB1)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eB1
    }
  }
  case class AbsorbSecondUnitIsoIsoElem[A1, B1](eA1: Elem[A1], eB1: Elem[B1]) extends Elem[AbsorbSecondUnitIsoIso[A1, B1]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbSecondUnitIsoCompanionCtor extends CompanionDef[AbsorbSecondUnitIsoCompanionCtor] {
    def resultType = AbsorbSecondUnitIsoCompanionElem
    override def toString = "AbsorbSecondUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A1, B1](iso1: Iso[A1, B1]): Ref[AbsorbSecondUnitIso[A1, B1]] =
      mkAbsorbSecondUnitIso(iso1)

    def unapply[A1, B1](p: Ref[IsoUR[A1, (B1, Unit)]]) = unmkAbsorbSecondUnitIso(p)
  }
  lazy val AbsorbSecondUnitIsoRef: Ref[AbsorbSecondUnitIsoCompanionCtor] = new AbsorbSecondUnitIsoCompanionCtor
  lazy val RAbsorbSecondUnitIso: AbsorbSecondUnitIsoCompanionCtor = unrefAbsorbSecondUnitIsoCompanion(AbsorbSecondUnitIsoRef)
  implicit def unrefAbsorbSecondUnitIsoCompanion(p: Ref[AbsorbSecondUnitIsoCompanionCtor]): AbsorbSecondUnitIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[AbsorbSecondUnitIsoCompanionCtor])
      p.rhs.asInstanceOf[AbsorbSecondUnitIsoCompanionCtor]
    else
      unrefDelegate[AbsorbSecondUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbSecondUnitIsoCompanionElem extends CompanionElem[AbsorbSecondUnitIsoCompanionCtor]

  implicit def unrefAbsorbSecondUnitIso[A1, B1](p: Ref[AbsorbSecondUnitIso[A1, B1]]): AbsorbSecondUnitIso[A1, B1] = {
    if (p.rhs.isInstanceOf[AbsorbSecondUnitIso[A1, B1]@unchecked])
      p.rhs.asInstanceOf[AbsorbSecondUnitIso[A1, B1]]
    else
      unrefDelegate[AbsorbSecondUnitIso[A1, B1]](p)
  }

  implicit class ExtendedAbsorbSecondUnitIso[A1, B1](p: Ref[AbsorbSecondUnitIso[A1, B1]]) {
    def toData: Ref[AbsorbSecondUnitIsoData[A1, B1]] = {
      implicit val eA1 = p.iso1.eFrom;
implicit val eB1 = p.iso1.eTo
      isoAbsorbSecondUnitIso(eA1, eB1).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbSecondUnitIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1]): Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] =
    reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))

  def mkAbsorbSecondUnitIso[A1, B1]
    (iso1: Iso[A1, B1]): Ref[AbsorbSecondUnitIso[A1, B1]] = {
    new AbsorbSecondUnitIsoCtor[A1, B1](iso1)
  }
  def unmkAbsorbSecondUnitIso[A1, B1](p: Ref[IsoUR[A1, (B1, Unit)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbSecondUnitIsoElem[A1, B1] @unchecked =>
      Some((asRep[AbsorbSecondUnitIso[A1, B1]](p).iso1))
    case _ =>
      None
  }
} // of object AbsorbSecondUnitIso
  registerEntityObject("AbsorbSecondUnitIso", AbsorbSecondUnitIso)

object SumIso extends EntityObject("SumIso") {
  case class SumIsoCtor[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])
    extends SumIso[A1, A2, B1, B2](iso1, iso2) with Def[SumIso[A1, A2, B1, B2]] {
    implicit lazy val eA1 = iso1.eFrom;
implicit lazy val eA2 = iso2.eFrom;
implicit lazy val eB1 = iso1.eTo;
implicit lazy val eB2 = iso2.eTo
    override lazy val eFrom: Elem[$bar[A1, A2]] = implicitly[Elem[$bar[A1, A2]]]
override lazy val eTo: Elem[$bar[B1, B2]] = implicitly[Elem[$bar[B1, B2]]]
    lazy val resultType = element[SumIso[A1, A2, B1, B2]]
    override def transform(t: Transformer) = SumIsoCtor[A1, A2, B1, B2](t(iso1), t(iso2))
  }
  // elem for concrete class
  class SumIsoElem[A1, A2, B1, B2](val iso: Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[$bar[A1, A2], $bar[B1, B2], SumIso[A1, A2, B1, B2]]
    with ConcreteElem[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }

  // state representation type
  type SumIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class SumIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] with Def[SumIsoIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Ref[SumIso[A1, A2, B1, B2]] => (p.iso1, p.iso2) }
    override def from(p: Ref[SumIso[A1, A2, B1, B2]]) =
      tryConvert[SumIso[A1, A2, B1, B2], (IsoUR[A1, B1], IsoUR[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      RSumIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new SumIsoElem[A1, A2, B1, B2](self)
    lazy val resultType = new SumIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class SumIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[SumIsoIso[A1, A2, B1, B2]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SumIsoCompanionCtor extends CompanionDef[SumIsoCompanionCtor] {
    def resultType = SumIsoCompanionElem
    override def toString = "SumIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Ref[SumIsoData[A1, A2, B1, B2]]): Ref[SumIso[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eFrom;
implicit val eA2 = p._2.eFrom;
implicit val eB1 = p._1.eTo;
implicit val eB2 = p._2.eTo
      isoSumIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Ref[SumIso[A1, A2, B1, B2]] =
      mkSumIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Ref[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso(p)
  }
  lazy val SumIsoRef: Ref[SumIsoCompanionCtor] = new SumIsoCompanionCtor
  lazy val RSumIso: SumIsoCompanionCtor = unrefSumIsoCompanion(SumIsoRef)
  implicit def unrefSumIsoCompanion(p: Ref[SumIsoCompanionCtor]): SumIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[SumIsoCompanionCtor])
      p.rhs.asInstanceOf[SumIsoCompanionCtor]
    else
      unrefDelegate[SumIsoCompanionCtor](p)
  }

  implicit case object SumIsoCompanionElem extends CompanionElem[SumIsoCompanionCtor]

  implicit def unrefSumIso[A1, A2, B1, B2](p: Ref[SumIso[A1, A2, B1, B2]]): SumIso[A1, A2, B1, B2] = {
    if (p.rhs.isInstanceOf[SumIso[A1, A2, B1, B2]@unchecked])
      p.rhs.asInstanceOf[SumIso[A1, A2, B1, B2]]
    else
      unrefDelegate[SumIso[A1, A2, B1, B2]](p)
  }

  implicit class ExtendedSumIso[A1, A2, B1, B2](p: Ref[SumIso[A1, A2, B1, B2]]) {
    def toData: Ref[SumIsoData[A1, A2, B1, B2]] = {
      implicit val eA1 = p.iso1.eFrom;
implicit val eA2 = p.iso2.eFrom;
implicit val eB1 = p.iso1.eTo;
implicit val eB2 = p.iso2.eTo
      isoSumIso(eA1, eA2, eB1, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSumIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] =
    reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  def mkSumIso[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Ref[SumIso[A1, A2, B1, B2]] = {
    new SumIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkSumIso[A1, A2, B1, B2](p: Ref[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[SumIso[A1, A2, B1, B2]](p).iso1, asRep[SumIso[A1, A2, B1, B2]](p).iso2))
    case _ =>
      None
  }
} // of object SumIso
  registerEntityObject("SumIso", SumIso)

object ComposeIso extends EntityObject("ComposeIso") {
  case class ComposeIsoCtor[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])
    extends ComposeIso[A, B, C](iso2, iso1) with Def[ComposeIso[A, B, C]] {
    implicit lazy val eA = iso1.eFrom;
implicit lazy val eB = iso2.eFrom;
implicit lazy val eC = iso2.eTo

    lazy val resultType = element[ComposeIso[A, B, C]]
    override def transform(t: Transformer) = ComposeIsoCtor[A, B, C](t(iso2), t(iso1))
  }
  // elem for concrete class
  class ComposeIsoElem[A, B, C](val iso: Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends IsoURElem[A, C, ComposeIso[A, B, C]]
    with ConcreteElem[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[C]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }

  // state representation type
  type ComposeIsoData[A, B, C] = (IsoUR[B, C], IsoUR[A, B])

  // 3) Iso for concrete class
  class ComposeIsoIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends EntityIso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] with Def[ComposeIsoIso[A, B, C]] {
    override def transform(t: Transformer) = new ComposeIsoIso[A, B, C]()(eA, eB, eC)
    private lazy val _safeFrom = fun { p: Ref[ComposeIso[A, B, C]] => (p.iso2, p.iso1) }
    override def from(p: Ref[ComposeIso[A, B, C]]) =
      tryConvert[ComposeIso[A, B, C], (IsoUR[B, C], IsoUR[A, B])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(IsoUR[B, C], IsoUR[A, B])]) = {
      val Pair(iso2, iso1) = p
      RComposeIso(iso2, iso1)
    }
    lazy val eFrom = pairElement(element[IsoUR[B, C]], element[IsoUR[A, B]])
    lazy val eTo = new ComposeIsoElem[A, B, C](self)
    lazy val resultType = new ComposeIsoIsoElem[A, B, C](eA, eB, eC)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
    }
  }
  case class ComposeIsoIsoElem[A, B, C](eA: Elem[A], eB: Elem[B], eC: Elem[C]) extends Elem[ComposeIsoIso[A, B, C]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ComposeIsoCompanionCtor extends CompanionDef[ComposeIsoCompanionCtor] {
    def resultType = ComposeIsoCompanionElem
    override def toString = "ComposeIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Ref[ComposeIsoData[A, B, C]]): Ref[ComposeIso[A, B, C]] = {
      implicit val eA = p._2.eFrom;
implicit val eB = p._1.eFrom;
implicit val eC = p._1.eTo
      isoComposeIso[A, B, C].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Ref[ComposeIso[A, B, C]] =
      mkComposeIso(iso2, iso1)

    def unapply[A, B, C](p: Ref[IsoUR[A, C]]) = unmkComposeIso(p)
  }
  lazy val ComposeIsoRef: Ref[ComposeIsoCompanionCtor] = new ComposeIsoCompanionCtor
  lazy val RComposeIso: ComposeIsoCompanionCtor = unrefComposeIsoCompanion(ComposeIsoRef)
  implicit def unrefComposeIsoCompanion(p: Ref[ComposeIsoCompanionCtor]): ComposeIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ComposeIsoCompanionCtor])
      p.rhs.asInstanceOf[ComposeIsoCompanionCtor]
    else
      unrefDelegate[ComposeIsoCompanionCtor](p)
  }

  implicit case object ComposeIsoCompanionElem extends CompanionElem[ComposeIsoCompanionCtor]

  implicit def unrefComposeIso[A, B, C](p: Ref[ComposeIso[A, B, C]]): ComposeIso[A, B, C] = {
    if (p.rhs.isInstanceOf[ComposeIso[A, B, C]@unchecked])
      p.rhs.asInstanceOf[ComposeIso[A, B, C]]
    else
      unrefDelegate[ComposeIso[A, B, C]](p)
  }

  implicit class ExtendedComposeIso[A, B, C](p: Ref[ComposeIso[A, B, C]]) {
    def toData: Ref[ComposeIsoData[A, B, C]] = {
      implicit val eA = p.iso1.eFrom;
implicit val eB = p.iso2.eFrom;
implicit val eC = p.iso2.eTo
      isoComposeIso(eA, eB, eC).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] =
    reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))

  def mkComposeIso[A, B, C]
    (iso2: Iso[B, C], iso1: Iso[A, B]): Ref[ComposeIso[A, B, C]] = {
    new ComposeIsoCtor[A, B, C](iso2, iso1)
  }
  def unmkComposeIso[A, B, C](p: Ref[IsoUR[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIsoElem[A, B, C] @unchecked =>
      Some((asRep[ComposeIso[A, B, C]](p).iso2, asRep[ComposeIso[A, B, C]](p).iso1))
    case _ =>
      None
  }
} // of object ComposeIso
  registerEntityObject("ComposeIso", ComposeIso)

object FuncIso extends EntityObject("FuncIso") {
  case class FuncIsoCtor[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])
    extends FuncIso[A, B, C, D](iso1, iso2) with Def[FuncIso[A, B, C, D]] {
    implicit lazy val eA = iso1.eFrom;
implicit lazy val eB = iso1.eTo;
implicit lazy val eC = iso2.eFrom;
implicit lazy val eD = iso2.eTo
    override lazy val eFrom: Elem[A => C] = implicitly[Elem[A => C]]
override lazy val eTo: Elem[B => D] = implicitly[Elem[B => D]]
    lazy val resultType = element[FuncIso[A, B, C, D]]
    override def transform(t: Transformer) = FuncIsoCtor[A, B, C, D](t(iso1), t(iso2))
  }
  // elem for concrete class
  class FuncIsoElem[A, B, C, D](val iso: Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends IsoURElem[A => C, B => D, FuncIso[A, B, C, D]]
    with ConcreteElem[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))
  }

  // state representation type
  type FuncIsoData[A, B, C, D] = (IsoUR[A, B], IsoUR[C, D])

  // 3) Iso for concrete class
  class FuncIsoIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends EntityIso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] with Def[FuncIsoIso[A, B, C, D]] {
    override def transform(t: Transformer) = new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD)
    private lazy val _safeFrom = fun { p: Ref[FuncIso[A, B, C, D]] => (p.iso1, p.iso2) }
    override def from(p: Ref[FuncIso[A, B, C, D]]) =
      tryConvert[FuncIso[A, B, C, D], (IsoUR[A, B], IsoUR[C, D])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(IsoUR[A, B], IsoUR[C, D])]) = {
      val Pair(iso1, iso2) = p
      RFuncIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A, B]], element[IsoUR[C, D]])
    lazy val eTo = new FuncIsoElem[A, B, C, D](self)
    lazy val resultType = new FuncIsoIsoElem[A, B, C, D](eA, eB, eC, eD)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
      case 3 => eD
    }
  }
  case class FuncIsoIsoElem[A, B, C, D](eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) extends Elem[FuncIsoIso[A, B, C, D]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class FuncIsoCompanionCtor extends CompanionDef[FuncIsoCompanionCtor] {
    def resultType = FuncIsoCompanionElem
    override def toString = "FuncIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C, D](p: Ref[FuncIsoData[A, B, C, D]]): Ref[FuncIso[A, B, C, D]] = {
      implicit val eA = p._1.eFrom;
implicit val eB = p._1.eTo;
implicit val eC = p._2.eFrom;
implicit val eD = p._2.eTo
      isoFuncIso[A, B, C, D].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Ref[FuncIso[A, B, C, D]] =
      mkFuncIso(iso1, iso2)

    def unapply[A, B, C, D](p: Ref[IsoUR[A => C, B => D]]) = unmkFuncIso(p)
  }
  lazy val FuncIsoRef: Ref[FuncIsoCompanionCtor] = new FuncIsoCompanionCtor
  lazy val RFuncIso: FuncIsoCompanionCtor = unrefFuncIsoCompanion(FuncIsoRef)
  implicit def unrefFuncIsoCompanion(p: Ref[FuncIsoCompanionCtor]): FuncIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[FuncIsoCompanionCtor])
      p.rhs.asInstanceOf[FuncIsoCompanionCtor]
    else
      unrefDelegate[FuncIsoCompanionCtor](p)
  }

  implicit case object FuncIsoCompanionElem extends CompanionElem[FuncIsoCompanionCtor]

  implicit def unrefFuncIso[A, B, C, D](p: Ref[FuncIso[A, B, C, D]]): FuncIso[A, B, C, D] = {
    if (p.rhs.isInstanceOf[FuncIso[A, B, C, D]@unchecked])
      p.rhs.asInstanceOf[FuncIso[A, B, C, D]]
    else
      unrefDelegate[FuncIso[A, B, C, D]](p)
  }

  implicit class ExtendedFuncIso[A, B, C, D](p: Ref[FuncIso[A, B, C, D]]) {
    def toData: Ref[FuncIsoData[A, B, C, D]] = {
      implicit val eA = p.iso1.eFrom;
implicit val eB = p.iso1.eTo;
implicit val eC = p.iso2.eFrom;
implicit val eD = p.iso2.eTo
      isoFuncIso(eA, eB, eC, eD).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoFuncIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] =
    reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))

  def mkFuncIso[A, B, C, D]
    (iso1: Iso[A, B], iso2: Iso[C, D]): Ref[FuncIso[A, B, C, D]] = {
    new FuncIsoCtor[A, B, C, D](iso1, iso2)
  }
  def unmkFuncIso[A, B, C, D](p: Ref[IsoUR[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIsoElem[A, B, C, D] @unchecked =>
      Some((asRep[FuncIso[A, B, C, D]](p).iso1, asRep[FuncIso[A, B, C, D]](p).iso2))
    case _ =>
      None
  }
} // of object FuncIso
  registerEntityObject("FuncIso", FuncIso)

object ConverterIso extends EntityObject("ConverterIso") {
  case class ConverterIsoCtor[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])
    extends ConverterIso[A, B](convTo, convFrom) with Def[ConverterIso[A, B]] {
    implicit lazy val eA = convTo.eT;
implicit lazy val eB = convTo.eR

    lazy val resultType = element[ConverterIso[A, B]]
    override def transform(t: Transformer) = ConverterIsoCtor[A, B](t(convTo), t(convFrom))
  }
  // elem for concrete class
  class ConverterIsoElem[A, B](val iso: Iso[ConverterIsoData[A, B], ConverterIso[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IsoURElem[A, B, ConverterIso[A, B]]
    with ConcreteElem[ConverterIsoData[A, B], ConverterIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[B]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }

  // state representation type
  type ConverterIsoData[A, B] = (Converter[A, B], Converter[B, A])

  // 3) Iso for concrete class
  class ConverterIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ConverterIsoData[A, B], ConverterIso[A, B]] with Def[ConverterIsoIso[A, B]] {
    override def transform(t: Transformer) = new ConverterIsoIso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Ref[ConverterIso[A, B]] => (p.convTo, p.convFrom) }
    override def from(p: Ref[ConverterIso[A, B]]) =
      tryConvert[ConverterIso[A, B], (Converter[A, B], Converter[B, A])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Converter[A, B], Converter[B, A])]) = {
      val Pair(convTo, convFrom) = p
      RConverterIso(convTo, convFrom)
    }
    lazy val eFrom = pairElement(element[Converter[A, B]], element[Converter[B, A]])
    lazy val eTo = new ConverterIsoElem[A, B](self)
    lazy val resultType = new ConverterIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ConverterIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ConverterIsoIso[A, B]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ConverterIsoCompanionCtor extends CompanionDef[ConverterIsoCompanionCtor] {
    def resultType = ConverterIsoCompanionElem
    override def toString = "ConverterIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Ref[ConverterIsoData[A, B]]): Ref[ConverterIso[A, B]] = {
      implicit val eA = p._1.eT;
implicit val eB = p._1.eR
      isoConverterIso[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A]): Ref[ConverterIso[A, B]] =
      mkConverterIso(convTo, convFrom)

    def unapply[A, B](p: Ref[IsoUR[A, B]]) = unmkConverterIso(p)
  }
  lazy val ConverterIsoRef: Ref[ConverterIsoCompanionCtor] = new ConverterIsoCompanionCtor
  lazy val RConverterIso: ConverterIsoCompanionCtor = unrefConverterIsoCompanion(ConverterIsoRef)
  implicit def unrefConverterIsoCompanion(p: Ref[ConverterIsoCompanionCtor]): ConverterIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ConverterIsoCompanionCtor])
      p.rhs.asInstanceOf[ConverterIsoCompanionCtor]
    else
      unrefDelegate[ConverterIsoCompanionCtor](p)
  }

  implicit case object ConverterIsoCompanionElem extends CompanionElem[ConverterIsoCompanionCtor]

  implicit def unrefConverterIso[A, B](p: Ref[ConverterIso[A, B]]): ConverterIso[A, B] = {
    if (p.rhs.isInstanceOf[ConverterIso[A, B]@unchecked])
      p.rhs.asInstanceOf[ConverterIso[A, B]]
    else
      unrefDelegate[ConverterIso[A, B]](p)
  }

  implicit class ExtendedConverterIso[A, B](p: Ref[ConverterIso[A, B]]) {
    def toData: Ref[ConverterIsoData[A, B]] = {
      implicit val eA = p.convTo.eT;
implicit val eB = p.convTo.eR
      isoConverterIso(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ConverterIsoData[A, B], ConverterIso[A, B]] =
    reifyObject(new ConverterIsoIso[A, B]()(eA, eB))

  def mkConverterIso[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A]): Ref[ConverterIso[A, B]] = {
    new ConverterIsoCtor[A, B](convTo, convFrom)
  }
  def unmkConverterIso[A, B](p: Ref[IsoUR[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConverterIsoElem[A, B] @unchecked =>
      Some((asRep[ConverterIso[A, B]](p).convTo, asRep[ConverterIso[A, B]](p).convFrom))
    case _ =>
      None
  }
} // of object ConverterIso
  registerEntityObject("ConverterIso", ConverterIso)

object ThunkIso extends EntityObject("ThunkIso") {
  case class ThunkIsoCtor[A, B]
      (override val innerIso: Iso[A, B])
    extends ThunkIso[A, B](innerIso) with Def[ThunkIso[A, B]] {
    implicit override lazy val eA = innerIso.eFrom;
implicit override lazy val eB = innerIso.eTo

    lazy val resultType = element[ThunkIso[A, B]]
    override def transform(t: Transformer) = ThunkIsoCtor[A, B](t(innerIso))
  }
  // elem for concrete class
  class ThunkIsoElem[A, B](val iso: Iso[ThunkIsoData[A, B], ThunkIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, Thunk, ThunkIso[A, B]]
    with ConcreteElem[ThunkIsoData[A, B], ThunkIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Thunk]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }

  // state representation type
  type ThunkIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ThunkIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ThunkIsoData[A, B], ThunkIso[A, B]] with Def[ThunkIsoIso[A, B]] {
    override def transform(t: Transformer) = new ThunkIsoIso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Ref[ThunkIso[A, B]] => p.innerIso }
    override def from(p: Ref[ThunkIso[A, B]]) =
      tryConvert[ThunkIso[A, B], IsoUR[A, B]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[IsoUR[A, B]]) = {
      val innerIso = p
      RThunkIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ThunkIsoElem[A, B](self)
    lazy val resultType = new ThunkIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ThunkIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ThunkIsoIso[A, B]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ThunkIsoCompanionCtor extends CompanionDef[ThunkIsoCompanionCtor] {
    def resultType = ThunkIsoCompanionElem
    override def toString = "ThunkIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B]): Ref[ThunkIso[A, B]] =
      mkThunkIso(innerIso)

    def unapply[A, B](p: Ref[Iso1UR[A, B, Thunk]]) = unmkThunkIso(p)
  }
  lazy val ThunkIsoRef: Ref[ThunkIsoCompanionCtor] = new ThunkIsoCompanionCtor
  lazy val RThunkIso: ThunkIsoCompanionCtor = unrefThunkIsoCompanion(ThunkIsoRef)
  implicit def unrefThunkIsoCompanion(p: Ref[ThunkIsoCompanionCtor]): ThunkIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ThunkIsoCompanionCtor])
      p.rhs.asInstanceOf[ThunkIsoCompanionCtor]
    else
      unrefDelegate[ThunkIsoCompanionCtor](p)
  }

  implicit case object ThunkIsoCompanionElem extends CompanionElem[ThunkIsoCompanionCtor]

  implicit def unrefThunkIso[A, B](p: Ref[ThunkIso[A, B]]): ThunkIso[A, B] = {
    if (p.rhs.isInstanceOf[ThunkIso[A, B]@unchecked])
      p.rhs.asInstanceOf[ThunkIso[A, B]]
    else
      unrefDelegate[ThunkIso[A, B]](p)
  }

  implicit class ExtendedThunkIso[A, B](p: Ref[ThunkIso[A, B]]) {
    def toData: Ref[ThunkIsoData[A, B]] = {
      implicit val eA = p.innerIso.eFrom;
implicit val eB = p.innerIso.eTo
      isoThunkIso(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoThunkIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ThunkIsoData[A, B], ThunkIso[A, B]] =
    reifyObject(new ThunkIsoIso[A, B]()(eA, eB))

  def mkThunkIso[A, B]
    (innerIso: Iso[A, B]): Ref[ThunkIso[A, B]] = {
    new ThunkIsoCtor[A, B](innerIso)
  }
  def unmkThunkIso[A, B](p: Ref[Iso1UR[A, B, Thunk]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ThunkIsoElem[A, B] @unchecked =>
      Some((asRep[ThunkIso[A, B]](p).innerIso))
    case _ =>
      None
  }
} // of object ThunkIso
  registerEntityObject("ThunkIso", ThunkIso)

  registerModule(ViewsModule)
}

object ViewsModule extends scalan.ModuleInfo("scalan", "Views")
}

