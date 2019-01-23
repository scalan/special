package scalan

import java.lang.reflect.Method
import scala.language.higherKinds
import scala.collection.mutable.{Map=>MutMap}
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

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
  // entityAdapter for IsoUR trait
  case class IsoURAdapter[From, To](source: Rep[IsoUR[From, To]])
      extends IsoUR[From, To] with Def[IsoUR[From, To]] {
    implicit lazy val eFrom = source.elem.typeArgs("From")._1.asElem[From];
implicit lazy val eTo = source.elem.typeArgs("To")._1.asElem[To]

    val selfType: Elem[IsoUR[From, To]] = element[IsoUR[From, To]]
    override def transform(t: Transformer) = IsoURAdapter[From, To](t(source))
    private val thisClass = classOf[IsoUR[From, To]]

    def from(p: Rep[To]): Rep[From] = {
      asRep[From](mkMethodCall(source,
        thisClass.getMethod("from", classOf[Sym]),
        List(p),
        true, true, element[From]))
    }

    def to(p: Rep[From]): Rep[To] = {
      asRep[To](mkMethodCall(source,
        thisClass.getMethod("to", classOf[Sym]),
        List(p),
        true, true, element[To]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyIsoUR[From, To](p: Rep[IsoUR[From, To]]): IsoUR[From, To] = {
    if (p.rhs.isInstanceOf[IsoUR[From, To]@unchecked]) p.rhs.asInstanceOf[IsoUR[From, To]]
    else
      IsoURAdapter(p)
  }

  // familyElem
  class IsoURElem[From, To, To0 <: IsoUR[From, To]](implicit _eFrom: Elem[From], _eTo: Elem[To])
    extends EntityElem[To0] {
    def eFrom = _eFrom
    def eTo = _eTo

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("From" -> (eFrom -> scalan.util.Invariant), "To" -> (eTo -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagFrom = eFrom.tag
      implicit val tagTo = eTo.tag
      weakTypeTag[IsoUR[From, To]].asInstanceOf[WeakTypeTag[To0]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[IsoUR[From, To]] => convertIsoUR(x) }
      tryConvert(element[IsoUR[From, To]], this, x, conv)
    }

    def convertIsoUR(x: Rep[IsoUR[From, To]]): Rep[To0] = {
      x.elem match {
        case _: IsoURElem[_, _, _] => asRep[To0](x)
        case e => !!!(s"Expected $x to have IsoURElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To0] = ???
  }

  implicit def isoURElement[From, To](implicit eFrom: Elem[From], eTo: Elem[To]): Elem[IsoUR[From, To]] =
    cachedElem[IsoURElem[From, To, IsoUR[From, To]]](eFrom, eTo)

  implicit case object IsoURCompanionElem extends CompanionElem[IsoURCompanionCtor] {
    lazy val tag = weakTypeTag[IsoURCompanionCtor]
    protected def getDefaultRep = RIsoUR
  }

  abstract class IsoURCompanionCtor extends CompanionDef[IsoURCompanionCtor] {
    def selfType = IsoURCompanionElem
    override def toString = "IsoUR"
  }
  implicit def proxyIsoURCompanionCtor(p: Rep[IsoURCompanionCtor]): IsoURCompanionCtor =
    proxyOps[IsoURCompanionCtor](p)

  lazy val RIsoUR: Rep[IsoURCompanionCtor] = new IsoURCompanionCtor {
  }

  object IsoURMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IsoURElem[_, _, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IsoUR[From, To]], Rep[To]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IsoURElem[_, _, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IsoUR[From, To]], Rep[From]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method toString

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals

    // WARNING: Cannot generate matcher for method `isIdentity`: Method's return type Boolean is not a Rep
  }
} // of object IsoUR
  registerEntityObject("IsoUR", IsoUR)

object Iso1UR extends EntityObject("Iso1UR") {
  // entityAdapter for Iso1UR trait
  case class Iso1URAdapter[A, B, C[_]](source: Rep[Iso1UR[A, B, C]])
      extends Iso1UR[A, B, C] with Def[Iso1UR[A, B, C]] {
    implicit override lazy val eA = source.elem.typeArgs("A")._1.asElem[A];
implicit override lazy val eB = source.elem.typeArgs("B")._1.asElem[B];
implicit lazy val cC = source.elem.typeArgs("C")._1.asCont[C]

    val selfType: Elem[Iso1UR[A, B, C]] = element[Iso1UR[A, B, C]]
    override def transform(t: Transformer) = Iso1URAdapter[A, B, C](t(source))
    private val thisClass = classOf[Iso1UR[A, B, C]]

    def innerIso: Iso[A, B] = {
      asRep[IsoUR[A, B]](mkMethodCall(source,
        thisClass.getMethod("innerIso"),
        List(),
        true, true, element[IsoUR[A, B]]))
    }

    def from(p: Rep[C[B]]): Rep[C[A]] = {
      asRep[C[A]](mkMethodCall(source,
        thisClass.getMethod("from", classOf[Sym]),
        List(p),
        true, true, element[C[A]]))
    }

    def to(p: Rep[C[A]]): Rep[C[B]] = {
      asRep[C[B]](mkMethodCall(source,
        thisClass.getMethod("to", classOf[Sym]),
        List(p),
        true, true, element[C[B]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyIso1UR[A, B, C[_]](p: Rep[Iso1UR[A, B, C]]): Iso1UR[A, B, C] = {
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
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[Iso1UR[A, B, C]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Iso1UR[A, B, C]] => convertIso1UR(x) }
      tryConvert(element[Iso1UR[A, B, C]], this, x, conv)
    }

    def convertIso1UR(x: Rep[Iso1UR[A, B, C]]): Rep[To] = {
      x.elem.asInstanceOf[Elem[_]] match {
        case _: Iso1URElem[_, _, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have Iso1URElem[_, _, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iso1URElement[A, B, C[_]](implicit eA: Elem[A], eB: Elem[B], cC: Cont[C]): Elem[Iso1UR[A, B, C]] =
    cachedElem[Iso1URElem[A, B, C, Iso1UR[A, B, C]]](eA, eB, cC)

  implicit case object Iso1URCompanionElem extends CompanionElem[Iso1URCompanionCtor] {
    lazy val tag = weakTypeTag[Iso1URCompanionCtor]
    protected def getDefaultRep = RIso1UR
  }

  abstract class Iso1URCompanionCtor extends CompanionDef[Iso1URCompanionCtor] {
    def selfType = Iso1URCompanionElem
    override def toString = "Iso1UR"
  }
  implicit def proxyIso1URCompanionCtor(p: Rep[Iso1URCompanionCtor]): Iso1URCompanionCtor =
    proxyOps[Iso1URCompanionCtor](p)

  lazy val RIso1UR: Rep[Iso1URCompanionCtor] = new Iso1URCompanionCtor {
  }

  object Iso1URMethods {
    object innerIso {
      def unapply(d: Def[_]): Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: Iso1URElem[_, _, _, _] => true; case _ => false }) && method.getName == "innerIso" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Elem[_]] match { case _: Iso1URElem[_, _, _, _] => true; case _ => false }) && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Iso1UR[A, B, C]] forSome {type A; type B; type C[_]}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
  }
} // of object Iso1UR
  registerEntityObject("Iso1UR", Iso1UR)

object IdentityIso extends EntityObject("IdentityIso") {
  case class IdentityIsoCtor[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso[A]() with Def[IdentityIso[A]] {
    lazy val selfType = element[IdentityIso[A]]
    override def transform(t: Transformer) = IdentityIsoCtor[A]()(eA)
  }
  // elem for concrete class
  class IdentityIsoElem[A](val iso: Iso[IdentityIsoData[A], IdentityIso[A]])(implicit val eA: Elem[A])
    extends IsoURElem[A, A, IdentityIso[A]]
    with ConcreteElem[IdentityIsoData[A], IdentityIso[A]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A, A]]) = RIdentityIso()
    override def getDefaultRep = RIdentityIso()
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIso[A]]
    }
  }

  // state representation type
  type IdentityIsoData[A] = Unit

  // 3) Iso for concrete class
  class IdentityIsoIso[A](implicit eA: Elem[A])
    extends EntityIso[IdentityIsoData[A], IdentityIso[A]] with Def[IdentityIsoIso[A]] {
    override def transform(t: Transformer) = new IdentityIsoIso[A]()(eA)
    private lazy val _safeFrom = fun { p: Rep[IdentityIso[A]] => () }
    override def from(p: Rep[IdentityIso[A]]) =
      tryConvert[IdentityIso[A], Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RIdentityIso()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityIsoElem[A](self)
    lazy val selfType = new IdentityIsoIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class IdentityIsoIsoElem[A](eA: Elem[A]) extends Elem[IdentityIsoIso[A]] {
    def getDefaultRep = reifyObject(new IdentityIsoIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIsoIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IdentityIsoCompanionCtor extends CompanionDef[IdentityIsoCompanionCtor] {
    def selfType = IdentityIsoCompanionElem
    override def toString = "IdentityIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[IdentityIsoData[A]])(implicit eA: Elem[A]): Rep[IdentityIso[A]] = {
      isoIdentityIso[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityIso[A]] =
      mkIdentityIso()

    def unapply[A](p: Rep[IsoUR[A, A]]) = unmkIdentityIso(p)
  }
  lazy val IdentityIsoRep: Rep[IdentityIsoCompanionCtor] = new IdentityIsoCompanionCtor
  lazy val RIdentityIso: IdentityIsoCompanionCtor = proxyIdentityIsoCompanion(IdentityIsoRep)
  implicit def proxyIdentityIsoCompanion(p: Rep[IdentityIsoCompanionCtor]): IdentityIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[IdentityIsoCompanionCtor])
      p.rhs.asInstanceOf[IdentityIsoCompanionCtor]
    else
      proxyOps[IdentityIsoCompanionCtor](p)
  }

  implicit case object IdentityIsoCompanionElem extends CompanionElem[IdentityIsoCompanionCtor] {
    lazy val tag = weakTypeTag[IdentityIsoCompanionCtor]
    protected def getDefaultRep = IdentityIsoRep
  }

  implicit def proxyIdentityIso[A](p: Rep[IdentityIso[A]]): IdentityIso[A] =
    proxyOps[IdentityIso[A]](p)

  implicit class ExtendedIdentityIso[A](p: Rep[IdentityIso[A]])(implicit eA: Elem[A]) {
    def toData: Rep[IdentityIsoData[A]] = {
      isoIdentityIso(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityIso[A](implicit eA: Elem[A]): Iso[IdentityIsoData[A], IdentityIso[A]] =
    reifyObject(new IdentityIsoIso[A]()(eA))

  def mkIdentityIso[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso[A]] = {
    new IdentityIsoCtor[A]()
  }
  def unmkIdentityIso[A](p: Rep[IsoUR[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityIsoElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }

    object IdentityIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[IdentityIso[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[IdentityIso[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IdentityIsoElem[_]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[IdentityIso[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[IdentityIso[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
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
    lazy val selfType = element[PairIso[A1, A2, B1, B2]]
    override def transform(t: Transformer) = PairIsoCtor[A1, A2, B1, B2](t(iso1), t(iso2))
  }
  // elem for concrete class
  class PairIsoElem[A1, A2, B1, B2](val iso: Iso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[(A1, A2), (B1, B2), PairIso[A1, A2, B1, B2]]
    with ConcreteElem[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to PairIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = RPairIso(element[IsoUR[A1, B1]].defaultRepValue, element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIso[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type PairIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class PairIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[PairIsoData[A1, A2, B1, B2], PairIso[A1, A2, B1, B2]] with Def[PairIsoIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Rep[PairIso[A1, A2, B1, B2]] => (p.iso1, p.iso2) }
    override def from(p: Rep[PairIso[A1, A2, B1, B2]]) =
      tryConvert[PairIso[A1, A2, B1, B2], (IsoUR[A1, B1], IsoUR[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      RPairIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new PairIsoElem[A1, A2, B1, B2](self)
    lazy val selfType = new PairIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class PairIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[PairIsoIso[A1, A2, B1, B2]] {
    def getDefaultRep = reifyObject(new PairIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIsoIso[A1, A2, B1, B2]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairIsoCompanionCtor extends CompanionDef[PairIsoCompanionCtor] with PairIsoCompanion {
    def selfType = PairIsoCompanionElem
    override def toString = "PairIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[PairIsoData[A1, A2, B1, B2]]): Rep[PairIso[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eFrom;
implicit val eA2 = p._2.eFrom;
implicit val eB1 = p._1.eTo;
implicit val eB2 = p._2.eTo
      isoPairIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Rep[PairIso[A1, A2, B1, B2]] =
      mkPairIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = unmkPairIso(p)
  }
  lazy val PairIsoRep: Rep[PairIsoCompanionCtor] = new PairIsoCompanionCtor
  lazy val RPairIso: PairIsoCompanionCtor = proxyPairIsoCompanion(PairIsoRep)
  implicit def proxyPairIsoCompanion(p: Rep[PairIsoCompanionCtor]): PairIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[PairIsoCompanionCtor])
      p.rhs.asInstanceOf[PairIsoCompanionCtor]
    else
      proxyOps[PairIsoCompanionCtor](p)
  }

  implicit case object PairIsoCompanionElem extends CompanionElem[PairIsoCompanionCtor] {
    lazy val tag = weakTypeTag[PairIsoCompanionCtor]
    protected def getDefaultRep = PairIsoRep
  }

  implicit def proxyPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]]): PairIso[A1, A2, B1, B2] =
    proxyOps[PairIso[A1, A2, B1, B2]](p)

  implicit class ExtendedPairIso[A1, A2, B1, B2](p: Rep[PairIso[A1, A2, B1, B2]]) {
    def toData: Rep[PairIsoData[A1, A2, B1, B2]] = {
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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Rep[PairIso[A1, A2, B1, B2]] = {
    new PairIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkPairIso[A1, A2, B1, B2](p: Rep[IsoUR[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[PairIso[A1, A2, B1, B2]](p).iso1, asRep[PairIso[A1, A2, B1, B2]](p).iso2))
    case _ =>
      None
  }

    object PairIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairIso[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
  }

  object PairIsoCompanionMethods {
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
    lazy val selfType = element[AbsorbFirstUnitIso[A2, B2]]
    override def transform(t: Transformer) = AbsorbFirstUnitIsoCtor[A2, B2](t(iso2))
  }
  // elem for concrete class
  class AbsorbFirstUnitIsoElem[A2, B2](val iso: Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]])(implicit val eA2: Elem[A2], val eB2: Elem[B2])
    extends IsoURElem[A2, (Unit, B2), AbsorbFirstUnitIso[A2, B2]]
    with ConcreteElem[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A2], pairElement(UnitElement,element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A2, (Unit, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to AbsorbFirstUnitIso: missing fields List(iso2)")
    override def getDefaultRep = RAbsorbFirstUnitIso(element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA2 = eA2.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[AbsorbFirstUnitIso[A2, B2]]
    }
  }

  // state representation type
  type AbsorbFirstUnitIsoData[A2, B2] = IsoUR[A2, B2]

  // 3) Iso for concrete class
  class AbsorbFirstUnitIsoIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2])
    extends EntityIso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] with Def[AbsorbFirstUnitIsoIso[A2, B2]] {
    override def transform(t: Transformer) = new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2)
    private lazy val _safeFrom = fun { p: Rep[AbsorbFirstUnitIso[A2, B2]] => p.iso2 }
    override def from(p: Rep[AbsorbFirstUnitIso[A2, B2]]) =
      tryConvert[AbsorbFirstUnitIso[A2, B2], IsoUR[A2, B2]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[IsoUR[A2, B2]]) = {
      val iso2 = p
      RAbsorbFirstUnitIso(iso2)
    }
    lazy val eFrom = element[IsoUR[A2, B2]]
    lazy val eTo = new AbsorbFirstUnitIsoElem[A2, B2](self)
    lazy val selfType = new AbsorbFirstUnitIsoIsoElem[A2, B2](eA2, eB2)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA2
      case 1 => eB2
    }
  }
  case class AbsorbFirstUnitIsoIsoElem[A2, B2](eA2: Elem[A2], eB2: Elem[B2]) extends Elem[AbsorbFirstUnitIsoIso[A2, B2]] {
    def getDefaultRep = reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))
    lazy val tag = {
      implicit val tagA2 = eA2.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[AbsorbFirstUnitIsoIso[A2, B2]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A2" -> (eA2 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbFirstUnitIsoCompanionCtor extends CompanionDef[AbsorbFirstUnitIsoCompanionCtor] {
    def selfType = AbsorbFirstUnitIsoCompanionElem
    override def toString = "AbsorbFirstUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A2, B2](iso2: Iso[A2, B2]): Rep[AbsorbFirstUnitIso[A2, B2]] =
      mkAbsorbFirstUnitIso(iso2)

    def unapply[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = unmkAbsorbFirstUnitIso(p)
  }
  lazy val AbsorbFirstUnitIsoRep: Rep[AbsorbFirstUnitIsoCompanionCtor] = new AbsorbFirstUnitIsoCompanionCtor
  lazy val RAbsorbFirstUnitIso: AbsorbFirstUnitIsoCompanionCtor = proxyAbsorbFirstUnitIsoCompanion(AbsorbFirstUnitIsoRep)
  implicit def proxyAbsorbFirstUnitIsoCompanion(p: Rep[AbsorbFirstUnitIsoCompanionCtor]): AbsorbFirstUnitIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[AbsorbFirstUnitIsoCompanionCtor])
      p.rhs.asInstanceOf[AbsorbFirstUnitIsoCompanionCtor]
    else
      proxyOps[AbsorbFirstUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbFirstUnitIsoCompanionElem extends CompanionElem[AbsorbFirstUnitIsoCompanionCtor] {
    lazy val tag = weakTypeTag[AbsorbFirstUnitIsoCompanionCtor]
    protected def getDefaultRep = AbsorbFirstUnitIsoRep
  }

  implicit def proxyAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]]): AbsorbFirstUnitIso[A2, B2] =
    proxyOps[AbsorbFirstUnitIso[A2, B2]](p)

  implicit class ExtendedAbsorbFirstUnitIso[A2, B2](p: Rep[AbsorbFirstUnitIso[A2, B2]]) {
    def toData: Rep[AbsorbFirstUnitIsoData[A2, B2]] = {
      implicit val eA2 = p.iso2.eFrom;
implicit val eB2 = p.iso2.eTo
      isoAbsorbFirstUnitIso(eA2, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbFirstUnitIso[A2, B2](implicit eA2: Elem[A2], eB2: Elem[B2]): Iso[AbsorbFirstUnitIsoData[A2, B2], AbsorbFirstUnitIso[A2, B2]] =
    reifyObject(new AbsorbFirstUnitIsoIso[A2, B2]()(eA2, eB2))

  def mkAbsorbFirstUnitIso[A2, B2]
    (iso2: Iso[A2, B2]): Rep[AbsorbFirstUnitIso[A2, B2]] = {
    new AbsorbFirstUnitIsoCtor[A2, B2](iso2)
  }
  def unmkAbsorbFirstUnitIso[A2, B2](p: Rep[IsoUR[A2, (Unit, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbFirstUnitIsoElem[A2, B2] @unchecked =>
      Some((asRep[AbsorbFirstUnitIso[A2, B2]](p).iso2))
    case _ =>
      None
  }

    object AbsorbFirstUnitIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[(Unit, B2)]) forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AbsorbFirstUnitIso[A2, B2]], Rep[A2]) forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbsorbFirstUnitIsoElem[_, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AbsorbFirstUnitIso[A2, B2]] forSome {type A2; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
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
    lazy val selfType = element[AbsorbSecondUnitIso[A1, B1]]
    override def transform(t: Transformer) = AbsorbSecondUnitIsoCtor[A1, B1](t(iso1))
  }
  // elem for concrete class
  class AbsorbSecondUnitIsoElem[A1, B1](val iso: Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]])(implicit val eA1: Elem[A1], val eB1: Elem[B1])
    extends IsoURElem[A1, (B1, Unit), AbsorbSecondUnitIso[A1, B1]]
    with ConcreteElem[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A1], pairElement(element[B1],UnitElement)))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A1, (B1, Unit)]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to AbsorbSecondUnitIso: missing fields List(iso1)")
    override def getDefaultRep = RAbsorbSecondUnitIso(element[IsoUR[A1, B1]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagB1 = eB1.tag
      weakTypeTag[AbsorbSecondUnitIso[A1, B1]]
    }
  }

  // state representation type
  type AbsorbSecondUnitIsoData[A1, B1] = IsoUR[A1, B1]

  // 3) Iso for concrete class
  class AbsorbSecondUnitIsoIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1])
    extends EntityIso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] with Def[AbsorbSecondUnitIsoIso[A1, B1]] {
    override def transform(t: Transformer) = new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1)
    private lazy val _safeFrom = fun { p: Rep[AbsorbSecondUnitIso[A1, B1]] => p.iso1 }
    override def from(p: Rep[AbsorbSecondUnitIso[A1, B1]]) =
      tryConvert[AbsorbSecondUnitIso[A1, B1], IsoUR[A1, B1]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[IsoUR[A1, B1]]) = {
      val iso1 = p
      RAbsorbSecondUnitIso(iso1)
    }
    lazy val eFrom = element[IsoUR[A1, B1]]
    lazy val eTo = new AbsorbSecondUnitIsoElem[A1, B1](self)
    lazy val selfType = new AbsorbSecondUnitIsoIsoElem[A1, B1](eA1, eB1)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eB1
    }
  }
  case class AbsorbSecondUnitIsoIsoElem[A1, B1](eA1: Elem[A1], eB1: Elem[B1]) extends Elem[AbsorbSecondUnitIsoIso[A1, B1]] {
    def getDefaultRep = reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagB1 = eB1.tag
      weakTypeTag[AbsorbSecondUnitIsoIso[A1, B1]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class AbsorbSecondUnitIsoCompanionCtor extends CompanionDef[AbsorbSecondUnitIsoCompanionCtor] {
    def selfType = AbsorbSecondUnitIsoCompanionElem
    override def toString = "AbsorbSecondUnitIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A1, B1](iso1: Iso[A1, B1]): Rep[AbsorbSecondUnitIso[A1, B1]] =
      mkAbsorbSecondUnitIso(iso1)

    def unapply[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = unmkAbsorbSecondUnitIso(p)
  }
  lazy val AbsorbSecondUnitIsoRep: Rep[AbsorbSecondUnitIsoCompanionCtor] = new AbsorbSecondUnitIsoCompanionCtor
  lazy val RAbsorbSecondUnitIso: AbsorbSecondUnitIsoCompanionCtor = proxyAbsorbSecondUnitIsoCompanion(AbsorbSecondUnitIsoRep)
  implicit def proxyAbsorbSecondUnitIsoCompanion(p: Rep[AbsorbSecondUnitIsoCompanionCtor]): AbsorbSecondUnitIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[AbsorbSecondUnitIsoCompanionCtor])
      p.rhs.asInstanceOf[AbsorbSecondUnitIsoCompanionCtor]
    else
      proxyOps[AbsorbSecondUnitIsoCompanionCtor](p)
  }

  implicit case object AbsorbSecondUnitIsoCompanionElem extends CompanionElem[AbsorbSecondUnitIsoCompanionCtor] {
    lazy val tag = weakTypeTag[AbsorbSecondUnitIsoCompanionCtor]
    protected def getDefaultRep = AbsorbSecondUnitIsoRep
  }

  implicit def proxyAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]]): AbsorbSecondUnitIso[A1, B1] =
    proxyOps[AbsorbSecondUnitIso[A1, B1]](p)

  implicit class ExtendedAbsorbSecondUnitIso[A1, B1](p: Rep[AbsorbSecondUnitIso[A1, B1]]) {
    def toData: Rep[AbsorbSecondUnitIsoData[A1, B1]] = {
      implicit val eA1 = p.iso1.eFrom;
implicit val eB1 = p.iso1.eTo
      isoAbsorbSecondUnitIso(eA1, eB1).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoAbsorbSecondUnitIso[A1, B1](implicit eA1: Elem[A1], eB1: Elem[B1]): Iso[AbsorbSecondUnitIsoData[A1, B1], AbsorbSecondUnitIso[A1, B1]] =
    reifyObject(new AbsorbSecondUnitIsoIso[A1, B1]()(eA1, eB1))

  def mkAbsorbSecondUnitIso[A1, B1]
    (iso1: Iso[A1, B1]): Rep[AbsorbSecondUnitIso[A1, B1]] = {
    new AbsorbSecondUnitIsoCtor[A1, B1](iso1)
  }
  def unmkAbsorbSecondUnitIso[A1, B1](p: Rep[IsoUR[A1, (B1, Unit)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: AbsorbSecondUnitIsoElem[A1, B1] @unchecked =>
      Some((asRep[AbsorbSecondUnitIso[A1, B1]](p).iso1))
    case _ =>
      None
  }

    object AbsorbSecondUnitIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[(B1, Unit)]) forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[AbsorbSecondUnitIso[A1, B1]], Rep[A1]) forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AbsorbSecondUnitIsoElem[_, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[AbsorbSecondUnitIso[A1, B1]] forSome {type A1; type B1}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
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
    lazy val selfType = element[SumIso[A1, A2, B1, B2]]
    override def transform(t: Transformer) = SumIsoCtor[A1, A2, B1, B2](t(iso1), t(iso2))
  }
  // elem for concrete class
  class SumIsoElem[A1, A2, B1, B2](val iso: Iso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends IsoURElem[$bar[A1, A2], $bar[B1, B2], SumIso[A1, A2, B1, B2]]
    with ConcreteElem[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to SumIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = RSumIso(element[IsoUR[A1, B1]].defaultRepValue, element[IsoUR[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIso[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type SumIsoData[A1, A2, B1, B2] = (IsoUR[A1, B1], IsoUR[A2, B2])

  // 3) Iso for concrete class
  class SumIsoIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[SumIsoData[A1, A2, B1, B2], SumIso[A1, A2, B1, B2]] with Def[SumIsoIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Rep[SumIso[A1, A2, B1, B2]] => (p.iso1, p.iso2) }
    override def from(p: Rep[SumIso[A1, A2, B1, B2]]) =
      tryConvert[SumIso[A1, A2, B1, B2], (IsoUR[A1, B1], IsoUR[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(IsoUR[A1, B1], IsoUR[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      RSumIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A1, B1]], element[IsoUR[A2, B2]])
    lazy val eTo = new SumIsoElem[A1, A2, B1, B2](self)
    lazy val selfType = new SumIsoIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class SumIsoIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[SumIsoIso[A1, A2, B1, B2]] {
    def getDefaultRep = reifyObject(new SumIsoIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))
    lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIsoIso[A1, A2, B1, B2]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SumIsoCompanionCtor extends CompanionDef[SumIsoCompanionCtor] {
    def selfType = SumIsoCompanionElem
    override def toString = "SumIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Rep[SumIsoData[A1, A2, B1, B2]]): Rep[SumIso[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eFrom;
implicit val eA2 = p._2.eFrom;
implicit val eB1 = p._1.eTo;
implicit val eB2 = p._2.eTo
      isoSumIso[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Rep[SumIso[A1, A2, B1, B2]] =
      mkSumIso(iso1, iso2)

    def unapply[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso(p)
  }
  lazy val SumIsoRep: Rep[SumIsoCompanionCtor] = new SumIsoCompanionCtor
  lazy val RSumIso: SumIsoCompanionCtor = proxySumIsoCompanion(SumIsoRep)
  implicit def proxySumIsoCompanion(p: Rep[SumIsoCompanionCtor]): SumIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[SumIsoCompanionCtor])
      p.rhs.asInstanceOf[SumIsoCompanionCtor]
    else
      proxyOps[SumIsoCompanionCtor](p)
  }

  implicit case object SumIsoCompanionElem extends CompanionElem[SumIsoCompanionCtor] {
    lazy val tag = weakTypeTag[SumIsoCompanionCtor]
    protected def getDefaultRep = SumIsoRep
  }

  implicit def proxySumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]]): SumIso[A1, A2, B1, B2] =
    proxyOps[SumIso[A1, A2, B1, B2]](p)

  implicit class ExtendedSumIso[A1, A2, B1, B2](p: Rep[SumIso[A1, A2, B1, B2]]) {
    def toData: Rep[SumIsoData[A1, A2, B1, B2]] = {
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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Rep[SumIso[A1, A2, B1, B2]] = {
    new SumIsoCtor[A1, A2, B1, B2](iso1, iso2)
  }
  def unmkSumIso[A1, A2, B1, B2](p: Rep[IsoUR[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIsoElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[SumIso[A1, A2, B1, B2]](p).iso1, asRep[SumIso[A1, A2, B1, B2]](p).iso2))
    case _ =>
      None
  }

    object SumIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SumIso[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SumIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SumIso[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
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

    lazy val selfType = element[ComposeIso[A, B, C]]
    override def transform(t: Transformer) = ComposeIsoCtor[A, B, C](t(iso2), t(iso1))
  }
  // elem for concrete class
  class ComposeIsoElem[A, B, C](val iso: Iso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends IsoURElem[A, C, ComposeIso[A, B, C]]
    with ConcreteElem[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[C]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A, C]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to ComposeIso: missing fields List(iso2, iso1)")
    override def getDefaultRep = RComposeIso(element[IsoUR[B, C]].defaultRepValue, element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIso[A, B, C]]
    }
  }

  // state representation type
  type ComposeIsoData[A, B, C] = (IsoUR[B, C], IsoUR[A, B])

  // 3) Iso for concrete class
  class ComposeIsoIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends EntityIso[ComposeIsoData[A, B, C], ComposeIso[A, B, C]] with Def[ComposeIsoIso[A, B, C]] {
    override def transform(t: Transformer) = new ComposeIsoIso[A, B, C]()(eA, eB, eC)
    private lazy val _safeFrom = fun { p: Rep[ComposeIso[A, B, C]] => (p.iso2, p.iso1) }
    override def from(p: Rep[ComposeIso[A, B, C]]) =
      tryConvert[ComposeIso[A, B, C], (IsoUR[B, C], IsoUR[A, B])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(IsoUR[B, C], IsoUR[A, B])]) = {
      val Pair(iso2, iso1) = p
      RComposeIso(iso2, iso1)
    }
    lazy val eFrom = pairElement(element[IsoUR[B, C]], element[IsoUR[A, B]])
    lazy val eTo = new ComposeIsoElem[A, B, C](self)
    lazy val selfType = new ComposeIsoIsoElem[A, B, C](eA, eB, eC)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
    }
  }
  case class ComposeIsoIsoElem[A, B, C](eA: Elem[A], eB: Elem[B], eC: Elem[C]) extends Elem[ComposeIsoIso[A, B, C]] {
    def getDefaultRep = reifyObject(new ComposeIsoIso[A, B, C]()(eA, eB, eC))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIsoIso[A, B, C]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ComposeIsoCompanionCtor extends CompanionDef[ComposeIsoCompanionCtor] {
    def selfType = ComposeIsoCompanionElem
    override def toString = "ComposeIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Rep[ComposeIsoData[A, B, C]]): Rep[ComposeIso[A, B, C]] = {
      implicit val eA = p._2.eFrom;
implicit val eB = p._1.eFrom;
implicit val eC = p._1.eTo
      isoComposeIso[A, B, C].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Rep[ComposeIso[A, B, C]] =
      mkComposeIso(iso2, iso1)

    def unapply[A, B, C](p: Rep[IsoUR[A, C]]) = unmkComposeIso(p)
  }
  lazy val ComposeIsoRep: Rep[ComposeIsoCompanionCtor] = new ComposeIsoCompanionCtor
  lazy val RComposeIso: ComposeIsoCompanionCtor = proxyComposeIsoCompanion(ComposeIsoRep)
  implicit def proxyComposeIsoCompanion(p: Rep[ComposeIsoCompanionCtor]): ComposeIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ComposeIsoCompanionCtor])
      p.rhs.asInstanceOf[ComposeIsoCompanionCtor]
    else
      proxyOps[ComposeIsoCompanionCtor](p)
  }

  implicit case object ComposeIsoCompanionElem extends CompanionElem[ComposeIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ComposeIsoCompanionCtor]
    protected def getDefaultRep = ComposeIsoRep
  }

  implicit def proxyComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]]): ComposeIso[A, B, C] =
    proxyOps[ComposeIso[A, B, C]](p)

  implicit class ExtendedComposeIso[A, B, C](p: Rep[ComposeIso[A, B, C]]) {
    def toData: Rep[ComposeIsoData[A, B, C]] = {
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
    (iso2: Iso[B, C], iso1: Iso[A, B]): Rep[ComposeIso[A, B, C]] = {
    new ComposeIsoCtor[A, B, C](iso2, iso1)
  }
  def unmkComposeIso[A, B, C](p: Rep[IsoUR[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIsoElem[A, B, C] @unchecked =>
      Some((asRep[ComposeIso[A, B, C]](p).iso2, asRep[ComposeIso[A, B, C]](p).iso1))
    case _ =>
      None
  }

    object ComposeIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ComposeIso[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ComposeIso[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ComposeIsoElem[_, _, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ComposeIso[A, B, C]] forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
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
    lazy val selfType = element[FuncIso[A, B, C, D]]
    override def transform(t: Transformer) = FuncIsoCtor[A, B, C, D](t(iso1), t(iso2))
  }
  // elem for concrete class
  class FuncIsoElem[A, B, C, D](val iso: Iso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends IsoURElem[A => C, B => D, FuncIso[A, B, C, D]]
    with ConcreteElem[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A => C, B => D]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to FuncIso: missing fields List(iso1, iso2)")
    override def getDefaultRep = RFuncIso(element[IsoUR[A, B]].defaultRepValue, element[IsoUR[C, D]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIso[A, B, C, D]]
    }
  }

  // state representation type
  type FuncIsoData[A, B, C, D] = (IsoUR[A, B], IsoUR[C, D])

  // 3) Iso for concrete class
  class FuncIsoIso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends EntityIso[FuncIsoData[A, B, C, D], FuncIso[A, B, C, D]] with Def[FuncIsoIso[A, B, C, D]] {
    override def transform(t: Transformer) = new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD)
    private lazy val _safeFrom = fun { p: Rep[FuncIso[A, B, C, D]] => (p.iso1, p.iso2) }
    override def from(p: Rep[FuncIso[A, B, C, D]]) =
      tryConvert[FuncIso[A, B, C, D], (IsoUR[A, B], IsoUR[C, D])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(IsoUR[A, B], IsoUR[C, D])]) = {
      val Pair(iso1, iso2) = p
      RFuncIso(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[IsoUR[A, B]], element[IsoUR[C, D]])
    lazy val eTo = new FuncIsoElem[A, B, C, D](self)
    lazy val selfType = new FuncIsoIsoElem[A, B, C, D](eA, eB, eC, eD)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
      case 3 => eD
    }
  }
  case class FuncIsoIsoElem[A, B, C, D](eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) extends Elem[FuncIsoIso[A, B, C, D]] {
    def getDefaultRep = reifyObject(new FuncIsoIso[A, B, C, D]()(eA, eB, eC, eD))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIsoIso[A, B, C, D]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant), "D" -> (eD -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class FuncIsoCompanionCtor extends CompanionDef[FuncIsoCompanionCtor] {
    def selfType = FuncIsoCompanionElem
    override def toString = "FuncIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C, D](p: Rep[FuncIsoData[A, B, C, D]]): Rep[FuncIso[A, B, C, D]] = {
      implicit val eA = p._1.eFrom;
implicit val eB = p._1.eTo;
implicit val eC = p._2.eFrom;
implicit val eD = p._2.eTo
      isoFuncIso[A, B, C, D].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Rep[FuncIso[A, B, C, D]] =
      mkFuncIso(iso1, iso2)

    def unapply[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = unmkFuncIso(p)
  }
  lazy val FuncIsoRep: Rep[FuncIsoCompanionCtor] = new FuncIsoCompanionCtor
  lazy val RFuncIso: FuncIsoCompanionCtor = proxyFuncIsoCompanion(FuncIsoRep)
  implicit def proxyFuncIsoCompanion(p: Rep[FuncIsoCompanionCtor]): FuncIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[FuncIsoCompanionCtor])
      p.rhs.asInstanceOf[FuncIsoCompanionCtor]
    else
      proxyOps[FuncIsoCompanionCtor](p)
  }

  implicit case object FuncIsoCompanionElem extends CompanionElem[FuncIsoCompanionCtor] {
    lazy val tag = weakTypeTag[FuncIsoCompanionCtor]
    protected def getDefaultRep = FuncIsoRep
  }

  implicit def proxyFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]]): FuncIso[A, B, C, D] =
    proxyOps[FuncIso[A, B, C, D]](p)

  implicit class ExtendedFuncIso[A, B, C, D](p: Rep[FuncIso[A, B, C, D]]) {
    def toData: Rep[FuncIsoData[A, B, C, D]] = {
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
    (iso1: Iso[A, B], iso2: Iso[C, D]): Rep[FuncIso[A, B, C, D]] = {
    new FuncIsoCtor[A, B, C, D](iso1, iso2)
  }
  def unmkFuncIso[A, B, C, D](p: Rep[IsoUR[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIsoElem[A, B, C, D] @unchecked =>
      Some((asRep[FuncIso[A, B, C, D]](p).iso1, asRep[FuncIso[A, B, C, D]](p).iso2))
    case _ =>
      None
  }

    object FuncIsoMethods {
    object from {
      def unapply(d: Def[_]): Nullable[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[FuncIso[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[FuncIso[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[FuncIsoElem[_, _, _, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[FuncIso[A, B, C, D]] forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
  }
} // of object FuncIso
  registerEntityObject("FuncIso", FuncIso)

object ConverterIso extends EntityObject("ConverterIso") {
  case class ConverterIsoCtor[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])
    extends ConverterIso[A, B](convTo, convFrom) with Def[ConverterIso[A, B]] {
    implicit lazy val eA = convTo.eT;
implicit lazy val eB = convTo.eR

    lazy val selfType = element[ConverterIso[A, B]]
    override def transform(t: Transformer) = ConverterIsoCtor[A, B](t(convTo), t(convFrom))
  }
  // elem for concrete class
  class ConverterIsoElem[A, B](val iso: Iso[ConverterIsoData[A, B], ConverterIso[A, B]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends IsoURElem[A, B, ConverterIso[A, B]]
    with ConcreteElem[ConverterIsoData[A, B], ConverterIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(isoURElement(element[A], element[B]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override def convertIsoUR(x: Rep[IsoUR[A, B]]) = // Converter is not generated by meta
!!!("Cannot convert from IsoUR to ConverterIso: missing fields List(convTo, convFrom)")
    override def getDefaultRep = RConverterIso(element[Converter[A, B]].defaultRepValue, element[Converter[B, A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIso[A, B]]
    }
  }

  // state representation type
  type ConverterIsoData[A, B] = (Converter[A, B], Converter[B, A])

  // 3) Iso for concrete class
  class ConverterIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ConverterIsoData[A, B], ConverterIso[A, B]] with Def[ConverterIsoIso[A, B]] {
    override def transform(t: Transformer) = new ConverterIsoIso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Rep[ConverterIso[A, B]] => (p.convTo, p.convFrom) }
    override def from(p: Rep[ConverterIso[A, B]]) =
      tryConvert[ConverterIso[A, B], (Converter[A, B], Converter[B, A])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Converter[A, B], Converter[B, A])]) = {
      val Pair(convTo, convFrom) = p
      RConverterIso(convTo, convFrom)
    }
    lazy val eFrom = pairElement(element[Converter[A, B]], element[Converter[B, A]])
    lazy val eTo = new ConverterIsoElem[A, B](self)
    lazy val selfType = new ConverterIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ConverterIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ConverterIsoIso[A, B]] {
    def getDefaultRep = reifyObject(new ConverterIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIsoIso[A, B]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ConverterIsoCompanionCtor extends CompanionDef[ConverterIsoCompanionCtor] {
    def selfType = ConverterIsoCompanionElem
    override def toString = "ConverterIsoCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[ConverterIsoData[A, B]]): Rep[ConverterIso[A, B]] = {
      implicit val eA = p._1.eT;
implicit val eB = p._1.eR
      isoConverterIso[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A]): Rep[ConverterIso[A, B]] =
      mkConverterIso(convTo, convFrom)

    def unapply[A, B](p: Rep[IsoUR[A, B]]) = unmkConverterIso(p)
  }
  lazy val ConverterIsoRep: Rep[ConverterIsoCompanionCtor] = new ConverterIsoCompanionCtor
  lazy val RConverterIso: ConverterIsoCompanionCtor = proxyConverterIsoCompanion(ConverterIsoRep)
  implicit def proxyConverterIsoCompanion(p: Rep[ConverterIsoCompanionCtor]): ConverterIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ConverterIsoCompanionCtor])
      p.rhs.asInstanceOf[ConverterIsoCompanionCtor]
    else
      proxyOps[ConverterIsoCompanionCtor](p)
  }

  implicit case object ConverterIsoCompanionElem extends CompanionElem[ConverterIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ConverterIsoCompanionCtor]
    protected def getDefaultRep = ConverterIsoRep
  }

  implicit def proxyConverterIso[A, B](p: Rep[ConverterIso[A, B]]): ConverterIso[A, B] =
    proxyOps[ConverterIso[A, B]](p)

  implicit class ExtendedConverterIso[A, B](p: Rep[ConverterIso[A, B]]) {
    def toData: Rep[ConverterIsoData[A, B]] = {
      implicit val eA = p.convTo.eT;
implicit val eB = p.convTo.eR
      isoConverterIso(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ConverterIsoData[A, B], ConverterIso[A, B]] =
    reifyObject(new ConverterIsoIso[A, B]()(eA, eB))

  def mkConverterIso[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A]): Rep[ConverterIso[A, B]] = {
    new ConverterIsoCtor[A, B](convTo, convFrom)
  }
  def unmkConverterIso[A, B](p: Rep[IsoUR[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConverterIsoElem[A, B] @unchecked =>
      Some((asRep[ConverterIso[A, B]](p).convTo, asRep[ConverterIso[A, B]](p).convFrom))
    case _ =>
      None
  }

    object ConverterIsoMethods {
    object to {
      def unapply(d: Def[_]): Nullable[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ConverterIso[A, B]], Rep[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object from {
      def unapply(d: Def[_]): Nullable[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ConverterIso[A, B]], Rep[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Nullable[Rep[ConverterIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConverterIsoElem[_, _]] && method.getName == "isIdentity" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ConverterIso[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ConverterIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method equals
  }
} // of object ConverterIso
  registerEntityObject("ConverterIso", ConverterIso)

object ThunkIso extends EntityObject("ThunkIso") {
  case class ThunkIsoCtor[A, B]
      (override val innerIso: Iso[A, B])
    extends ThunkIso[A, B](innerIso) with Def[ThunkIso[A, B]] {
    implicit override lazy val eA = innerIso.eFrom;
implicit override lazy val eB = innerIso.eTo

    lazy val selfType = element[ThunkIso[A, B]]
    override def transform(t: Transformer) = ThunkIsoCtor[A, B](t(innerIso))
  }
  // elem for concrete class
  class ThunkIsoElem[A, B](val iso: Iso[ThunkIsoData[A, B], ThunkIso[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends Iso1URElem[A, B, Thunk, ThunkIso[A, B]]
    with ConcreteElem[ThunkIsoData[A, B], ThunkIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso1URElement(element[A], element[B], container[Thunk]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override def convertIso1UR(x: Rep[Iso1UR[A, B, Thunk]]) = RThunkIso(x.innerIso)
    override def getDefaultRep = RThunkIso(element[IsoUR[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIso[A, B]]
    }
  }

  // state representation type
  type ThunkIsoData[A, B] = IsoUR[A, B]

  // 3) Iso for concrete class
  class ThunkIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[ThunkIsoData[A, B], ThunkIso[A, B]] with Def[ThunkIsoIso[A, B]] {
    override def transform(t: Transformer) = new ThunkIsoIso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Rep[ThunkIso[A, B]] => p.innerIso }
    override def from(p: Rep[ThunkIso[A, B]]) =
      tryConvert[ThunkIso[A, B], IsoUR[A, B]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[IsoUR[A, B]]) = {
      val innerIso = p
      RThunkIso(innerIso)
    }
    lazy val eFrom = element[IsoUR[A, B]]
    lazy val eTo = new ThunkIsoElem[A, B](self)
    lazy val selfType = new ThunkIsoIsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class ThunkIsoIsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[ThunkIsoIso[A, B]] {
    def getDefaultRep = reifyObject(new ThunkIsoIso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ThunkIsoIso[A, B]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ThunkIsoCompanionCtor extends CompanionDef[ThunkIsoCompanionCtor] {
    def selfType = ThunkIsoCompanionElem
    override def toString = "ThunkIsoCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A, B](innerIso: Iso[A, B]): Rep[ThunkIso[A, B]] =
      mkThunkIso(innerIso)

    def unapply[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = unmkThunkIso(p)
  }
  lazy val ThunkIsoRep: Rep[ThunkIsoCompanionCtor] = new ThunkIsoCompanionCtor
  lazy val RThunkIso: ThunkIsoCompanionCtor = proxyThunkIsoCompanion(ThunkIsoRep)
  implicit def proxyThunkIsoCompanion(p: Rep[ThunkIsoCompanionCtor]): ThunkIsoCompanionCtor = {
    if (p.rhs.isInstanceOf[ThunkIsoCompanionCtor])
      p.rhs.asInstanceOf[ThunkIsoCompanionCtor]
    else
      proxyOps[ThunkIsoCompanionCtor](p)
  }

  implicit case object ThunkIsoCompanionElem extends CompanionElem[ThunkIsoCompanionCtor] {
    lazy val tag = weakTypeTag[ThunkIsoCompanionCtor]
    protected def getDefaultRep = ThunkIsoRep
  }

  implicit def proxyThunkIso[A, B](p: Rep[ThunkIso[A, B]]): ThunkIso[A, B] =
    proxyOps[ThunkIso[A, B]](p)

  implicit class ExtendedThunkIso[A, B](p: Rep[ThunkIso[A, B]]) {
    def toData: Rep[ThunkIsoData[A, B]] = {
      implicit val eA = p.innerIso.eFrom;
implicit val eB = p.innerIso.eTo
      isoThunkIso(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoThunkIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ThunkIsoData[A, B], ThunkIso[A, B]] =
    reifyObject(new ThunkIsoIso[A, B]()(eA, eB))

  def mkThunkIso[A, B]
    (innerIso: Iso[A, B]): Rep[ThunkIso[A, B]] = {
    new ThunkIsoCtor[A, B](innerIso)
  }
  def unmkThunkIso[A, B](p: Rep[Iso1UR[A, B, Thunk]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ThunkIsoElem[A, B] @unchecked =>
      Some((asRep[ThunkIso[A, B]](p).innerIso))
    case _ =>
      None
  }

    object ThunkIsoMethods {
    object cC {
      def unapply(d: Def[_]): Nullable[Rep[ThunkIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "cC" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ThunkIso[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ThunkIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object from {
      def unapply(d: Def[_]): Nullable[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "from" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ThunkIso[A, B]], Th[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object to {
      def unapply(d: Def[_]): Nullable[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ThunkIsoElem[_, _]] && method.getName == "to" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ThunkIso[A, B]], Th[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object ThunkIso
  registerEntityObject("ThunkIso", ThunkIso)

  registerModule(ViewsModule)
}

object ViewsModule extends scalan.ModuleInfo("scalan", "Views")
}

