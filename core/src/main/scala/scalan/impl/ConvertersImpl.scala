package scalan

import OverloadHack.Overloaded2
import scalan.primitives.TypeSum
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait ConvertersDefs extends Converters {
  self: Scalan with ConvertersModule =>
import IsoUR._
import Converter._
import BaseConverter._
import ComposeConverter._
import FunctorConverter._
import IdentityConv._
import NaturalConverter._
import PairConverter._
import SumConverter._

object Converter extends EntityObject("Converter") {
  private val ConverterClass = classOf[Converter[_, _]]

  // entityAdapter for Converter trait
  case class ConverterAdapter[T, R](source: Ref[Converter[T, R]])
      extends Converter[T, R]
      with Def[Converter[T, R]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]

    val resultType: Elem[Converter[T, R]] = element[Converter[T, R]]
    override def transform(t: Transformer) = ConverterAdapter[T, R](t(source))

    def apply(x: Ref[T]): Ref[R] = {
      asRep[R](mkMethodCall(source,
        ConverterClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](x),
        true, true, element[R]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyConverter[T, R](p: Ref[Converter[T, R]]): Converter[T, R] = {
    if (p.rhs.isInstanceOf[Converter[T, R]@unchecked]) p.rhs.asInstanceOf[Converter[T, R]]
    else
      ConverterAdapter(p)
  }

  // familyElem
  class ConverterElem[T, R, To <: Converter[T, R]](implicit _eT: Elem[T], _eR: Elem[R])
    extends EntityElem[To] {
    def eT = _eT
    def eR = _eR

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit def converterElement[T, R](implicit eT: Elem[T], eR: Elem[R]): Elem[Converter[T, R]] =
    cachedElemByClass(eT, eR)(classOf[ConverterElem[T, R, Converter[T, R]]])

  implicit case object ConverterCompanionElem extends CompanionElem[ConverterCompanionCtor]

  abstract class ConverterCompanionCtor extends CompanionDef[ConverterCompanionCtor] with ConverterCompanion {
    def resultType = ConverterCompanionElem
    override def toString = "Converter"
  }
  implicit def proxyConverterCompanionCtor(p: Ref[ConverterCompanionCtor]): ConverterCompanionCtor =
    p.rhs.asInstanceOf[ConverterCompanionCtor]

  lazy val RConverter: Ref[ConverterCompanionCtor] = new ConverterCompanionCtor {
    private val thisClass = classOf[ConverterCompanion]
  }
} // of object Converter
  registerEntityObject("Converter", Converter)

object IdentityConv extends EntityObject("IdentityConv") {
  case class IdentityConvCtor[A]
      ()(implicit eT: Elem[A])
    extends IdentityConv[A]() with Def[IdentityConv[A]] {
    lazy val resultType = element[IdentityConv[A]]
    override def transform(t: Transformer) = IdentityConvCtor[A]()(eT)
  }
  // elem for concrete class
  class IdentityConvElem[A](val iso: Iso[IdentityConvData[A], IdentityConv[A]])(implicit override val eT: Elem[A])
    extends ConverterElem[A, A, IdentityConv[A]]
    with ConcreteElem[IdentityConvData[A], IdentityConv[A]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[A], element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eT -> scalan.util.Invariant))
  }

  // state representation type
  type IdentityConvData[A] = Unit

  // 3) Iso for concrete class
  class IdentityConvIso[A](implicit eT: Elem[A])
    extends EntityIso[IdentityConvData[A], IdentityConv[A]] with Def[IdentityConvIso[A]] {
    override def transform(t: Transformer) = new IdentityConvIso[A]()(eT)
    private lazy val _safeFrom = fun { p: Ref[IdentityConv[A]] => () }
    override def from(p: Ref[IdentityConv[A]]) =
      tryConvert[IdentityConv[A], Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Unit]) = {
      val unit = p
      RIdentityConv()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityConvElem[A](self)
    lazy val resultType = new IdentityConvIsoElem[A](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class IdentityConvIsoElem[A](eT: Elem[A]) extends Elem[IdentityConvIso[A]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IdentityConvCompanionCtor extends CompanionDef[IdentityConvCompanionCtor] {
    def resultType = IdentityConvCompanionElem
    override def toString = "IdentityConvCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Ref[IdentityConvData[A]])(implicit eT: Elem[A]): Ref[IdentityConv[A]] = {
      isoIdentityConv[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A]()(implicit eT: Elem[A]): Ref[IdentityConv[A]] =
      mkIdentityConv()

    def unapply[A](p: Ref[Converter[A, A]]) = unmkIdentityConv(p)
  }
  lazy val IdentityConvRef: Ref[IdentityConvCompanionCtor] = new IdentityConvCompanionCtor
  lazy val RIdentityConv: IdentityConvCompanionCtor = proxyIdentityConvCompanion(IdentityConvRef)
  implicit def proxyIdentityConvCompanion(p: Ref[IdentityConvCompanionCtor]): IdentityConvCompanionCtor = {
    if (p.rhs.isInstanceOf[IdentityConvCompanionCtor])
      p.rhs.asInstanceOf[IdentityConvCompanionCtor]
    else
      proxyOps[IdentityConvCompanionCtor](p)
  }

  implicit case object IdentityConvCompanionElem extends CompanionElem[IdentityConvCompanionCtor]

  implicit def proxyIdentityConv[A](p: Ref[IdentityConv[A]]): IdentityConv[A] = {
    if (p.rhs.isInstanceOf[IdentityConv[A]@unchecked])
      p.rhs.asInstanceOf[IdentityConv[A]]
    else
      proxyOps[IdentityConv[A]](p)
  }

  implicit class ExtendedIdentityConv[A](p: Ref[IdentityConv[A]])(implicit eT: Elem[A]) {
    def toData: Ref[IdentityConvData[A]] = {
      isoIdentityConv(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityConv[A](implicit eT: Elem[A]): Iso[IdentityConvData[A], IdentityConv[A]] =
    reifyObject(new IdentityConvIso[A]()(eT))

  def mkIdentityConv[A]
    ()(implicit eT: Elem[A]): Ref[IdentityConv[A]] = {
    new IdentityConvCtor[A]()
  }
  def unmkIdentityConv[A](p: Ref[Converter[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityConvElem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }
} // of object IdentityConv
  registerEntityObject("IdentityConv", IdentityConv)

object BaseConverter extends EntityObject("BaseConverter") {
  case class BaseConverterCtor[T, R]
      (override val convFun: Ref[T => R])
    extends BaseConverter[T, R](convFun) with Def[BaseConverter[T, R]] {
    implicit lazy val eT = convFun.elem.eDom;
implicit lazy val eR = convFun.elem.eRange

    lazy val resultType = element[BaseConverter[T, R]]
    override def transform(t: Transformer) = BaseConverterCtor[T, R](t(convFun))
  }
  // elem for concrete class
  class BaseConverterElem[T, R](val iso: Iso[BaseConverterData[T, R], BaseConverter[T, R]])(implicit override val eT: Elem[T], override val eR: Elem[R])
    extends ConverterElem[T, R, BaseConverter[T, R]]
    with ConcreteElem[BaseConverterData[T, R], BaseConverter[T, R]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[T], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  // state representation type
  type BaseConverterData[T, R] = T => R

  // 3) Iso for concrete class
  class BaseConverterIso[T, R](implicit eT: Elem[T], eR: Elem[R])
    extends EntityIso[BaseConverterData[T, R], BaseConverter[T, R]] with Def[BaseConverterIso[T, R]] {
    override def transform(t: Transformer) = new BaseConverterIso[T, R]()(eT, eR)
    private lazy val _safeFrom = fun { p: Ref[BaseConverter[T, R]] => p.convFun }
    override def from(p: Ref[BaseConverter[T, R]]) =
      tryConvert[BaseConverter[T, R], T => R](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[T => R]) = {
      val convFun = p
      RBaseConverter(convFun)
    }
    lazy val eFrom = element[T => R]
    lazy val eTo = new BaseConverterElem[T, R](self)
    lazy val resultType = new BaseConverterIsoElem[T, R](eT, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eT
      case 1 => eR
    }
  }
  case class BaseConverterIsoElem[T, R](eT: Elem[T], eR: Elem[R]) extends Elem[BaseConverterIso[T, R]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class BaseConverterCompanionCtor extends CompanionDef[BaseConverterCompanionCtor] with BaseConverterCompanion {
    def resultType = BaseConverterCompanionElem
    override def toString = "BaseConverterCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T, R](convFun: Ref[T => R]): Ref[BaseConverter[T, R]] =
      mkBaseConverter(convFun)

    def unapply[T, R](p: Ref[Converter[T, R]]) = unmkBaseConverter(p)
  }
  lazy val BaseConverterRef: Ref[BaseConverterCompanionCtor] = new BaseConverterCompanionCtor
  lazy val RBaseConverter: BaseConverterCompanionCtor = proxyBaseConverterCompanion(BaseConverterRef)
  implicit def proxyBaseConverterCompanion(p: Ref[BaseConverterCompanionCtor]): BaseConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[BaseConverterCompanionCtor])
      p.rhs.asInstanceOf[BaseConverterCompanionCtor]
    else
      proxyOps[BaseConverterCompanionCtor](p)
  }

  implicit case object BaseConverterCompanionElem extends CompanionElem[BaseConverterCompanionCtor]

  implicit def proxyBaseConverter[T, R](p: Ref[BaseConverter[T, R]]): BaseConverter[T, R] = {
    if (p.rhs.isInstanceOf[BaseConverter[T, R]@unchecked])
      p.rhs.asInstanceOf[BaseConverter[T, R]]
    else
      proxyOps[BaseConverter[T, R]](p)
  }

  implicit class ExtendedBaseConverter[T, R](p: Ref[BaseConverter[T, R]]) {
    def toData: Ref[BaseConverterData[T, R]] = {
      implicit val eT = p.convFun.elem.eDom;
implicit val eR = p.convFun.elem.eRange
      isoBaseConverter(eT, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseConverter[T, R](implicit eT: Elem[T], eR: Elem[R]): Iso[BaseConverterData[T, R], BaseConverter[T, R]] =
    reifyObject(new BaseConverterIso[T, R]()(eT, eR))

  def mkBaseConverter[T, R]
    (convFun: Ref[T => R]): Ref[BaseConverter[T, R]] = {
    new BaseConverterCtor[T, R](convFun)
  }
  def unmkBaseConverter[T, R](p: Ref[Converter[T, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BaseConverterElem[T, R] @unchecked =>
      Some((asRep[BaseConverter[T, R]](p).convFun))
    case _ =>
      None
  }
} // of object BaseConverter
  registerEntityObject("BaseConverter", BaseConverter)

object PairConverter extends EntityObject("PairConverter") {
  case class PairConverterCtor[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])
    extends PairConverter[A1, A2, B1, B2](conv1, conv2) with Def[PairConverter[A1, A2, B1, B2]] {
    implicit lazy val eA1 = conv1.eT;
implicit lazy val eA2 = conv2.eT;
implicit lazy val eB1 = conv1.eR;
implicit lazy val eB2 = conv2.eR
    override lazy val eT: Elem[(A1, A2)] = implicitly[Elem[(A1, A2)]]
override lazy val eR: Elem[(B1, B2)] = implicitly[Elem[(B1, B2)]]
    lazy val resultType = element[PairConverter[A1, A2, B1, B2]]
    override def transform(t: Transformer) = PairConverterCtor[A1, A2, B1, B2](t(conv1), t(conv2))
  }
  // elem for concrete class
  class PairConverterElem[A1, A2, B1, B2](val iso: Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends ConverterElem[(A1, A2), (B1, B2), PairConverter[A1, A2, B1, B2]]
    with ConcreteElem[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }

  // state representation type
  type PairConverterData[A1, A2, B1, B2] = (Converter[A1, B1], Converter[A2, B2])

  // 3) Iso for concrete class
  class PairConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] with Def[PairConverterIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new PairConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Ref[PairConverter[A1, A2, B1, B2]] => (p.conv1, p.conv2) }
    override def from(p: Ref[PairConverter[A1, A2, B1, B2]]) =
      tryConvert[PairConverter[A1, A2, B1, B2], (Converter[A1, B1], Converter[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Converter[A1, B1], Converter[A2, B2])]) = {
      val Pair(conv1, conv2) = p
      RPairConverter(conv1, conv2)
    }
    lazy val eFrom = pairElement(element[Converter[A1, B1]], element[Converter[A2, B2]])
    lazy val eTo = new PairConverterElem[A1, A2, B1, B2](self)
    lazy val resultType = new PairConverterIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class PairConverterIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[PairConverterIso[A1, A2, B1, B2]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairConverterCompanionCtor extends CompanionDef[PairConverterCompanionCtor] with PairConverterCompanion {
    def resultType = PairConverterCompanionElem
    override def toString = "PairConverterCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Ref[PairConverterData[A1, A2, B1, B2]]): Ref[PairConverter[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eT;
implicit val eA2 = p._2.eT;
implicit val eB1 = p._1.eR;
implicit val eB2 = p._2.eR
      isoPairConverter[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2]): Ref[PairConverter[A1, A2, B1, B2]] =
      mkPairConverter(conv1, conv2)

    def unapply[A1, A2, B1, B2](p: Ref[Converter[(A1, A2), (B1, B2)]]) = unmkPairConverter(p)
  }
  lazy val PairConverterRef: Ref[PairConverterCompanionCtor] = new PairConverterCompanionCtor
  lazy val RPairConverter: PairConverterCompanionCtor = proxyPairConverterCompanion(PairConverterRef)
  implicit def proxyPairConverterCompanion(p: Ref[PairConverterCompanionCtor]): PairConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[PairConverterCompanionCtor])
      p.rhs.asInstanceOf[PairConverterCompanionCtor]
    else
      proxyOps[PairConverterCompanionCtor](p)
  }

  implicit case object PairConverterCompanionElem extends CompanionElem[PairConverterCompanionCtor]

  implicit def proxyPairConverter[A1, A2, B1, B2](p: Ref[PairConverter[A1, A2, B1, B2]]): PairConverter[A1, A2, B1, B2] = {
    if (p.rhs.isInstanceOf[PairConverter[A1, A2, B1, B2]@unchecked])
      p.rhs.asInstanceOf[PairConverter[A1, A2, B1, B2]]
    else
      proxyOps[PairConverter[A1, A2, B1, B2]](p)
  }

  implicit class ExtendedPairConverter[A1, A2, B1, B2](p: Ref[PairConverter[A1, A2, B1, B2]]) {
    def toData: Ref[PairConverterData[A1, A2, B1, B2]] = {
      implicit val eA1 = p.conv1.eT;
implicit val eA2 = p.conv2.eT;
implicit val eB1 = p.conv1.eR;
implicit val eB2 = p.conv2.eR
      isoPairConverter(eA1, eA2, eB1, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoPairConverter[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairConverterData[A1, A2, B1, B2], PairConverter[A1, A2, B1, B2]] =
    reifyObject(new PairConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  def mkPairConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2]): Ref[PairConverter[A1, A2, B1, B2]] = {
    new PairConverterCtor[A1, A2, B1, B2](conv1, conv2)
  }
  def unmkPairConverter[A1, A2, B1, B2](p: Ref[Converter[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[PairConverter[A1, A2, B1, B2]](p).conv1, asRep[PairConverter[A1, A2, B1, B2]](p).conv2))
    case _ =>
      None
  }
} // of object PairConverter
  registerEntityObject("PairConverter", PairConverter)

object SumConverter extends EntityObject("SumConverter") {
  case class SumConverterCtor[A1, A2, B1, B2]
      (override val conv1: Conv[A1, B1], override val conv2: Conv[A2, B2])
    extends SumConverter[A1, A2, B1, B2](conv1, conv2) with Def[SumConverter[A1, A2, B1, B2]] {
    implicit lazy val eA1 = conv1.eT;
implicit lazy val eA2 = conv2.eT;
implicit lazy val eB1 = conv1.eR;
implicit lazy val eB2 = conv2.eR
    override lazy val eT: Elem[$bar[A1, A2]] = implicitly[Elem[$bar[A1, A2]]]
override lazy val eR: Elem[$bar[B1, B2]] = implicitly[Elem[$bar[B1, B2]]]
    lazy val resultType = element[SumConverter[A1, A2, B1, B2]]
    override def transform(t: Transformer) = SumConverterCtor[A1, A2, B1, B2](t(conv1), t(conv2))
  }
  // elem for concrete class
  class SumConverterElem[A1, A2, B1, B2](val iso: Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]])(implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends ConverterElem[$bar[A1, A2], $bar[B1, B2], SumConverter[A1, A2, B1, B2]]
    with ConcreteElem[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }

  // state representation type
  type SumConverterData[A1, A2, B1, B2] = (Converter[A1, B1], Converter[A2, B2])

  // 3) Iso for concrete class
  class SumConverterIso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends EntityIso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] with Def[SumConverterIso[A1, A2, B1, B2]] {
    override def transform(t: Transformer) = new SumConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2)
    private lazy val _safeFrom = fun { p: Ref[SumConverter[A1, A2, B1, B2]] => (p.conv1, p.conv2) }
    override def from(p: Ref[SumConverter[A1, A2, B1, B2]]) =
      tryConvert[SumConverter[A1, A2, B1, B2], (Converter[A1, B1], Converter[A2, B2])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Converter[A1, B1], Converter[A2, B2])]) = {
      val Pair(conv1, conv2) = p
      RSumConverter(conv1, conv2)
    }
    lazy val eFrom = pairElement(element[Converter[A1, B1]], element[Converter[A2, B2]])
    lazy val eTo = new SumConverterElem[A1, A2, B1, B2](self)
    lazy val resultType = new SumConverterIsoElem[A1, A2, B1, B2](eA1, eA2, eB1, eB2)
    def productArity = 4
    def productElement(n: Int) = n match {
      case 0 => eA1
      case 1 => eA2
      case 2 => eB1
      case 3 => eB2
    }
  }
  case class SumConverterIsoElem[A1, A2, B1, B2](eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) extends Elem[SumConverterIso[A1, A2, B1, B2]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A1" -> (eA1 -> scalan.util.Invariant), "A2" -> (eA2 -> scalan.util.Invariant), "B1" -> (eB1 -> scalan.util.Invariant), "B2" -> (eB2 -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SumConverterCompanionCtor extends CompanionDef[SumConverterCompanionCtor] with SumConverterCompanion {
    def resultType = SumConverterCompanionElem
    override def toString = "SumConverterCompanion"
    @scalan.OverloadId("fromData")
    def apply[A1, A2, B1, B2](p: Ref[SumConverterData[A1, A2, B1, B2]]): Ref[SumConverter[A1, A2, B1, B2]] = {
      implicit val eA1 = p._1.eT;
implicit val eA2 = p._2.eT;
implicit val eB1 = p._1.eR;
implicit val eB2 = p._2.eR
      isoSumConverter[A1, A2, B1, B2].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A1, A2, B1, B2](conv1: Conv[A1, B1], conv2: Conv[A2, B2]): Ref[SumConverter[A1, A2, B1, B2]] =
      mkSumConverter(conv1, conv2)

    def unapply[A1, A2, B1, B2](p: Ref[Converter[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumConverter(p)
  }
  lazy val SumConverterRef: Ref[SumConverterCompanionCtor] = new SumConverterCompanionCtor
  lazy val RSumConverter: SumConverterCompanionCtor = proxySumConverterCompanion(SumConverterRef)
  implicit def proxySumConverterCompanion(p: Ref[SumConverterCompanionCtor]): SumConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[SumConverterCompanionCtor])
      p.rhs.asInstanceOf[SumConverterCompanionCtor]
    else
      proxyOps[SumConverterCompanionCtor](p)
  }

  implicit case object SumConverterCompanionElem extends CompanionElem[SumConverterCompanionCtor]

  implicit def proxySumConverter[A1, A2, B1, B2](p: Ref[SumConverter[A1, A2, B1, B2]]): SumConverter[A1, A2, B1, B2] = {
    if (p.rhs.isInstanceOf[SumConverter[A1, A2, B1, B2]@unchecked])
      p.rhs.asInstanceOf[SumConverter[A1, A2, B1, B2]]
    else
      proxyOps[SumConverter[A1, A2, B1, B2]](p)
  }

  implicit class ExtendedSumConverter[A1, A2, B1, B2](p: Ref[SumConverter[A1, A2, B1, B2]]) {
    def toData: Ref[SumConverterData[A1, A2, B1, B2]] = {
      implicit val eA1 = p.conv1.eT;
implicit val eA2 = p.conv2.eT;
implicit val eB1 = p.conv1.eR;
implicit val eB2 = p.conv2.eR
      isoSumConverter(eA1, eA2, eB1, eB2).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSumConverter[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumConverterData[A1, A2, B1, B2], SumConverter[A1, A2, B1, B2]] =
    reifyObject(new SumConverterIso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  def mkSumConverter[A1, A2, B1, B2]
    (conv1: Conv[A1, B1], conv2: Conv[A2, B2]): Ref[SumConverter[A1, A2, B1, B2]] = {
    new SumConverterCtor[A1, A2, B1, B2](conv1, conv2)
  }
  def unmkSumConverter[A1, A2, B1, B2](p: Ref[Converter[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumConverterElem[A1, A2, B1, B2] @unchecked =>
      Some((asRep[SumConverter[A1, A2, B1, B2]](p).conv1, asRep[SumConverter[A1, A2, B1, B2]](p).conv2))
    case _ =>
      None
  }
} // of object SumConverter
  registerEntityObject("SumConverter", SumConverter)

object ComposeConverter extends EntityObject("ComposeConverter") {
  case class ComposeConverterCtor[A, B, C]
      (override val conv2: Conv[B, C], override val conv1: Conv[A, B])
    extends ComposeConverter[A, B, C](conv2, conv1) with Def[ComposeConverter[A, B, C]] {
    implicit lazy val eA = conv1.eT;
implicit lazy val eB = conv2.eT;
implicit lazy val eC = conv2.eR

    lazy val resultType = element[ComposeConverter[A, B, C]]
    override def transform(t: Transformer) = ComposeConverterCtor[A, B, C](t(conv2), t(conv1))
  }
  // elem for concrete class
  class ComposeConverterElem[A, B, C](val iso: Iso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]])(implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C])
    extends ConverterElem[A, C, ComposeConverter[A, B, C]]
    with ConcreteElem[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[A], element[C]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }

  // state representation type
  type ComposeConverterData[A, B, C] = (Converter[B, C], Converter[A, B])

  // 3) Iso for concrete class
  class ComposeConverterIso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends EntityIso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] with Def[ComposeConverterIso[A, B, C]] {
    override def transform(t: Transformer) = new ComposeConverterIso[A, B, C]()(eA, eB, eC)
    private lazy val _safeFrom = fun { p: Ref[ComposeConverter[A, B, C]] => (p.conv2, p.conv1) }
    override def from(p: Ref[ComposeConverter[A, B, C]]) =
      tryConvert[ComposeConverter[A, B, C], (Converter[B, C], Converter[A, B])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Converter[B, C], Converter[A, B])]) = {
      val Pair(conv2, conv1) = p
      RComposeConverter(conv2, conv1)
    }
    lazy val eFrom = pairElement(element[Converter[B, C]], element[Converter[A, B]])
    lazy val eTo = new ComposeConverterElem[A, B, C](self)
    lazy val resultType = new ComposeConverterIsoElem[A, B, C](eA, eB, eC)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
      case 2 => eC
    }
  }
  case class ComposeConverterIsoElem[A, B, C](eA: Elem[A], eB: Elem[B], eC: Elem[C]) extends Elem[ComposeConverterIso[A, B, C]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "C" -> (eC -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ComposeConverterCompanionCtor extends CompanionDef[ComposeConverterCompanionCtor] {
    def resultType = ComposeConverterCompanionElem
    override def toString = "ComposeConverterCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B, C](p: Ref[ComposeConverterData[A, B, C]]): Ref[ComposeConverter[A, B, C]] = {
      implicit val eA = p._2.eT;
implicit val eB = p._1.eT;
implicit val eC = p._1.eR
      isoComposeConverter[A, B, C].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B, C](conv2: Conv[B, C], conv1: Conv[A, B]): Ref[ComposeConverter[A, B, C]] =
      mkComposeConverter(conv2, conv1)

    def unapply[A, B, C](p: Ref[Converter[A, C]]) = unmkComposeConverter(p)
  }
  lazy val ComposeConverterRef: Ref[ComposeConverterCompanionCtor] = new ComposeConverterCompanionCtor
  lazy val RComposeConverter: ComposeConverterCompanionCtor = proxyComposeConverterCompanion(ComposeConverterRef)
  implicit def proxyComposeConverterCompanion(p: Ref[ComposeConverterCompanionCtor]): ComposeConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[ComposeConverterCompanionCtor])
      p.rhs.asInstanceOf[ComposeConverterCompanionCtor]
    else
      proxyOps[ComposeConverterCompanionCtor](p)
  }

  implicit case object ComposeConverterCompanionElem extends CompanionElem[ComposeConverterCompanionCtor]

  implicit def proxyComposeConverter[A, B, C](p: Ref[ComposeConverter[A, B, C]]): ComposeConverter[A, B, C] = {
    if (p.rhs.isInstanceOf[ComposeConverter[A, B, C]@unchecked])
      p.rhs.asInstanceOf[ComposeConverter[A, B, C]]
    else
      proxyOps[ComposeConverter[A, B, C]](p)
  }

  implicit class ExtendedComposeConverter[A, B, C](p: Ref[ComposeConverter[A, B, C]]) {
    def toData: Ref[ComposeConverterData[A, B, C]] = {
      implicit val eA = p.conv1.eT;
implicit val eB = p.conv2.eT;
implicit val eC = p.conv2.eR
      isoComposeConverter(eA, eB, eC).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeConverter[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeConverterData[A, B, C], ComposeConverter[A, B, C]] =
    reifyObject(new ComposeConverterIso[A, B, C]()(eA, eB, eC))

  def mkComposeConverter[A, B, C]
    (conv2: Conv[B, C], conv1: Conv[A, B]): Ref[ComposeConverter[A, B, C]] = {
    new ComposeConverterCtor[A, B, C](conv2, conv1)
  }
  def unmkComposeConverter[A, B, C](p: Ref[Converter[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeConverterElem[A, B, C] @unchecked =>
      Some((asRep[ComposeConverter[A, B, C]](p).conv2, asRep[ComposeConverter[A, B, C]](p).conv1))
    case _ =>
      None
  }
} // of object ComposeConverter
  registerEntityObject("ComposeConverter", ComposeConverter)

object FunctorConverter extends EntityObject("FunctorConverter") {
  case class FunctorConverterCtor[A, B, F[_]]
      (override val itemConv: Conv[A, B])(implicit F: Functor[F])
    extends FunctorConverter[A, B, F](itemConv) with Def[FunctorConverter[A, B, F]] {
    implicit lazy val eA = itemConv.eT;
implicit lazy val eB = itemConv.eR

    lazy val resultType = element[FunctorConverter[A, B, F]]
    override def transform(t: Transformer) = FunctorConverterCtor[A, B, F](t(itemConv))(F)
  }
  // elem for concrete class
  class FunctorConverterElem[A, B, F[_]](val iso: Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]])(implicit val F: Functor[F], val eA: Elem[A], val eB: Elem[B])
    extends ConverterElem[F[A], F[B], FunctorConverter[A, B, F]]
    with ConcreteElem[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[F[B]]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "F" -> (F -> scalan.util.Invariant))
  }

  // state representation type
  type FunctorConverterData[A, B, F[_]] = Converter[A, B]

  // 3) Iso for concrete class
  class FunctorConverterIso[A, B, F[_]](implicit F: Functor[F], eA: Elem[A], eB: Elem[B])
    extends EntityIso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] with Def[FunctorConverterIso[A, B, F]] {
    override def transform(t: Transformer) = new FunctorConverterIso[A, B, F]()(F, eA, eB)
    private lazy val _safeFrom = fun { p: Ref[FunctorConverter[A, B, F]] => p.itemConv }
    override def from(p: Ref[FunctorConverter[A, B, F]]) =
      tryConvert[FunctorConverter[A, B, F], Converter[A, B]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Converter[A, B]]) = {
      val itemConv = p
      RFunctorConverter(itemConv)
    }
    lazy val eFrom = element[Converter[A, B]]
    lazy val eTo = new FunctorConverterElem[A, B, F](self)
    lazy val resultType = new FunctorConverterIsoElem[A, B, F](F, eA, eB)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => F
      case 1 => eA
      case 2 => eB
    }
  }
  case class FunctorConverterIsoElem[A, B, F[_]](F: Functor[F], eA: Elem[A], eB: Elem[B]) extends Elem[FunctorConverterIso[A, B, F]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant), "F" -> (F -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class FunctorConverterCompanionCtor extends CompanionDef[FunctorConverterCompanionCtor] with FunctorConverterCompanion {
    def resultType = FunctorConverterCompanionElem
    override def toString = "FunctorConverterCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A, B, F[_]](itemConv: Conv[A, B])(implicit F: Functor[F]): Ref[FunctorConverter[A, B, F]] =
      mkFunctorConverter(itemConv)

    def unapply[A, B, F[_]](p: Ref[Converter[F[A], F[B]]]) = unmkFunctorConverter(p)
  }
  lazy val FunctorConverterRef: Ref[FunctorConverterCompanionCtor] = new FunctorConverterCompanionCtor
  lazy val RFunctorConverter: FunctorConverterCompanionCtor = proxyFunctorConverterCompanion(FunctorConverterRef)
  implicit def proxyFunctorConverterCompanion(p: Ref[FunctorConverterCompanionCtor]): FunctorConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[FunctorConverterCompanionCtor])
      p.rhs.asInstanceOf[FunctorConverterCompanionCtor]
    else
      proxyOps[FunctorConverterCompanionCtor](p)
  }

  implicit case object FunctorConverterCompanionElem extends CompanionElem[FunctorConverterCompanionCtor]

  implicit def proxyFunctorConverter[A, B, F[_]](p: Ref[FunctorConverter[A, B, F]]): FunctorConverter[A, B, F] = {
    if (p.rhs.isInstanceOf[FunctorConverter[A, B, F]@unchecked])
      p.rhs.asInstanceOf[FunctorConverter[A, B, F]]
    else
      proxyOps[FunctorConverter[A, B, F]](p)
  }

  implicit class ExtendedFunctorConverter[A, B, F[_]](p: Ref[FunctorConverter[A, B, F]])(implicit F: Functor[F]) {
    def toData: Ref[FunctorConverterData[A, B, F]] = {
      implicit val eA = p.itemConv.eT;
implicit val eB = p.itemConv.eR
      isoFunctorConverter(F, eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoFunctorConverter[A, B, F[_]](implicit F: Functor[F], eA: Elem[A], eB: Elem[B]): Iso[FunctorConverterData[A, B, F], FunctorConverter[A, B, F]] =
    reifyObject(new FunctorConverterIso[A, B, F]()(F, eA, eB))

  def mkFunctorConverter[A, B, F[_]]
    (itemConv: Conv[A, B])(implicit F: Functor[F]): Ref[FunctorConverter[A, B, F]] = {
    new FunctorConverterCtor[A, B, F](itemConv)
  }
  def unmkFunctorConverter[A, B, F[_]](p: Ref[Converter[F[A], F[B]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FunctorConverterElem[A, B, F] @unchecked =>
      Some((asRep[FunctorConverter[A, B, F]](p).itemConv))
    case _ =>
      None
  }
} // of object FunctorConverter
  registerEntityObject("FunctorConverter", FunctorConverter)

object NaturalConverter extends EntityObject("NaturalConverter") {
  case class NaturalConverterCtor[A, F[_], G[_]]
      (override val convFun: Ref[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends NaturalConverter[A, F, G](convFun) with Def[NaturalConverter[A, F, G]] {
    lazy val resultType = element[NaturalConverter[A, F, G]]
    override def transform(t: Transformer) = NaturalConverterCtor[A, F, G](t(convFun))(eA, cF, cG)
  }
  // elem for concrete class
  class NaturalConverterElem[A, F[_], G[_]](val iso: Iso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]])(implicit val eA: Elem[A], val cF: Cont[F], val cG: Cont[G])
    extends ConverterElem[F[A], G[A], NaturalConverter[A, F, G]]
    with ConcreteElem[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] {
    override lazy val parent: Option[Elem[_]] = Some(converterElement(element[F[A]], element[G[A]]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "F" -> (cF -> scalan.util.Invariant), "G" -> (cG -> scalan.util.Invariant))
  }

  // state representation type
  type NaturalConverterData[A, F[_], G[_]] = F[A] => G[A]

  // 3) Iso for concrete class
  class NaturalConverterIso[A, F[_], G[_]](implicit eA: Elem[A], cF: Cont[F], cG: Cont[G])
    extends EntityIso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] with Def[NaturalConverterIso[A, F, G]] {
    override def transform(t: Transformer) = new NaturalConverterIso[A, F, G]()(eA, cF, cG)
    private lazy val _safeFrom = fun { p: Ref[NaturalConverter[A, F, G]] => p.convFun }
    override def from(p: Ref[NaturalConverter[A, F, G]]) =
      tryConvert[NaturalConverter[A, F, G], F[A] => G[A]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[F[A] => G[A]]) = {
      val convFun = p
      RNaturalConverter(convFun)
    }
    lazy val eFrom = element[F[A] => G[A]]
    lazy val eTo = new NaturalConverterElem[A, F, G](self)
    lazy val resultType = new NaturalConverterIsoElem[A, F, G](eA, cF, cG)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => cF
      case 2 => cG
    }
  }
  case class NaturalConverterIsoElem[A, F[_], G[_]](eA: Elem[A], cF: Cont[F], cG: Cont[G]) extends Elem[NaturalConverterIso[A, F, G]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "F" -> (cF -> scalan.util.Invariant), "G" -> (cG -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class NaturalConverterCompanionCtor extends CompanionDef[NaturalConverterCompanionCtor] {
    def resultType = NaturalConverterCompanionElem
    override def toString = "NaturalConverterCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A, F[_], G[_]](convFun: Ref[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Ref[NaturalConverter[A, F, G]] =
      mkNaturalConverter(convFun)

    def unapply[A, F[_], G[_]](p: Ref[Converter[F[A], G[A]]]) = unmkNaturalConverter(p)
  }
  lazy val NaturalConverterRef: Ref[NaturalConverterCompanionCtor] = new NaturalConverterCompanionCtor
  lazy val RNaturalConverter: NaturalConverterCompanionCtor = proxyNaturalConverterCompanion(NaturalConverterRef)
  implicit def proxyNaturalConverterCompanion(p: Ref[NaturalConverterCompanionCtor]): NaturalConverterCompanionCtor = {
    if (p.rhs.isInstanceOf[NaturalConverterCompanionCtor])
      p.rhs.asInstanceOf[NaturalConverterCompanionCtor]
    else
      proxyOps[NaturalConverterCompanionCtor](p)
  }

  implicit case object NaturalConverterCompanionElem extends CompanionElem[NaturalConverterCompanionCtor]

  implicit def proxyNaturalConverter[A, F[_], G[_]](p: Ref[NaturalConverter[A, F, G]]): NaturalConverter[A, F, G] = {
    if (p.rhs.isInstanceOf[NaturalConverter[A, F, G]@unchecked])
      p.rhs.asInstanceOf[NaturalConverter[A, F, G]]
    else
      proxyOps[NaturalConverter[A, F, G]](p)
  }

  implicit class ExtendedNaturalConverter[A, F[_], G[_]](p: Ref[NaturalConverter[A, F, G]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]) {
    def toData: Ref[NaturalConverterData[A, F, G]] = {
      isoNaturalConverter(eA, cF, cG).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoNaturalConverter[A, F[_], G[_]](implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Iso[NaturalConverterData[A, F, G], NaturalConverter[A, F, G]] =
    reifyObject(new NaturalConverterIso[A, F, G]()(eA, cF, cG))

  def mkNaturalConverter[A, F[_], G[_]]
    (convFun: Ref[F[A] => G[A]])(implicit eA: Elem[A], cF: Cont[F], cG: Cont[G]): Ref[NaturalConverter[A, F, G]] = {
    new NaturalConverterCtor[A, F, G](convFun)
  }
  def unmkNaturalConverter[A, F[_], G[_]](p: Ref[Converter[F[A], G[A]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: NaturalConverterElem[A, F, G] @unchecked =>
      Some((asRep[NaturalConverter[A, F, G]](p).convFun))
    case _ =>
      None
  }
} // of object NaturalConverter
  registerEntityObject("NaturalConverter", NaturalConverter)

  registerModule(ConvertersModule)
}

object ConvertersModule extends scalan.ModuleInfo("scalan", "Converters")
}

