package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ConcreteCostsDefs extends scalan.Scalan with ConcreteCosts {
  self: Library =>
import IsoUR._
import Converter._
import CCostedBuilder._
import CCostedCol._
import CCostedFunc._
import CCostedNestedCol._
import CCostedOption._
import CCostedPair._
import CCostedPairCol._
import CCostedPrim._
import CCostedSum._
import Col._
import Costed._
import CostedBuilder._
import CostedCol._
import CostedFunc._
import CostedNestedCol._
import CostedNone._
import CostedOption._
import CostedPair._
import CostedPairCol._
import CostedPrim._
import CostedSome._
import CostedSum._
import MonoidBuilderInst._

import WRType._ // manual fix
import WEither._ // manual fix

object CCostedPrim extends EntityObject("CCostedPrim") {
  case class CCostedPrimCtor[Val]
      (override val value: Rep[Val], override val cost: Rep[Int], override val dataSize: Rep[Long])
    extends CCostedPrim[Val](value, cost, dataSize) with Def[CCostedPrim[Val]] {
    implicit lazy val eVal = value.elem

    lazy val selfType = element[CCostedPrim[Val]]
  }
  // elem for concrete class
  class CCostedPrimElem[Val](val iso: Iso[CCostedPrimData[Val], CCostedPrim[Val]])(implicit override val eVal: Elem[Val])
    extends CostedPrimElem[Val, CCostedPrim[Val]]
    with ConcreteElem[CCostedPrimData[Val], CCostedPrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPrimElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override def convertCostedPrim(x: Rep[CostedPrim[Val]]) = RCCostedPrim(x.value, x.cost, x.dataSize)
    override def getDefaultRep = RCCostedPrim(element[Val].defaultRepValue, 0, 0l)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CCostedPrim[Val]]
    }
  }

  // state representation type
  type CCostedPrimData[Val] = (Val, (Int, Long))

  // 3) Iso for concrete class
  class CCostedPrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CCostedPrimData[Val], CCostedPrim[Val]] with Def[CCostedPrimIso[Val]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedPrim[Val]] => (p.value, p.cost, p.dataSize) }
    override def from(p: Rep[CCostedPrim[Val]]) =
      tryConvert[CCostedPrim[Val], (Val, (Int, Long))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Val, (Int, Long))]) = {
      val Pair(value, Pair(cost, dataSize)) = p
      RCCostedPrim(value, cost, dataSize)
    }
    lazy val eFrom = pairElement(element[Val], pairElement(element[Int], element[Long]))
    lazy val eTo = new CCostedPrimElem[Val](self)
    lazy val selfType = new CCostedPrimIsoElem[Val](eVal)
    def productArity = 1
    def productElement(n: Int) = eVal
  }
  case class CCostedPrimIsoElem[Val](eVal: Elem[Val]) extends Elem[CCostedPrimIso[Val]] {
    def getDefaultRep = reifyObject(new CCostedPrimIso[Val]()(eVal))
    lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CCostedPrimIso[Val]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedPrimCompanionCtor extends CompanionDef[CCostedPrimCompanionCtor] with CCostedPrimCompanion {
    def selfType = CCostedPrimCompanionElem
    override def toString = "CCostedPrimCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val](p: Rep[CCostedPrimData[Val]]): Rep[CCostedPrim[Val]] = {
      implicit val eVal = p._1.elem
      isoCCostedPrim[Val].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val](value: Rep[Val], cost: Rep[Int], dataSize: Rep[Long]): Rep[CCostedPrim[Val]] =
      mkCCostedPrim(value, cost, dataSize)

    def unapply[Val](p: Rep[CostedPrim[Val]]) = unmkCCostedPrim(p)
  }
  lazy val CCostedPrimRep: Rep[CCostedPrimCompanionCtor] = new CCostedPrimCompanionCtor
  lazy val RCCostedPrim: CCostedPrimCompanionCtor = proxyCCostedPrimCompanion(CCostedPrimRep)
  implicit def proxyCCostedPrimCompanion(p: Rep[CCostedPrimCompanionCtor]): CCostedPrimCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedPrimCompanionCtor])
      p.rhs.asInstanceOf[CCostedPrimCompanionCtor]
    else
      proxyOps[CCostedPrimCompanionCtor](p)
  }

  implicit case object CCostedPrimCompanionElem extends CompanionElem[CCostedPrimCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedPrimCompanionCtor]
    protected def getDefaultRep = CCostedPrimRep
  }

  implicit def proxyCCostedPrim[Val](p: Rep[CCostedPrim[Val]]): CCostedPrim[Val] =
    proxyOps[CCostedPrim[Val]](p)

  implicit class ExtendedCCostedPrim[Val](p: Rep[CCostedPrim[Val]]) {
    def toData: Rep[CCostedPrimData[Val]] = {
      implicit val eVal = p.value.elem
      isoCCostedPrim(eVal).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedPrim[Val](implicit eVal: Elem[Val]): Iso[CCostedPrimData[Val], CCostedPrim[Val]] =
    reifyObject(new CCostedPrimIso[Val]()(eVal))

  def mkCCostedPrim[Val]
    (value: Rep[Val], cost: Rep[Int], dataSize: Rep[Long]): Rep[CCostedPrim[Val]] = {
    new CCostedPrimCtor[Val](value, cost, dataSize)
  }
  def unmkCCostedPrim[Val](p: Rep[CostedPrim[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPrimElem[Val] @unchecked =>
      Some((asRep[CCostedPrim[Val]](p).value, asRep[CCostedPrim[Val]](p).cost, asRep[CCostedPrim[Val]](p).dataSize))
    case _ =>
      None
  }

    object CCostedPrimMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPrimElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedPrimCompanionMethods {
  }
} // of object CCostedPrim
  registerEntityObject("CCostedPrim", CCostedPrim)

object CCostedPair extends EntityObject("CCostedPair") {
  case class CCostedPairCtor[L, R]
      (override val l: Rep[Costed[L]], override val r: Rep[Costed[R]])
    extends CCostedPair[L, R](l, r) with Def[CCostedPair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[CCostedPair[L, R]]
  }
  // elem for concrete class
  class CCostedPairElem[L, R](val iso: Iso[CCostedPairData[L, R], CCostedPair[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends CostedPairElem[L, R, CCostedPair[L, R]]
    with ConcreteElem[CCostedPairData[L, R], CCostedPair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPairElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCostedPair(x: Rep[CostedPair[L, R]]) = RCCostedPair(x.l, x.r)
    override def getDefaultRep = RCCostedPair(element[Costed[L]].defaultRepValue, element[Costed[R]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedPair[L, R]]
    }
  }

  // state representation type
  type CCostedPairData[L, R] = (Costed[L], Costed[R])

  // 3) Iso for concrete class
  class CCostedPairIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CCostedPairData[L, R], CCostedPair[L, R]] with Def[CCostedPairIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedPair[L, R]] => (p.l, p.r) }
    override def from(p: Rep[CCostedPair[L, R]]) =
      tryConvert[CCostedPair[L, R], (Costed[L], Costed[R])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[L], Costed[R])]) = {
      val Pair(l, r) = p
      RCCostedPair(l, r)
    }
    lazy val eFrom = pairElement(element[Costed[L]], element[Costed[R]])
    lazy val eTo = new CCostedPairElem[L, R](self)
    lazy val selfType = new CCostedPairIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CCostedPairIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CCostedPairIso[L, R]] {
    def getDefaultRep = reifyObject(new CCostedPairIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedPairIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedPairCompanionCtor extends CompanionDef[CCostedPairCompanionCtor] with CCostedPairCompanion {
    def selfType = CCostedPairCompanionElem
    override def toString = "CCostedPairCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CCostedPairData[L, R]]): Rep[CCostedPair[L, R]] = {
      implicit val eL = p._1.eVal;
implicit val eR = p._2.eVal
      isoCCostedPair[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Rep[Costed[L]], r: Rep[Costed[R]]): Rep[CCostedPair[L, R]] =
      mkCCostedPair(l, r)

    def unapply[L, R](p: Rep[CostedPair[L, R]]) = unmkCCostedPair(p)
  }
  lazy val CCostedPairRep: Rep[CCostedPairCompanionCtor] = new CCostedPairCompanionCtor
  lazy val RCCostedPair: CCostedPairCompanionCtor = proxyCCostedPairCompanion(CCostedPairRep)
  implicit def proxyCCostedPairCompanion(p: Rep[CCostedPairCompanionCtor]): CCostedPairCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedPairCompanionCtor])
      p.rhs.asInstanceOf[CCostedPairCompanionCtor]
    else
      proxyOps[CCostedPairCompanionCtor](p)
  }

  implicit case object CCostedPairCompanionElem extends CompanionElem[CCostedPairCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedPairCompanionCtor]
    protected def getDefaultRep = CCostedPairRep
  }

  implicit def proxyCCostedPair[L, R](p: Rep[CCostedPair[L, R]]): CCostedPair[L, R] =
    proxyOps[CCostedPair[L, R]](p)

  implicit class ExtendedCCostedPair[L, R](p: Rep[CCostedPair[L, R]]) {
    def toData: Rep[CCostedPairData[L, R]] = {
      implicit val eL = p.l.eVal;
implicit val eR = p.r.eVal
      isoCCostedPair(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedPair[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CCostedPairData[L, R], CCostedPair[L, R]] =
    reifyObject(new CCostedPairIso[L, R]()(eL, eR))

  def mkCCostedPair[L, R]
    (l: Rep[Costed[L]], r: Rep[Costed[R]]): Rep[CCostedPair[L, R]] = {
    new CCostedPairCtor[L, R](l, r)
  }
  def unmkCCostedPair[L, R](p: Rep[CostedPair[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPairElem[L, R] @unchecked =>
      Some((asRep[CCostedPair[L, R]](p).l, asRep[CCostedPair[L, R]](p).r))
    case _ =>
      None
  }

    object CCostedPairMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedPairCompanionMethods {
  }
} // of object CCostedPair
  registerEntityObject("CCostedPair", CCostedPair)

object CCostedSum extends EntityObject("CCostedSum") {
  case class CCostedSumCtor[L, R]
      (override val value: Rep[WEither[L, R]], override val left: Rep[Costed[Unit]], override val right: Rep[Costed[Unit]])
    extends CCostedSum[L, R](value, left, right) with Def[CCostedSum[L, R]] {
    implicit lazy val eL = value.eA;
implicit lazy val eR = value.eB
    override lazy val eVal: Elem[WEither[L, R]] = implicitly[Elem[WEither[L, R]]]
    lazy val selfType = element[CCostedSum[L, R]]
    private val thisClass = classOf[CostedSum[L, R]] // manual fix

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, isAdapterCall = false, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, isAdapterCall = false, element[Long]))
    }
  }
  // elem for concrete class
  class CCostedSumElem[L, R](val iso: Iso[CCostedSumData[L, R], CCostedSum[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends CostedSumElem[L, R, CCostedSum[L, R]]
    with ConcreteElem[CCostedSumData[L, R], CCostedSum[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedSumElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCostedSum(x: Rep[CostedSum[L, R]]) = RCCostedSum(x.value, x.left, x.right)
    override def getDefaultRep = RCCostedSum(element[WEither[L, R]].defaultRepValue, element[Costed[Unit]].defaultRepValue, element[Costed[Unit]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedSum[L, R]]
    }
  }

  // state representation type
  type CCostedSumData[L, R] = (WEither[L, R], (Costed[Unit], Costed[Unit]))

  // 3) Iso for concrete class
  class CCostedSumIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CCostedSumData[L, R], CCostedSum[L, R]] with Def[CCostedSumIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedSum[L, R]] => (p.value, p.left, p.right) }
    override def from(p: Rep[CCostedSum[L, R]]) =
      tryConvert[CCostedSum[L, R], (WEither[L, R], (Costed[Unit], Costed[Unit]))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WEither[L, R], (Costed[Unit], Costed[Unit]))]) = {
      val Pair(value, Pair(left, right)) = p
      RCCostedSum(value, left, right)
    }
    lazy val eFrom = pairElement(element[WEither[L, R]], pairElement(element[Costed[Unit]], element[Costed[Unit]]))
    lazy val eTo = new CCostedSumElem[L, R](self)
    lazy val selfType = new CCostedSumIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CCostedSumIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CCostedSumIso[L, R]] {
    def getDefaultRep = reifyObject(new CCostedSumIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedSumIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedSumCompanionCtor extends CompanionDef[CCostedSumCompanionCtor] with CCostedSumCompanion {
    def selfType = CCostedSumCompanionElem
    override def toString = "CCostedSumCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CCostedSumData[L, R]]): Rep[CCostedSum[L, R]] = {
      implicit val eL = p._1.eA;
implicit val eR = p._1.eB
      isoCCostedSum[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CCostedSum[L, R]] =
      mkCCostedSum(value, left, right)

    def unapply[L, R](p: Rep[CostedSum[L, R]]) = unmkCCostedSum(p)
  }
  lazy val CCostedSumRep: Rep[CCostedSumCompanionCtor] = new CCostedSumCompanionCtor
  lazy val RCCostedSum: CCostedSumCompanionCtor = proxyCCostedSumCompanion(CCostedSumRep)
  implicit def proxyCCostedSumCompanion(p: Rep[CCostedSumCompanionCtor]): CCostedSumCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedSumCompanionCtor])
      p.rhs.asInstanceOf[CCostedSumCompanionCtor]
    else
      proxyOps[CCostedSumCompanionCtor](p)
  }

  implicit case object CCostedSumCompanionElem extends CompanionElem[CCostedSumCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedSumCompanionCtor]
    protected def getDefaultRep = CCostedSumRep
  }

  implicit def proxyCCostedSum[L, R](p: Rep[CCostedSum[L, R]]): CCostedSum[L, R] =
    proxyOps[CCostedSum[L, R]](p)

  implicit class ExtendedCCostedSum[L, R](p: Rep[CCostedSum[L, R]]) {
    def toData: Rep[CCostedSumData[L, R]] = {
      implicit val eL = p.value.eA;
implicit val eR = p.value.eB
      isoCCostedSum(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedSum[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CCostedSumData[L, R], CCostedSum[L, R]] =
    reifyObject(new CCostedSumIso[L, R]()(eL, eR))

  def mkCCostedSum[L, R]
    (value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CCostedSum[L, R]] = {
    new CCostedSumCtor[L, R](value, left, right)
  }
  def unmkCCostedSum[L, R](p: Rep[CostedSum[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedSumElem[L, R] @unchecked =>
      Some((asRep[CCostedSum[L, R]](p).value, asRep[CCostedSum[L, R]](p).left, asRep[CCostedSum[L, R]](p).right))
    case _ =>
      None
  }

    object CCostedSumMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedSumElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedSumElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedSumElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedSumCompanionMethods {
  }
} // of object CCostedSum
  registerEntityObject("CCostedSum", CCostedSum)

object CCostedFunc extends EntityObject("CCostedFunc") {
  case class CCostedFuncCtor[Env, Arg, Res]
      (override val envCosted: Rep[Costed[Env]], override val func: Rep[Costed[Arg] => Costed[Res]], override val cost: Rep[Int], override val dataSize: Rep[Long])
    extends CCostedFunc[Env, Arg, Res](envCosted, func, cost, dataSize) with Def[CCostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = envCosted.eVal;
implicit lazy val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit lazy val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val selfType = element[CCostedFunc[Env, Arg, Res]]
    private val thisClass = classOf[CostedFunc[Env, Arg, Res]] // manual fix

    override def value: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(self,
        thisClass.getMethod("value"),
        List(),
        true, isAdapterCall = false, element[Arg => Res]))
    }
  }
  // elem for concrete class
  class CCostedFuncElem[Env, Arg, Res](val iso: Iso[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]])(implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends CostedFuncElem[Env, Arg, Res, CCostedFunc[Env, Arg, Res]]
    with ConcreteElem[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(costedFuncElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override def convertCostedFunc(x: Rep[CostedFunc[Env, Arg, Res]]) = RCCostedFunc(x.envCosted, x.func, x.cost, x.dataSize)
    override def getDefaultRep = RCCostedFunc(element[Costed[Env]].defaultRepValue, constFun[Costed[Arg], Costed[Res]](element[Costed[Res]].defaultRepValue), 0, 0l)
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CCostedFunc[Env, Arg, Res]]
    }
  }

  // state representation type
  type CCostedFuncData[Env, Arg, Res] = (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))

  // 3) Iso for concrete class
  class CCostedFuncIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] with Def[CCostedFuncIso[Env, Arg, Res]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedFunc[Env, Arg, Res]] => (p.envCosted, p.func, p.cost, p.dataSize) }
    override def from(p: Rep[CCostedFunc[Env, Arg, Res]]) =
      tryConvert[CCostedFunc[Env, Arg, Res], (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))]) = {
      val Pair(envCosted, Pair(func, Pair(cost, dataSize))) = p
      RCCostedFunc(envCosted, func, cost, dataSize)
    }
    lazy val eFrom = pairElement(element[Costed[Env]], pairElement(element[Costed[Arg] => Costed[Res]], pairElement(element[Int], element[Long])))
    lazy val eTo = new CCostedFuncElem[Env, Arg, Res](self)
    lazy val selfType = new CCostedFuncIsoElem[Env, Arg, Res](eEnv, eArg, eRes)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eEnv
      case 1 => eArg
      case 2 => eRes
    }
  }
  case class CCostedFuncIsoElem[Env, Arg, Res](eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]) extends Elem[CCostedFuncIso[Env, Arg, Res]] {
    def getDefaultRep = reifyObject(new CCostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))
    lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CCostedFuncIso[Env, Arg, Res]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedFuncCompanionCtor extends CompanionDef[CCostedFuncCompanionCtor] with CCostedFuncCompanion {
    def selfType = CCostedFuncCompanionElem
    override def toString = "CCostedFuncCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Rep[CCostedFuncData[Env, Arg, Res]]): Rep[CCostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = p._1.eVal;
implicit val eArg = p._2.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit val eRes = p._2.elem.eRange.typeArgs("Val")._1.asElem[Res]
      isoCCostedFunc[Env, Arg, Res].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CCostedFunc[Env, Arg, Res]] =
      mkCCostedFunc(envCosted, func, cost, dataSize)

    def unapply[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]) = unmkCCostedFunc(p)
  }
  lazy val CCostedFuncRep: Rep[CCostedFuncCompanionCtor] = new CCostedFuncCompanionCtor
  lazy val RCCostedFunc: CCostedFuncCompanionCtor = proxyCCostedFuncCompanion(CCostedFuncRep)
  implicit def proxyCCostedFuncCompanion(p: Rep[CCostedFuncCompanionCtor]): CCostedFuncCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedFuncCompanionCtor])
      p.rhs.asInstanceOf[CCostedFuncCompanionCtor]
    else
      proxyOps[CCostedFuncCompanionCtor](p)
  }

  implicit case object CCostedFuncCompanionElem extends CompanionElem[CCostedFuncCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedFuncCompanionCtor]
    protected def getDefaultRep = CCostedFuncRep
  }

  implicit def proxyCCostedFunc[Env, Arg, Res](p: Rep[CCostedFunc[Env, Arg, Res]]): CCostedFunc[Env, Arg, Res] =
    proxyOps[CCostedFunc[Env, Arg, Res]](p)

  implicit class ExtendedCCostedFunc[Env, Arg, Res](p: Rep[CCostedFunc[Env, Arg, Res]]) {
    def toData: Rep[CCostedFuncData[Env, Arg, Res]] = {
      implicit val eEnv = p.envCosted.eVal;
implicit val eArg = p.func.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit val eRes = p.func.elem.eRange.typeArgs("Val")._1.asElem[Res]
      isoCCostedFunc(eEnv, eArg, eRes).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedFunc[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Iso[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] =
    reifyObject(new CCostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))

  def mkCCostedFunc[Env, Arg, Res]
    (envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CCostedFunc[Env, Arg, Res]] = {
    new CCostedFuncCtor[Env, Arg, Res](envCosted, func, cost, dataSize)
  }
  def unmkCCostedFunc[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedFuncElem[Env, Arg, Res] @unchecked =>
      Some((asRep[CCostedFunc[Env, Arg, Res]](p).envCosted, asRep[CCostedFunc[Env, Arg, Res]](p).func, asRep[CCostedFunc[Env, Arg, Res]](p).cost, asRep[CCostedFunc[Env, Arg, Res]](p).dataSize))
    case _ =>
      None
  }

    object CCostedFuncMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedFuncCompanionMethods {
  }
} // of object CCostedFunc
  registerEntityObject("CCostedFunc", CCostedFunc)

object CCostedCol extends EntityObject("CCostedCol") {
  case class CCostedColCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Int]], override val sizes: Rep[Col[Long]], override val valuesCost: Rep[Int])
    extends CCostedCol[Item](values, costs, sizes, valuesCost) with Def[CCostedCol[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[Col[Item]] = implicitly[Elem[Col[Item]]]
    lazy val selfType = element[CCostedCol[Item]]
    private val thisClass = classOf[CostedCol[Item]] // manual fix

    override def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedCol[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedCol[Res]](mkMethodCall(self,
        thisClass.getMethod("mapCosted", classOf[Sym]),
        List(f),
        true, isAdapterCall = false, element[CostedCol[Res]]))
    }

    override def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedCol[Item]] = {
      asRep[CostedCol[Item]](mkMethodCall(self,
        thisClass.getMethod("filterCosted", classOf[Sym]),
        List(f),
        true, isAdapterCall = false, element[CostedCol[Item]]))
    }

    override def foldCosted[B](zero: Rep[Costed[B]], op: Rep[Costed[(B, Item)] => Costed[B]]): Rep[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(self,
        thisClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, isAdapterCall = false, element[Costed[B]]))
    }
  }
  // elem for concrete class
  class CCostedColElem[Item](val iso: Iso[CCostedColData[Item], CCostedCol[Item]])(implicit override val eItem: Elem[Item])
    extends CostedColElem[Item, CCostedCol[Item]]
    with ConcreteElem[CCostedColData[Item], CCostedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedColElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCostedCol(x: Rep[CostedCol[Item]]) = RCCostedCol(x.values, x.costs, x.sizes, x.valuesCost)
    override def getDefaultRep = RCCostedCol(element[Col[Item]].defaultRepValue, element[Col[Int]].defaultRepValue, element[Col[Long]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedCol[Item]]
    }
  }

  // state representation type
  type CCostedColData[Item] = (Col[Item], (Col[Int], (Col[Long], Int)))

  // 3) Iso for concrete class
  class CCostedColIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CCostedColData[Item], CCostedCol[Item]] with Def[CCostedColIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedCol[Item]] => (p.values, p.costs, p.sizes, p.valuesCost) }
    override def from(p: Rep[CCostedCol[Item]]) =
      tryConvert[CCostedCol[Item], (Col[Item], (Col[Int], (Col[Long], Int)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], (Col[Int], (Col[Long], Int)))]) = {
      val Pair(values, Pair(costs, Pair(sizes, valuesCost))) = p
      RCCostedCol(values, costs, sizes, valuesCost)
    }
    lazy val eFrom = pairElement(element[Col[Item]], pairElement(element[Col[Int]], pairElement(element[Col[Long]], element[Int])))
    lazy val eTo = new CCostedColElem[Item](self)
    lazy val selfType = new CCostedColIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CCostedColIsoElem[Item](eItem: Elem[Item]) extends Elem[CCostedColIso[Item]] {
    def getDefaultRep = reifyObject(new CCostedColIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedColIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedColCompanionCtor extends CompanionDef[CCostedColCompanionCtor] with CCostedColCompanion {
    def selfType = CCostedColCompanionElem
    override def toString = "CCostedColCompanion"
    @scalan.OverloadId("fromData")
    def apply[Item](p: Rep[CCostedColData[Item]]): Rep[CCostedCol[Item]] = {
      implicit val eItem = p._1.eA
      isoCCostedCol[Item].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]], valuesCost: Rep[Int]): Rep[CCostedCol[Item]] =
      mkCCostedCol(values, costs, sizes, valuesCost)

    def unapply[Item](p: Rep[CostedCol[Item]]) = unmkCCostedCol(p)
  }
  lazy val CCostedColRep: Rep[CCostedColCompanionCtor] = new CCostedColCompanionCtor
  lazy val RCCostedCol: CCostedColCompanionCtor = proxyCCostedColCompanion(CCostedColRep)
  implicit def proxyCCostedColCompanion(p: Rep[CCostedColCompanionCtor]): CCostedColCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedColCompanionCtor])
      p.rhs.asInstanceOf[CCostedColCompanionCtor]
    else
      proxyOps[CCostedColCompanionCtor](p)
  }

  implicit case object CCostedColCompanionElem extends CompanionElem[CCostedColCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedColCompanionCtor]
    protected def getDefaultRep = CCostedColRep
  }

  implicit def proxyCCostedCol[Item](p: Rep[CCostedCol[Item]]): CCostedCol[Item] =
    proxyOps[CCostedCol[Item]](p)

  implicit class ExtendedCCostedCol[Item](p: Rep[CCostedCol[Item]]) {
    def toData: Rep[CCostedColData[Item]] = {
      implicit val eItem = p.values.eA
      isoCCostedCol(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedCol[Item](implicit eItem: Elem[Item]): Iso[CCostedColData[Item], CCostedCol[Item]] =
    reifyObject(new CCostedColIso[Item]()(eItem))

  def mkCCostedCol[Item]
    (values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]], valuesCost: Rep[Int]): Rep[CCostedCol[Item]] = {
    new CCostedColCtor[Item](values, costs, sizes, valuesCost)
  }
  def unmkCCostedCol[Item](p: Rep[CostedCol[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedColElem[Item] @unchecked =>
      Some((asRep[CCostedCol[Item]](p).values, asRep[CCostedCol[Item]](p).costs, asRep[CCostedCol[Item]](p).sizes, asRep[CCostedCol[Item]](p).valuesCost))
    case _ =>
      None
  }

    object CCostedColMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "mapCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "filterCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedColElem[_]] && method.getName == "foldCosted" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedColCompanionMethods {
  }
} // of object CCostedCol
  registerEntityObject("CCostedCol", CCostedCol)

object CCostedPairCol extends EntityObject("CCostedPairCol") {
  case class CCostedPairColCtor[L, R]
      (override val ls: Rep[Costed[Col[L]]], override val rs: Rep[Costed[Col[R]]])
    extends CCostedPairCol[L, R](ls, rs) with Def[CCostedPairCol[L, R]] {
    implicit lazy val eL = ls.eVal.typeArgs("A")._1.asElem[L];
implicit lazy val eR = rs.eVal.typeArgs("A")._1.asElem[R]
    override lazy val eVal: Elem[Col[(L, R)]] = implicitly[Elem[Col[(L, R)]]]
    lazy val selfType = element[CCostedPairCol[L, R]]
  }
  // elem for concrete class
  class CCostedPairColElem[L, R](val iso: Iso[CCostedPairColData[L, R], CCostedPairCol[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends CostedPairColElem[L, R, CCostedPairCol[L, R]]
    with ConcreteElem[CCostedPairColData[L, R], CCostedPairCol[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPairColElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCostedPairCol(x: Rep[CostedPairCol[L, R]]) = RCCostedPairCol(x.ls, x.rs)
    override def getDefaultRep = RCCostedPairCol(element[Costed[Col[L]]].defaultRepValue, element[Costed[Col[R]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedPairCol[L, R]]
    }
  }

  // state representation type
  type CCostedPairColData[L, R] = (Costed[Col[L]], Costed[Col[R]])

  // 3) Iso for concrete class
  class CCostedPairColIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CCostedPairColData[L, R], CCostedPairCol[L, R]] with Def[CCostedPairColIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedPairCol[L, R]] => (p.ls, p.rs) }
    override def from(p: Rep[CCostedPairCol[L, R]]) =
      tryConvert[CCostedPairCol[L, R], (Costed[Col[L]], Costed[Col[R]])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Col[L]], Costed[Col[R]])]) = {
      val Pair(ls, rs) = p
      RCCostedPairCol(ls, rs)
    }
    lazy val eFrom = pairElement(element[Costed[Col[L]]], element[Costed[Col[R]]])
    lazy val eTo = new CCostedPairColElem[L, R](self)
    lazy val selfType = new CCostedPairColIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CCostedPairColIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CCostedPairColIso[L, R]] {
    def getDefaultRep = reifyObject(new CCostedPairColIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedPairColIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedPairColCompanionCtor extends CompanionDef[CCostedPairColCompanionCtor] with CCostedPairColCompanion {
    def selfType = CCostedPairColCompanionElem
    override def toString = "CCostedPairColCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CCostedPairColData[L, R]]): Rep[CCostedPairCol[L, R]] = {
      implicit val eL = p._1.eVal.typeArgs("A")._1.asElem[L];
implicit val eR = p._2.eVal.typeArgs("A")._1.asElem[R]
      isoCCostedPairCol[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](ls: Rep[Costed[Col[L]]], rs: Rep[Costed[Col[R]]]): Rep[CCostedPairCol[L, R]] =
      mkCCostedPairCol(ls, rs)

    def unapply[L, R](p: Rep[CostedPairCol[L, R]]) = unmkCCostedPairCol(p)
  }
  lazy val CCostedPairColRep: Rep[CCostedPairColCompanionCtor] = new CCostedPairColCompanionCtor
  lazy val RCCostedPairCol: CCostedPairColCompanionCtor = proxyCCostedPairColCompanion(CCostedPairColRep)
  implicit def proxyCCostedPairColCompanion(p: Rep[CCostedPairColCompanionCtor]): CCostedPairColCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedPairColCompanionCtor])
      p.rhs.asInstanceOf[CCostedPairColCompanionCtor]
    else
      proxyOps[CCostedPairColCompanionCtor](p)
  }

  implicit case object CCostedPairColCompanionElem extends CompanionElem[CCostedPairColCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedPairColCompanionCtor]
    protected def getDefaultRep = CCostedPairColRep
  }

  implicit def proxyCCostedPairCol[L, R](p: Rep[CCostedPairCol[L, R]]): CCostedPairCol[L, R] =
    proxyOps[CCostedPairCol[L, R]](p)

  implicit class ExtendedCCostedPairCol[L, R](p: Rep[CCostedPairCol[L, R]]) {
    def toData: Rep[CCostedPairColData[L, R]] = {
      implicit val eL = p.ls.eVal.typeArgs("A")._1.asElem[L];
implicit val eR = p.rs.eVal.typeArgs("A")._1.asElem[R]
      isoCCostedPairCol(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedPairCol[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CCostedPairColData[L, R], CCostedPairCol[L, R]] =
    reifyObject(new CCostedPairColIso[L, R]()(eL, eR))

  def mkCCostedPairCol[L, R]
    (ls: Rep[Costed[Col[L]]], rs: Rep[Costed[Col[R]]]): Rep[CCostedPairCol[L, R]] = {
    new CCostedPairColCtor[L, R](ls, rs)
  }
  def unmkCCostedPairCol[L, R](p: Rep[CostedPairCol[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPairColElem[L, R] @unchecked =>
      Some((asRep[CCostedPairCol[L, R]](p).ls, asRep[CCostedPairCol[L, R]](p).rs))
    case _ =>
      None
  }

    object CCostedPairColMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairColElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairColElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairColElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairColElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedPairColCompanionMethods {
  }
} // of object CCostedPairCol
  registerEntityObject("CCostedPairCol", CCostedPairCol)

object CCostedNestedCol extends EntityObject("CCostedNestedCol") {
  case class CCostedNestedColCtor[Item]
      (override val rows: Rep[Col[Costed[Col[Item]]]])
    extends CCostedNestedCol[Item](rows) with Def[CCostedNestedCol[Item]] {
    implicit lazy val eItem = rows.eA.typeArgs("Val")._1.asElem[Col[Item]].typeArgs("A")._1.asElem[Item]
    override lazy val eVal: Elem[Col[Col[Item]]] = implicitly[Elem[Col[Col[Item]]]]
    lazy val selfType = element[CCostedNestedCol[Item]]
    private val thisClass = classOf[CostedNestedCol[Item]] // manual fix

    override def value: Rep[Col[Col[Item]]] = {
      asRep[Col[Col[Item]]](mkMethodCall(self,
        thisClass.getMethod("value"),
        List(),
        true, isAdapterCall = false, element[Col[Col[Item]]]))
    }

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, isAdapterCall = false, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, isAdapterCall = false, element[Long]))
    }
  }
  // elem for concrete class
  class CCostedNestedColElem[Item](val iso: Iso[CCostedNestedColData[Item], CCostedNestedCol[Item]])(implicit override val eItem: Elem[Item])
    extends CostedNestedColElem[Item, CCostedNestedCol[Item]]
    with ConcreteElem[CCostedNestedColData[Item], CCostedNestedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedNestedColElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCostedNestedCol(x: Rep[CostedNestedCol[Item]]) = RCCostedNestedCol(x.rows)
    override def getDefaultRep = RCCostedNestedCol(element[Col[Costed[Col[Item]]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedNestedCol[Item]]
    }
  }

  // state representation type
  type CCostedNestedColData[Item] = Col[Costed[Col[Item]]]

  // 3) Iso for concrete class
  class CCostedNestedColIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CCostedNestedColData[Item], CCostedNestedCol[Item]] with Def[CCostedNestedColIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CCostedNestedCol[Item]] => p.rows }
    override def from(p: Rep[CCostedNestedCol[Item]]) =
      tryConvert[CCostedNestedCol[Item], Col[Costed[Col[Item]]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Col[Costed[Col[Item]]]]) = {
      val rows = p
      RCCostedNestedCol(rows)
    }
    lazy val eFrom = element[Col[Costed[Col[Item]]]]
    lazy val eTo = new CCostedNestedColElem[Item](self)
    lazy val selfType = new CCostedNestedColIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CCostedNestedColIsoElem[Item](eItem: Elem[Item]) extends Elem[CCostedNestedColIso[Item]] {
    def getDefaultRep = reifyObject(new CCostedNestedColIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedNestedColIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedNestedColCompanionCtor extends CompanionDef[CCostedNestedColCompanionCtor] with CCostedNestedColCompanion {
    def selfType = CCostedNestedColCompanionElem
    override def toString = "CCostedNestedColCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](rows: Rep[Col[Costed[Col[Item]]]]): Rep[CCostedNestedCol[Item]] =
      mkCCostedNestedCol(rows)

    def unapply[Item](p: Rep[CostedNestedCol[Item]]) = unmkCCostedNestedCol(p)
  }
  lazy val CCostedNestedColRep: Rep[CCostedNestedColCompanionCtor] = new CCostedNestedColCompanionCtor
  lazy val RCCostedNestedCol: CCostedNestedColCompanionCtor = proxyCCostedNestedColCompanion(CCostedNestedColRep)
  implicit def proxyCCostedNestedColCompanion(p: Rep[CCostedNestedColCompanionCtor]): CCostedNestedColCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedNestedColCompanionCtor])
      p.rhs.asInstanceOf[CCostedNestedColCompanionCtor]
    else
      proxyOps[CCostedNestedColCompanionCtor](p)
  }

  implicit case object CCostedNestedColCompanionElem extends CompanionElem[CCostedNestedColCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedNestedColCompanionCtor]
    protected def getDefaultRep = CCostedNestedColRep
  }

  implicit def proxyCCostedNestedCol[Item](p: Rep[CCostedNestedCol[Item]]): CCostedNestedCol[Item] =
    proxyOps[CCostedNestedCol[Item]](p)

  implicit class ExtendedCCostedNestedCol[Item](p: Rep[CCostedNestedCol[Item]]) {
    def toData: Rep[CCostedNestedColData[Item]] = {
      implicit val eItem = p.rows.eA.typeArgs("Val")._1.asElem[Col[Item]].typeArgs("A")._1.asElem[Item]
      isoCCostedNestedCol(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedNestedCol[Item](implicit eItem: Elem[Item]): Iso[CCostedNestedColData[Item], CCostedNestedCol[Item]] =
    reifyObject(new CCostedNestedColIso[Item]()(eItem))

  def mkCCostedNestedCol[Item]
    (rows: Rep[Col[Costed[Col[Item]]]]): Rep[CCostedNestedCol[Item]] = {
    new CCostedNestedColCtor[Item](rows)
  }
  def unmkCCostedNestedCol[Item](p: Rep[CostedNestedCol[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedNestedColElem[Item] @unchecked =>
      Some((asRep[CCostedNestedCol[Item]](p).rows))
    case _ =>
      None
  }

    object CCostedNestedColMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedNestedColElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedNestedColElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedNestedColElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedNestedColElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedNestedColCompanionMethods {
  }
} // of object CCostedNestedCol
  registerEntityObject("CCostedNestedCol", CCostedNestedCol)

object CCostedBuilder extends EntityObject("CCostedBuilder") {
  case class CCostedBuilderCtor
      ()
    extends CCostedBuilder() with Def[CCostedBuilder] {
    lazy val selfType = element[CCostedBuilder]
    private val thisClass = classOf[CostedBuilder] // manual fix

    override def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(self,
        thisClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        List(x, optCost),
        true, isAdapterCall = false, element[Costed[T]]))
    }

    override def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(self,
        thisClass.getMethod("defaultValue", classOf[Sym]),
        List(valueType),
        true, isAdapterCall = false, element[T]))
    }
  }
  // elem for concrete class
  class CCostedBuilderElem(val iso: Iso[CCostedBuilderData, CCostedBuilder])
    extends CostedBuilderElem[CCostedBuilder]
    with ConcreteElem[CCostedBuilderData, CCostedBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(costedBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCostedBuilder(x: Rep[CostedBuilder]) = RCCostedBuilder()
    override def getDefaultRep = RCCostedBuilder()
    override lazy val tag = {
      weakTypeTag[CCostedBuilder]
    }
  }

  // state representation type
  type CCostedBuilderData = Unit

  // 3) Iso for concrete class
  class CCostedBuilderIso
    extends EntityIso[CCostedBuilderData, CCostedBuilder] with Def[CCostedBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[CCostedBuilder] => () }
    override def from(p: Rep[CCostedBuilder]) =
      tryConvert[CCostedBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RCCostedBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new CCostedBuilderElem(self)
    lazy val selfType = new CCostedBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CCostedBuilderIsoElem() extends Elem[CCostedBuilderIso] {
    def getDefaultRep = reifyObject(new CCostedBuilderIso())
    lazy val tag = {
      weakTypeTag[CCostedBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CCostedBuilderCompanionCtor extends CompanionDef[CCostedBuilderCompanionCtor] with CCostedBuilderCompanion {
    def selfType = CCostedBuilderCompanionElem
    override def toString = "CCostedBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CCostedBuilderData]): Rep[CCostedBuilder] = {
      isoCCostedBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[CCostedBuilder] =
      mkCCostedBuilder()

    def unapply(p: Rep[CostedBuilder]) = unmkCCostedBuilder(p)
  }
  lazy val CCostedBuilderRep: Rep[CCostedBuilderCompanionCtor] = new CCostedBuilderCompanionCtor
  lazy val RCCostedBuilder: CCostedBuilderCompanionCtor = proxyCCostedBuilderCompanion(CCostedBuilderRep)
  implicit def proxyCCostedBuilderCompanion(p: Rep[CCostedBuilderCompanionCtor]): CCostedBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedBuilderCompanionCtor])
      p.rhs.asInstanceOf[CCostedBuilderCompanionCtor]
    else
      proxyOps[CCostedBuilderCompanionCtor](p)
  }

  implicit case object CCostedBuilderCompanionElem extends CompanionElem[CCostedBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedBuilderCompanionCtor]
    protected def getDefaultRep = CCostedBuilderRep
  }

  implicit def proxyCCostedBuilder(p: Rep[CCostedBuilder]): CCostedBuilder =
    proxyOps[CCostedBuilder](p)

  implicit class ExtendedCCostedBuilder(p: Rep[CCostedBuilder]) {
    def toData: Rep[CCostedBuilderData] = {
      isoCCostedBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedBuilder: Iso[CCostedBuilderData, CCostedBuilder] =
    reifyObject(new CCostedBuilderIso())

  def mkCCostedBuilder
    (): Rep[CCostedBuilder] = {
    new CCostedBuilderCtor()
  }
  def unmkCCostedBuilder(p: Rep[CostedBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object CCostedBuilderMethods {
    object monoidBuilder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "monoidBuilder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costedValue {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "costedValue" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object defaultValue {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "defaultValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedPrim" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedPair" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedSum {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedSum" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedCol {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedCol" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPairCol {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedPairCol" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedNestedCol {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedNestedCol" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedSome {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedSome" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedNone {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedNone" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedOption" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedBuilderCompanionMethods {
  }
} // of object CCostedBuilder
  registerEntityObject("CCostedBuilder", CCostedBuilder)

  registerModule(ConcreteCostsModule)
}

object ConcreteCostsModule extends scalan.ModuleInfo("special.collection", "ConcreteCosts")
}

trait ConcreteCostsModule extends special.collection.impl.ConcreteCostsDefs {self: Library =>}
