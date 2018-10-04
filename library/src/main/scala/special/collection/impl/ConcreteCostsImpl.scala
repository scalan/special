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
import ConcreteCostedBuilder._
import Costed._
import ConcreteCosted._
import WEither._
import WArray._
import WRType._
import Col._
import CostedCol._
import MonoidBuilderInst._
import CostedBuilder._
import CostedPrim._
import CostedPair._
import CostedSum._
import CostedFunc._
import CostedArray._
import CostedPairArray._
import CostedPairCol._
import CostedNestedArray._
import CostedNestedCol._

object ConcreteCosted extends EntityObject("ConcreteCosted") {
  // entityProxy: single proxy for each type family
  implicit def proxyConcreteCosted[Val](p: Rep[ConcreteCosted[Val]]): ConcreteCosted[Val] = {
    if (p.rhs.isInstanceOf[ConcreteCosted[Val]@unchecked]) p.rhs.asInstanceOf[ConcreteCosted[Val]]
    else
      proxyOps[ConcreteCosted[Val]](p)(scala.reflect.classTag[ConcreteCosted[Val]])
  }

  // familyElem
  class ConcreteCostedElem[Val, To <: ConcreteCosted[Val]](implicit _eVal: Elem[Val])
    extends CostedElem[Val, To] {
    override def eVal = _eVal

    override lazy val parent: Option[Elem[_]] = Some(costedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[ConcreteCosted[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ConcreteCosted[Val]] => convertConcreteCosted(x) }
      tryConvert(element[ConcreteCosted[Val]], this, x, conv)
    }

    def convertConcreteCosted(x: Rep[ConcreteCosted[Val]]): Rep[To] = {
      x.elem match {
        case _: ConcreteCostedElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ConcreteCostedElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def concreteCostedElement[Val](implicit eVal: Elem[Val]): Elem[ConcreteCosted[Val]] =
    cachedElem[ConcreteCostedElem[Val, ConcreteCosted[Val]]](eVal)

  implicit case object ConcreteCostedCompanionElem extends CompanionElem[ConcreteCostedCompanionCtor] {
    lazy val tag = weakTypeTag[ConcreteCostedCompanionCtor]
    protected def getDefaultRep = RConcreteCosted
  }

  abstract class ConcreteCostedCompanionCtor extends CompanionDef[ConcreteCostedCompanionCtor] with ConcreteCostedCompanion {
    def selfType = ConcreteCostedCompanionElem
    override def toString = "ConcreteCosted"
  }
  implicit def proxyConcreteCostedCompanionCtor(p: Rep[ConcreteCostedCompanionCtor]): ConcreteCostedCompanionCtor =
    proxyOps[ConcreteCostedCompanionCtor](p)

  lazy val RConcreteCosted: Rep[ConcreteCostedCompanionCtor] = new ConcreteCostedCompanionCtor {
  }

  object ConcreteCostedMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[ConcreteCosted[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConcreteCostedElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ConcreteCosted[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ConcreteCosted[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ConcreteCostedCompanionMethods {
  }
} // of object ConcreteCosted
  registerEntityObject("ConcreteCosted", ConcreteCosted)

object CostedPrim extends EntityObject("CostedPrim") {
  case class CostedPrimCtor[Val]
      (override val value: Rep[Val], override val cost: Rep[Int], override val dataSize: Rep[Long])
    extends CostedPrim[Val](value, cost, dataSize) with Def[CostedPrim[Val]] {
    implicit lazy val eVal = value.elem

    lazy val selfType = element[CostedPrim[Val]]
  }
  // elem for concrete class
  class CostedPrimElem[Val](val iso: Iso[CostedPrimData[Val], CostedPrim[Val]])(implicit override val eVal: Elem[Val])
    extends ConcreteCostedElem[Val, CostedPrim[Val]]
    with ConcreteElem[CostedPrimData[Val], CostedPrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[Val]]) = RCostedPrim(x.value, x.cost, x.dataSize)
    override def getDefaultRep = RCostedPrim(element[Val].defaultRepValue, 0, 0l)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedPrim[Val]]
    }
  }

  // state representation type
  type CostedPrimData[Val] = (Val, (Int, Long))

  // 3) Iso for concrete class
  class CostedPrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CostedPrimData[Val], CostedPrim[Val]] with Def[CostedPrimIso[Val]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPrim[Val]] => (p.value, p.cost, p.dataSize) }
    override def from(p: Rep[CostedPrim[Val]]) =
      tryConvert[CostedPrim[Val], (Val, (Int, Long))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Val, (Int, Long))]) = {
      val Pair(value, Pair(cost, dataSize)) = p
      RCostedPrim(value, cost, dataSize)
    }
    lazy val eFrom = pairElement(element[Val], pairElement(element[Int], element[Long]))
    lazy val eTo = new CostedPrimElem[Val](self)
    lazy val selfType = new CostedPrimIsoElem[Val](eVal)
    def productArity = 1
    def productElement(n: Int) = eVal
  }
  case class CostedPrimIsoElem[Val](eVal: Elem[Val]) extends Elem[CostedPrimIso[Val]] {
    def getDefaultRep = reifyObject(new CostedPrimIso[Val]()(eVal))
    lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedPrimIso[Val]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedPrimCompanionCtor extends CompanionDef[CostedPrimCompanionCtor] with CostedPrimCompanion {
    def selfType = CostedPrimCompanionElem
    override def toString = "CostedPrimCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val](p: Rep[CostedPrimData[Val]]): Rep[CostedPrim[Val]] = {
      implicit val eVal = p._1.elem
      isoCostedPrim[Val].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val](value: Rep[Val], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedPrim[Val]] =
      mkCostedPrim(value, cost, dataSize)

    def unapply[Val](p: Rep[ConcreteCosted[Val]]) = unmkCostedPrim(p)
  }
  lazy val CostedPrimRep: Rep[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor
  lazy val RCostedPrim: CostedPrimCompanionCtor = proxyCostedPrimCompanion(CostedPrimRep)
  implicit def proxyCostedPrimCompanion(p: Rep[CostedPrimCompanionCtor]): CostedPrimCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedPrimCompanionCtor])
      p.rhs.asInstanceOf[CostedPrimCompanionCtor]
    else
      proxyOps[CostedPrimCompanionCtor](p)
  }

  implicit case object CostedPrimCompanionElem extends CompanionElem[CostedPrimCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPrimCompanionCtor]
    protected def getDefaultRep = CostedPrimRep
  }

  implicit def proxyCostedPrim[Val](p: Rep[CostedPrim[Val]]): CostedPrim[Val] =
    proxyOps[CostedPrim[Val]](p)

  implicit class ExtendedCostedPrim[Val](p: Rep[CostedPrim[Val]]) {
    def toData: Rep[CostedPrimData[Val]] = {
      implicit val eVal = p.value.elem
      isoCostedPrim(eVal).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedPrim[Val](implicit eVal: Elem[Val]): Iso[CostedPrimData[Val], CostedPrim[Val]] =
    reifyObject(new CostedPrimIso[Val]()(eVal))

  def mkCostedPrim[Val]
    (value: Rep[Val], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedPrim[Val]] = {
    new CostedPrimCtor[Val](value, cost, dataSize)
  }
  def unmkCostedPrim[Val](p: Rep[ConcreteCosted[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPrimElem[Val] @unchecked =>
      Some((p.asRep[CostedPrim[Val]].value, p.asRep[CostedPrim[Val]].cost, p.asRep[CostedPrim[Val]].dataSize))
    case _ =>
      None
  }

    object CostedPrimMethods {
  }

  object CostedPrimCompanionMethods {
  }
} // of object CostedPrim
  registerEntityObject("CostedPrim", CostedPrim)

object CostedPair extends EntityObject("CostedPair") {
  case class CostedPairCtor[L, R]
      (override val l: Rep[Costed[L]], override val r: Rep[Costed[R]])
    extends CostedPair[L, R](l, r) with Def[CostedPair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[CostedPair[L, R]]
  }
  // elem for concrete class
  class CostedPairElem[L, R](val iso: Iso[CostedPairData[L, R], CostedPair[L, R]])(implicit val eL: Elem[L], val eR: Elem[R])
    extends ConcreteCostedElem[(L, R), CostedPair[L, R]]
    with ConcreteElem[CostedPairData[L, R], CostedPair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[(L, R)]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedPair: missing fields List(l, r)")
    override def getDefaultRep = RCostedPair(element[Costed[L]].defaultRepValue, element[Costed[R]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPair[L, R]]
    }
  }

  // state representation type
  type CostedPairData[L, R] = (Costed[L], Costed[R])

  // 3) Iso for concrete class
  class CostedPairIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CostedPairData[L, R], CostedPair[L, R]] with Def[CostedPairIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPair[L, R]] => (p.l, p.r) }
    override def from(p: Rep[CostedPair[L, R]]) =
      tryConvert[CostedPair[L, R], (Costed[L], Costed[R])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[L], Costed[R])]) = {
      val Pair(l, r) = p
      RCostedPair(l, r)
    }
    lazy val eFrom = pairElement(element[Costed[L]], element[Costed[R]])
    lazy val eTo = new CostedPairElem[L, R](self)
    lazy val selfType = new CostedPairIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CostedPairIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CostedPairIso[L, R]] {
    def getDefaultRep = reifyObject(new CostedPairIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedPairCompanionCtor extends CompanionDef[CostedPairCompanionCtor] with CostedPairCompanion {
    def selfType = CostedPairCompanionElem
    override def toString = "CostedPairCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CostedPairData[L, R]]): Rep[CostedPair[L, R]] = {
      implicit val eL = p._1.eVal;
implicit val eR = p._2.eVal
      isoCostedPair[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Rep[Costed[L]], r: Rep[Costed[R]]): Rep[CostedPair[L, R]] =
      mkCostedPair(l, r)

    def unapply[L, R](p: Rep[ConcreteCosted[(L, R)]]) = unmkCostedPair(p)
  }
  lazy val CostedPairRep: Rep[CostedPairCompanionCtor] = new CostedPairCompanionCtor
  lazy val RCostedPair: CostedPairCompanionCtor = proxyCostedPairCompanion(CostedPairRep)
  implicit def proxyCostedPairCompanion(p: Rep[CostedPairCompanionCtor]): CostedPairCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedPairCompanionCtor])
      p.rhs.asInstanceOf[CostedPairCompanionCtor]
    else
      proxyOps[CostedPairCompanionCtor](p)
  }

  implicit case object CostedPairCompanionElem extends CompanionElem[CostedPairCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairCompanionCtor]
    protected def getDefaultRep = CostedPairRep
  }

  implicit def proxyCostedPair[L, R](p: Rep[CostedPair[L, R]]): CostedPair[L, R] =
    proxyOps[CostedPair[L, R]](p)

  implicit class ExtendedCostedPair[L, R](p: Rep[CostedPair[L, R]]) {
    def toData: Rep[CostedPairData[L, R]] = {
      implicit val eL = p.l.eVal;
implicit val eR = p.r.eVal
      isoCostedPair(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedPair[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CostedPairData[L, R], CostedPair[L, R]] =
    reifyObject(new CostedPairIso[L, R]()(eL, eR))

  def mkCostedPair[L, R]
    (l: Rep[Costed[L]], r: Rep[Costed[R]]): Rep[CostedPair[L, R]] = {
    new CostedPairCtor[L, R](l, r)
  }
  def unmkCostedPair[L, R](p: Rep[ConcreteCosted[(L, R)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairElem[L, R] @unchecked =>
      Some((p.asRep[CostedPair[L, R]].l, p.asRep[CostedPair[L, R]].r))
    case _ =>
      None
  }

    object CostedPairMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedPairCompanionMethods {
  }
} // of object CostedPair
  registerEntityObject("CostedPair", CostedPair)

object CostedSum extends EntityObject("CostedSum") {
  case class CostedSumCtor[L, R]
      (override val value: Rep[WEither[L, R]], override val left: Rep[Costed[Unit]], override val right: Rep[Costed[Unit]])
    extends CostedSum[L, R](value, left, right) with Def[CostedSum[L, R]] {
    implicit lazy val eL = value.eA;
implicit lazy val eR = value.eB
    override lazy val eVal: Elem[WEither[L, R]] = implicitly[Elem[WEither[L, R]]]
    lazy val selfType = element[CostedSum[L, R]]

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        this.getClass.getMethod("cost"),
        List(),
        true, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        this.getClass.getMethod("dataSize"),
        List(),
        true, element[Long]))
    }
  }
  // elem for concrete class
  class CostedSumElem[L, R](val iso: Iso[CostedSumData[L, R], CostedSum[L, R]])(implicit val eL: Elem[L], val eR: Elem[R])
    extends ConcreteCostedElem[WEither[L, R], CostedSum[L, R]]
    with ConcreteElem[CostedSumData[L, R], CostedSum[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(wEitherElement(element[L], element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[WEither[L, R]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedSum: missing fields List(left, right)")
    override def getDefaultRep = RCostedSum(element[WEither[L, R]].defaultRepValue, element[Costed[Unit]].defaultRepValue, element[Costed[Unit]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedSum[L, R]]
    }
  }

  // state representation type
  type CostedSumData[L, R] = (WEither[L, R], (Costed[Unit], Costed[Unit]))

  // 3) Iso for concrete class
  class CostedSumIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CostedSumData[L, R], CostedSum[L, R]] with Def[CostedSumIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CostedSum[L, R]] => (p.value, p.left, p.right) }
    override def from(p: Rep[CostedSum[L, R]]) =
      tryConvert[CostedSum[L, R], (WEither[L, R], (Costed[Unit], Costed[Unit]))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WEither[L, R], (Costed[Unit], Costed[Unit]))]) = {
      val Pair(value, Pair(left, right)) = p
      RCostedSum(value, left, right)
    }
    lazy val eFrom = pairElement(element[WEither[L, R]], pairElement(element[Costed[Unit]], element[Costed[Unit]]))
    lazy val eTo = new CostedSumElem[L, R](self)
    lazy val selfType = new CostedSumIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CostedSumIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CostedSumIso[L, R]] {
    def getDefaultRep = reifyObject(new CostedSumIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedSumIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedSumCompanionCtor extends CompanionDef[CostedSumCompanionCtor] with CostedSumCompanion {
    def selfType = CostedSumCompanionElem
    override def toString = "CostedSumCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CostedSumData[L, R]]): Rep[CostedSum[L, R]] = {
      implicit val eL = p._1.eA;
implicit val eR = p._1.eB
      isoCostedSum[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CostedSum[L, R]] =
      mkCostedSum(value, left, right)

    def unapply[L, R](p: Rep[ConcreteCosted[WEither[L, R]]]) = unmkCostedSum(p)
  }
  lazy val CostedSumRep: Rep[CostedSumCompanionCtor] = new CostedSumCompanionCtor
  lazy val RCostedSum: CostedSumCompanionCtor = proxyCostedSumCompanion(CostedSumRep)
  implicit def proxyCostedSumCompanion(p: Rep[CostedSumCompanionCtor]): CostedSumCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedSumCompanionCtor])
      p.rhs.asInstanceOf[CostedSumCompanionCtor]
    else
      proxyOps[CostedSumCompanionCtor](p)
  }

  implicit case object CostedSumCompanionElem extends CompanionElem[CostedSumCompanionCtor] {
    lazy val tag = weakTypeTag[CostedSumCompanionCtor]
    protected def getDefaultRep = CostedSumRep
  }

  implicit def proxyCostedSum[L, R](p: Rep[CostedSum[L, R]]): CostedSum[L, R] =
    proxyOps[CostedSum[L, R]](p)

  implicit class ExtendedCostedSum[L, R](p: Rep[CostedSum[L, R]]) {
    def toData: Rep[CostedSumData[L, R]] = {
      implicit val eL = p.value.eA;
implicit val eR = p.value.eB
      isoCostedSum(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedSum[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CostedSumData[L, R], CostedSum[L, R]] =
    reifyObject(new CostedSumIso[L, R]()(eL, eR))

  def mkCostedSum[L, R]
    (value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CostedSum[L, R]] = {
    new CostedSumCtor[L, R](value, left, right)
  }
  def unmkCostedSum[L, R](p: Rep[ConcreteCosted[WEither[L, R]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedSumElem[L, R] @unchecked =>
      Some((p.asRep[CostedSum[L, R]].value, p.asRep[CostedSum[L, R]].left, p.asRep[CostedSum[L, R]].right))
    case _ =>
      None
  }

    object CostedSumMethods {
    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSumElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSumElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedSumCompanionMethods {
  }
} // of object CostedSum
  registerEntityObject("CostedSum", CostedSum)

object CostedFunc extends EntityObject("CostedFunc") {
  case class CostedFuncCtor[Env, Arg, Res]
      (override val envCosted: Rep[Costed[Env]], override val func: Rep[Costed[Arg] => Costed[Res]], override val cost: Rep[Int], override val dataSize: Rep[Long])
    extends CostedFunc[Env, Arg, Res](envCosted, func, cost, dataSize) with Def[CostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = envCosted.eVal;
implicit lazy val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit lazy val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val selfType = element[CostedFunc[Env, Arg, Res]]

    override def value: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(self,
        this.getClass.getMethod("value"),
        List(),
        true, element[Arg => Res]))
    }
  }
  // elem for concrete class
  class CostedFuncElem[Env, Arg, Res](val iso: Iso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]])(implicit val eEnv: Elem[Env], val eArg: Elem[Arg], val eRes: Elem[Res])
    extends ConcreteCostedElem[Arg => Res, CostedFunc[Env, Arg, Res]]
    with ConcreteElem[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[Arg => Res]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedFunc: missing fields List(envCosted, func)")
    override def getDefaultRep = RCostedFunc(element[Costed[Env]].defaultRepValue, constFun[Costed[Arg], Costed[Res]](element[Costed[Res]].defaultRepValue), 0, 0l)
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CostedFunc[Env, Arg, Res]]
    }
  }

  // state representation type
  type CostedFuncData[Env, Arg, Res] = (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))

  // 3) Iso for concrete class
  class CostedFuncIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] with Def[CostedFuncIso[Env, Arg, Res]] {
    private lazy val _safeFrom = fun { p: Rep[CostedFunc[Env, Arg, Res]] => (p.envCosted, p.func, p.cost, p.dataSize) }
    override def from(p: Rep[CostedFunc[Env, Arg, Res]]) =
      tryConvert[CostedFunc[Env, Arg, Res], (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Env], (Costed[Arg] => Costed[Res], (Int, Long)))]) = {
      val Pair(envCosted, Pair(func, Pair(cost, dataSize))) = p
      RCostedFunc(envCosted, func, cost, dataSize)
    }
    lazy val eFrom = pairElement(element[Costed[Env]], pairElement(element[Costed[Arg] => Costed[Res]], pairElement(element[Int], element[Long])))
    lazy val eTo = new CostedFuncElem[Env, Arg, Res](self)
    lazy val selfType = new CostedFuncIsoElem[Env, Arg, Res](eEnv, eArg, eRes)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eEnv
      case 1 => eArg
      case 2 => eRes
    }
  }
  case class CostedFuncIsoElem[Env, Arg, Res](eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]) extends Elem[CostedFuncIso[Env, Arg, Res]] {
    def getDefaultRep = reifyObject(new CostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))
    lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CostedFuncIso[Env, Arg, Res]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedFuncCompanionCtor extends CompanionDef[CostedFuncCompanionCtor] with CostedFuncCompanion {
    def selfType = CostedFuncCompanionElem
    override def toString = "CostedFuncCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Rep[CostedFuncData[Env, Arg, Res]]): Rep[CostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = p._1.eVal;
implicit val eArg = p._2.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit val eRes = p._2.elem.eRange.typeArgs("Val")._1.asElem[Res]
      isoCostedFunc[Env, Arg, Res].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedFunc[Env, Arg, Res]] =
      mkCostedFunc(envCosted, func, cost, dataSize)

    def unapply[Env, Arg, Res](p: Rep[ConcreteCosted[Arg => Res]]) = unmkCostedFunc(p)
  }
  lazy val CostedFuncRep: Rep[CostedFuncCompanionCtor] = new CostedFuncCompanionCtor
  lazy val RCostedFunc: CostedFuncCompanionCtor = proxyCostedFuncCompanion(CostedFuncRep)
  implicit def proxyCostedFuncCompanion(p: Rep[CostedFuncCompanionCtor]): CostedFuncCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedFuncCompanionCtor])
      p.rhs.asInstanceOf[CostedFuncCompanionCtor]
    else
      proxyOps[CostedFuncCompanionCtor](p)
  }

  implicit case object CostedFuncCompanionElem extends CompanionElem[CostedFuncCompanionCtor] {
    lazy val tag = weakTypeTag[CostedFuncCompanionCtor]
    protected def getDefaultRep = CostedFuncRep
  }

  implicit def proxyCostedFunc[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]): CostedFunc[Env, Arg, Res] =
    proxyOps[CostedFunc[Env, Arg, Res]](p)

  implicit class ExtendedCostedFunc[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]) {
    def toData: Rep[CostedFuncData[Env, Arg, Res]] = {
      implicit val eEnv = p.envCosted.eVal;
implicit val eArg = p.func.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit val eRes = p.func.elem.eRange.typeArgs("Val")._1.asElem[Res]
      isoCostedFunc(eEnv, eArg, eRes).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedFunc[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Iso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] =
    reifyObject(new CostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))

  def mkCostedFunc[Env, Arg, Res]
    (envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedFunc[Env, Arg, Res]] = {
    new CostedFuncCtor[Env, Arg, Res](envCosted, func, cost, dataSize)
  }
  def unmkCostedFunc[Env, Arg, Res](p: Rep[ConcreteCosted[Arg => Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedFuncElem[Env, Arg, Res] @unchecked =>
      Some((p.asRep[CostedFunc[Env, Arg, Res]].envCosted, p.asRep[CostedFunc[Env, Arg, Res]].func, p.asRep[CostedFunc[Env, Arg, Res]].cost, p.asRep[CostedFunc[Env, Arg, Res]].dataSize))
    case _ =>
      None
  }

    object CostedFuncMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedFuncCompanionMethods {
  }
} // of object CostedFunc
  registerEntityObject("CostedFunc", CostedFunc)

object CostedArray extends EntityObject("CostedArray") {
  case class CostedArrayCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Int]], override val sizes: Rep[Col[Long]])
    extends CostedArray[Item](values, costs, sizes) with Def[CostedArray[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[WArray[Item]] = implicitly[Elem[WArray[Item]]]
    lazy val selfType = element[CostedArray[Item]]
  }
  // elem for concrete class
  class CostedArrayElem[Item](val iso: Iso[CostedArrayData[Item], CostedArray[Item]])(implicit val eItem: Elem[Item])
    extends ConcreteCostedElem[WArray[Item], CostedArray[Item]]
    with ConcreteElem[CostedArrayData[Item], CostedArray[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(wArrayElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[WArray[Item]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedArray: missing fields List(values, costs, sizes)")
    override def getDefaultRep = RCostedArray(element[Col[Item]].defaultRepValue, element[Col[Int]].defaultRepValue, element[Col[Long]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedArray[Item]]
    }
  }

  // state representation type
  type CostedArrayData[Item] = (Col[Item], (Col[Int], Col[Long]))

  // 3) Iso for concrete class
  class CostedArrayIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedArrayData[Item], CostedArray[Item]] with Def[CostedArrayIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedArray[Item]] => (p.values, p.costs, p.sizes) }
    override def from(p: Rep[CostedArray[Item]]) =
      tryConvert[CostedArray[Item], (Col[Item], (Col[Int], Col[Long]))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], (Col[Int], Col[Long]))]) = {
      val Pair(values, Pair(costs, sizes)) = p
      RCostedArray(values, costs, sizes)
    }
    lazy val eFrom = pairElement(element[Col[Item]], pairElement(element[Col[Int]], element[Col[Long]]))
    lazy val eTo = new CostedArrayElem[Item](self)
    lazy val selfType = new CostedArrayIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CostedArrayIsoElem[Item](eItem: Elem[Item]) extends Elem[CostedArrayIso[Item]] {
    def getDefaultRep = reifyObject(new CostedArrayIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedArrayIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedArrayCompanionCtor extends CompanionDef[CostedArrayCompanionCtor] with CostedArrayCompanion {
    def selfType = CostedArrayCompanionElem
    override def toString = "CostedArrayCompanion"
    @scalan.OverloadId("fromData")
    def apply[Item](p: Rep[CostedArrayData[Item]]): Rep[CostedArray[Item]] = {
      implicit val eItem = p._1.eA
      isoCostedArray[Item].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]]): Rep[CostedArray[Item]] =
      mkCostedArray(values, costs, sizes)

    def unapply[Item](p: Rep[ConcreteCosted[WArray[Item]]]) = unmkCostedArray(p)
  }
  lazy val CostedArrayRep: Rep[CostedArrayCompanionCtor] = new CostedArrayCompanionCtor
  lazy val RCostedArray: CostedArrayCompanionCtor = proxyCostedArrayCompanion(CostedArrayRep)
  implicit def proxyCostedArrayCompanion(p: Rep[CostedArrayCompanionCtor]): CostedArrayCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedArrayCompanionCtor])
      p.rhs.asInstanceOf[CostedArrayCompanionCtor]
    else
      proxyOps[CostedArrayCompanionCtor](p)
  }

  implicit case object CostedArrayCompanionElem extends CompanionElem[CostedArrayCompanionCtor] {
    lazy val tag = weakTypeTag[CostedArrayCompanionCtor]
    protected def getDefaultRep = CostedArrayRep
  }

  implicit def proxyCostedArray[Item](p: Rep[CostedArray[Item]]): CostedArray[Item] =
    proxyOps[CostedArray[Item]](p)

  implicit class ExtendedCostedArray[Item](p: Rep[CostedArray[Item]]) {
    def toData: Rep[CostedArrayData[Item]] = {
      implicit val eItem = p.values.eA
      isoCostedArray(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedArray[Item](implicit eItem: Elem[Item]): Iso[CostedArrayData[Item], CostedArray[Item]] =
    reifyObject(new CostedArrayIso[Item]()(eItem))

  def mkCostedArray[Item]
    (values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]]): Rep[CostedArray[Item]] = {
    new CostedArrayCtor[Item](values, costs, sizes)
  }
  def unmkCostedArray[Item](p: Rep[ConcreteCosted[WArray[Item]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedArrayElem[Item] @unchecked =>
      Some((p.asRep[CostedArray[Item]].values, p.asRep[CostedArray[Item]].costs, p.asRep[CostedArray[Item]].sizes))
    case _ =>
      None
  }

    object CostedArrayMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedArrayCompanionMethods {
  }
} // of object CostedArray
  registerEntityObject("CostedArray", CostedArray)

object CostedCol extends EntityObject("CostedCol") {
  case class CostedColCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Int]], override val sizes: Rep[Col[Long]], override val valuesCost: Rep[Int])
    extends CostedCol[Item](values, costs, sizes, valuesCost) with Def[CostedCol[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[Col[Item]] = implicitly[Elem[Col[Item]]]
    lazy val selfType = element[CostedCol[Item]]

    override def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedCol[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedCol[Res]](mkMethodCall(self,
        this.getClass.getMethod("mapCosted", classOf[Sym]),
        List(f),
        true, element[CostedCol[Res]]))
    }

    override def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedCol[Item]] = {
      asRep[CostedCol[Item]](mkMethodCall(self,
        this.getClass.getMethod("filterCosted", classOf[Sym]),
        List(f),
        true, element[CostedCol[Item]]))
    }
  }
  // elem for concrete class
  class CostedColElem[Item](val iso: Iso[CostedColData[Item], CostedCol[Item]])(implicit val eItem: Elem[Item])
    extends ConcreteCostedElem[Col[Item], CostedCol[Item]]
    with ConcreteElem[CostedColData[Item], CostedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(colElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[Col[Item]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedCol: missing fields List(values, costs, sizes, valuesCost)")
    override def getDefaultRep = RCostedCol(element[Col[Item]].defaultRepValue, element[Col[Int]].defaultRepValue, element[Col[Long]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedCol[Item]]
    }
  }

  // state representation type
  type CostedColData[Item] = (Col[Item], (Col[Int], (Col[Long], Int)))

  // 3) Iso for concrete class
  class CostedColIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedColData[Item], CostedCol[Item]] with Def[CostedColIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedCol[Item]] => (p.values, p.costs, p.sizes, p.valuesCost) }
    override def from(p: Rep[CostedCol[Item]]) =
      tryConvert[CostedCol[Item], (Col[Item], (Col[Int], (Col[Long], Int)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], (Col[Int], (Col[Long], Int)))]) = {
      val Pair(values, Pair(costs, Pair(sizes, valuesCost))) = p
      RCostedCol(values, costs, sizes, valuesCost)
    }
    lazy val eFrom = pairElement(element[Col[Item]], pairElement(element[Col[Int]], pairElement(element[Col[Long]], element[Int])))
    lazy val eTo = new CostedColElem[Item](self)
    lazy val selfType = new CostedColIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CostedColIsoElem[Item](eItem: Elem[Item]) extends Elem[CostedColIso[Item]] {
    def getDefaultRep = reifyObject(new CostedColIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedColIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedColCompanionCtor extends CompanionDef[CostedColCompanionCtor] with CostedColCompanion {
    def selfType = CostedColCompanionElem
    override def toString = "CostedColCompanion"
    @scalan.OverloadId("fromData")
    def apply[Item](p: Rep[CostedColData[Item]]): Rep[CostedCol[Item]] = {
      implicit val eItem = p._1.eA
      isoCostedCol[Item].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]], valuesCost: Rep[Int]): Rep[CostedCol[Item]] =
      mkCostedCol(values, costs, sizes, valuesCost)

    def unapply[Item](p: Rep[ConcreteCosted[Col[Item]]]) = unmkCostedCol(p)
  }
  lazy val CostedColRep: Rep[CostedColCompanionCtor] = new CostedColCompanionCtor
  lazy val RCostedCol: CostedColCompanionCtor = proxyCostedColCompanion(CostedColRep)
  implicit def proxyCostedColCompanion(p: Rep[CostedColCompanionCtor]): CostedColCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedColCompanionCtor])
      p.rhs.asInstanceOf[CostedColCompanionCtor]
    else
      proxyOps[CostedColCompanionCtor](p)
  }

  implicit case object CostedColCompanionElem extends CompanionElem[CostedColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedColCompanionCtor]
    protected def getDefaultRep = CostedColRep
  }

  implicit def proxyCostedCol[Item](p: Rep[CostedCol[Item]]): CostedCol[Item] =
    proxyOps[CostedCol[Item]](p)

  implicit class ExtendedCostedCol[Item](p: Rep[CostedCol[Item]]) {
    def toData: Rep[CostedColData[Item]] = {
      implicit val eItem = p.values.eA
      isoCostedCol(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedCol[Item](implicit eItem: Elem[Item]): Iso[CostedColData[Item], CostedCol[Item]] =
    reifyObject(new CostedColIso[Item]()(eItem))

  def mkCostedCol[Item]
    (values: Rep[Col[Item]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]], valuesCost: Rep[Int]): Rep[CostedCol[Item]] = {
    new CostedColCtor[Item](values, costs, sizes, valuesCost)
  }
  def unmkCostedCol[Item](p: Rep[ConcreteCosted[Col[Item]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedColElem[Item] @unchecked =>
      Some((p.asRep[CostedCol[Item]].values, p.asRep[CostedCol[Item]].costs, p.asRep[CostedCol[Item]].sizes, p.asRep[CostedCol[Item]].valuesCost))
    case _ =>
      None
  }

    object CostedColMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "mapCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "filterCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedColCompanionMethods {
  }
} // of object CostedCol
  registerEntityObject("CostedCol", CostedCol)

object CostedPairArray extends EntityObject("CostedPairArray") {
  case class CostedPairArrayCtor[L, R]
      (override val ls: Rep[Costed[WArray[L]]], override val rs: Rep[Costed[WArray[R]]])
    extends CostedPairArray[L, R](ls, rs) with Def[CostedPairArray[L, R]] {
    implicit lazy val eL = ls.eVal.typeArgs("T")._1.asElem[L];
implicit lazy val eR = rs.eVal.typeArgs("T")._1.asElem[R]
    override lazy val eVal: Elem[WArray[(L, R)]] = implicitly[Elem[WArray[(L, R)]]]
    lazy val selfType = element[CostedPairArray[L, R]]
  }
  // elem for concrete class
  class CostedPairArrayElem[L, R](val iso: Iso[CostedPairArrayData[L, R], CostedPairArray[L, R]])(implicit val eL: Elem[L], val eR: Elem[R])
    extends ConcreteCostedElem[WArray[(L, R)], CostedPairArray[L, R]]
    with ConcreteElem[CostedPairArrayData[L, R], CostedPairArray[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(wArrayElement(pairElement(element[L],element[R]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[WArray[(L, R)]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedPairArray: missing fields List(ls, rs)")
    override def getDefaultRep = RCostedPairArray(element[Costed[WArray[L]]].defaultRepValue, element[Costed[WArray[R]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairArray[L, R]]
    }
  }

  // state representation type
  type CostedPairArrayData[L, R] = (Costed[WArray[L]], Costed[WArray[R]])

  // 3) Iso for concrete class
  class CostedPairArrayIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CostedPairArrayData[L, R], CostedPairArray[L, R]] with Def[CostedPairArrayIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPairArray[L, R]] => (p.ls, p.rs) }
    override def from(p: Rep[CostedPairArray[L, R]]) =
      tryConvert[CostedPairArray[L, R], (Costed[WArray[L]], Costed[WArray[R]])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[WArray[L]], Costed[WArray[R]])]) = {
      val Pair(ls, rs) = p
      RCostedPairArray(ls, rs)
    }
    lazy val eFrom = pairElement(element[Costed[WArray[L]]], element[Costed[WArray[R]]])
    lazy val eTo = new CostedPairArrayElem[L, R](self)
    lazy val selfType = new CostedPairArrayIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CostedPairArrayIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CostedPairArrayIso[L, R]] {
    def getDefaultRep = reifyObject(new CostedPairArrayIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairArrayIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedPairArrayCompanionCtor extends CompanionDef[CostedPairArrayCompanionCtor] with CostedPairArrayCompanion {
    def selfType = CostedPairArrayCompanionElem
    override def toString = "CostedPairArrayCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CostedPairArrayData[L, R]]): Rep[CostedPairArray[L, R]] = {
      implicit val eL = p._1.eVal.typeArgs("T")._1.asElem[L];
implicit val eR = p._2.eVal.typeArgs("T")._1.asElem[R]
      isoCostedPairArray[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](ls: Rep[Costed[WArray[L]]], rs: Rep[Costed[WArray[R]]]): Rep[CostedPairArray[L, R]] =
      mkCostedPairArray(ls, rs)

    def unapply[L, R](p: Rep[ConcreteCosted[WArray[(L, R)]]]) = unmkCostedPairArray(p)
  }
  lazy val CostedPairArrayRep: Rep[CostedPairArrayCompanionCtor] = new CostedPairArrayCompanionCtor
  lazy val RCostedPairArray: CostedPairArrayCompanionCtor = proxyCostedPairArrayCompanion(CostedPairArrayRep)
  implicit def proxyCostedPairArrayCompanion(p: Rep[CostedPairArrayCompanionCtor]): CostedPairArrayCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedPairArrayCompanionCtor])
      p.rhs.asInstanceOf[CostedPairArrayCompanionCtor]
    else
      proxyOps[CostedPairArrayCompanionCtor](p)
  }

  implicit case object CostedPairArrayCompanionElem extends CompanionElem[CostedPairArrayCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairArrayCompanionCtor]
    protected def getDefaultRep = CostedPairArrayRep
  }

  implicit def proxyCostedPairArray[L, R](p: Rep[CostedPairArray[L, R]]): CostedPairArray[L, R] =
    proxyOps[CostedPairArray[L, R]](p)

  implicit class ExtendedCostedPairArray[L, R](p: Rep[CostedPairArray[L, R]]) {
    def toData: Rep[CostedPairArrayData[L, R]] = {
      implicit val eL = p.ls.eVal.typeArgs("T")._1.asElem[L];
implicit val eR = p.rs.eVal.typeArgs("T")._1.asElem[R]
      isoCostedPairArray(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedPairArray[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CostedPairArrayData[L, R], CostedPairArray[L, R]] =
    reifyObject(new CostedPairArrayIso[L, R]()(eL, eR))

  def mkCostedPairArray[L, R]
    (ls: Rep[Costed[WArray[L]]], rs: Rep[Costed[WArray[R]]]): Rep[CostedPairArray[L, R]] = {
    new CostedPairArrayCtor[L, R](ls, rs)
  }
  def unmkCostedPairArray[L, R](p: Rep[ConcreteCosted[WArray[(L, R)]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairArrayElem[L, R] @unchecked =>
      Some((p.asRep[CostedPairArray[L, R]].ls, p.asRep[CostedPairArray[L, R]].rs))
    case _ =>
      None
  }

    object CostedPairArrayMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedPairArrayCompanionMethods {
  }
} // of object CostedPairArray
  registerEntityObject("CostedPairArray", CostedPairArray)

object CostedPairCol extends EntityObject("CostedPairCol") {
  case class CostedPairColCtor[L, R]
      (override val ls: Rep[Costed[Col[L]]], override val rs: Rep[Costed[Col[R]]])
    extends CostedPairCol[L, R](ls, rs) with Def[CostedPairCol[L, R]] {
    implicit lazy val eL = ls.eVal.typeArgs("A")._1.asElem[L];
implicit lazy val eR = rs.eVal.typeArgs("A")._1.asElem[R]
    override lazy val eVal: Elem[Col[(L, R)]] = implicitly[Elem[Col[(L, R)]]]
    lazy val selfType = element[CostedPairCol[L, R]]
  }
  // elem for concrete class
  class CostedPairColElem[L, R](val iso: Iso[CostedPairColData[L, R], CostedPairCol[L, R]])(implicit val eL: Elem[L], val eR: Elem[R])
    extends ConcreteCostedElem[Col[(L, R)], CostedPairCol[L, R]]
    with ConcreteElem[CostedPairColData[L, R], CostedPairCol[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(colElement(pairElement(element[L],element[R]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[Col[(L, R)]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedPairCol: missing fields List(ls, rs)")
    override def getDefaultRep = RCostedPairCol(element[Costed[Col[L]]].defaultRepValue, element[Costed[Col[R]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairCol[L, R]]
    }
  }

  // state representation type
  type CostedPairColData[L, R] = (Costed[Col[L]], Costed[Col[R]])

  // 3) Iso for concrete class
  class CostedPairColIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CostedPairColData[L, R], CostedPairCol[L, R]] with Def[CostedPairColIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPairCol[L, R]] => (p.ls, p.rs) }
    override def from(p: Rep[CostedPairCol[L, R]]) =
      tryConvert[CostedPairCol[L, R], (Costed[Col[L]], Costed[Col[R]])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Col[L]], Costed[Col[R]])]) = {
      val Pair(ls, rs) = p
      RCostedPairCol(ls, rs)
    }
    lazy val eFrom = pairElement(element[Costed[Col[L]]], element[Costed[Col[R]]])
    lazy val eTo = new CostedPairColElem[L, R](self)
    lazy val selfType = new CostedPairColIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CostedPairColIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CostedPairColIso[L, R]] {
    def getDefaultRep = reifyObject(new CostedPairColIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairColIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedPairColCompanionCtor extends CompanionDef[CostedPairColCompanionCtor] with CostedPairColCompanion {
    def selfType = CostedPairColCompanionElem
    override def toString = "CostedPairColCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[CostedPairColData[L, R]]): Rep[CostedPairCol[L, R]] = {
      implicit val eL = p._1.eVal.typeArgs("A")._1.asElem[L];
implicit val eR = p._2.eVal.typeArgs("A")._1.asElem[R]
      isoCostedPairCol[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](ls: Rep[Costed[Col[L]]], rs: Rep[Costed[Col[R]]]): Rep[CostedPairCol[L, R]] =
      mkCostedPairCol(ls, rs)

    def unapply[L, R](p: Rep[ConcreteCosted[Col[(L, R)]]]) = unmkCostedPairCol(p)
  }
  lazy val CostedPairColRep: Rep[CostedPairColCompanionCtor] = new CostedPairColCompanionCtor
  lazy val RCostedPairCol: CostedPairColCompanionCtor = proxyCostedPairColCompanion(CostedPairColRep)
  implicit def proxyCostedPairColCompanion(p: Rep[CostedPairColCompanionCtor]): CostedPairColCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedPairColCompanionCtor])
      p.rhs.asInstanceOf[CostedPairColCompanionCtor]
    else
      proxyOps[CostedPairColCompanionCtor](p)
  }

  implicit case object CostedPairColCompanionElem extends CompanionElem[CostedPairColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairColCompanionCtor]
    protected def getDefaultRep = CostedPairColRep
  }

  implicit def proxyCostedPairCol[L, R](p: Rep[CostedPairCol[L, R]]): CostedPairCol[L, R] =
    proxyOps[CostedPairCol[L, R]](p)

  implicit class ExtendedCostedPairCol[L, R](p: Rep[CostedPairCol[L, R]]) {
    def toData: Rep[CostedPairColData[L, R]] = {
      implicit val eL = p.ls.eVal.typeArgs("A")._1.asElem[L];
implicit val eR = p.rs.eVal.typeArgs("A")._1.asElem[R]
      isoCostedPairCol(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedPairCol[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CostedPairColData[L, R], CostedPairCol[L, R]] =
    reifyObject(new CostedPairColIso[L, R]()(eL, eR))

  def mkCostedPairCol[L, R]
    (ls: Rep[Costed[Col[L]]], rs: Rep[Costed[Col[R]]]): Rep[CostedPairCol[L, R]] = {
    new CostedPairColCtor[L, R](ls, rs)
  }
  def unmkCostedPairCol[L, R](p: Rep[ConcreteCosted[Col[(L, R)]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairColElem[L, R] @unchecked =>
      Some((p.asRep[CostedPairCol[L, R]].ls, p.asRep[CostedPairCol[L, R]].rs))
    case _ =>
      None
  }

    object CostedPairColMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedPairColCompanionMethods {
  }
} // of object CostedPairCol
  registerEntityObject("CostedPairCol", CostedPairCol)

object CostedNestedArray extends EntityObject("CostedNestedArray") {
  case class CostedNestedArrayCtor[Item]
      (override val rows: Rep[Col[Costed[WArray[Item]]]])
    extends CostedNestedArray[Item](rows) with Def[CostedNestedArray[Item]] {
    implicit lazy val eItem = rows.eA.typeArgs("Val")._1.asElem[WArray[Item]].typeArgs("T")._1.asElem[Item]
    override lazy val eVal: Elem[WArray[WArray[Item]]] = implicitly[Elem[WArray[WArray[Item]]]]
    lazy val selfType = element[CostedNestedArray[Item]]

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        this.getClass.getMethod("cost"),
        List(),
        true, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        this.getClass.getMethod("dataSize"),
        List(),
        true, element[Long]))
    }
  }
  // elem for concrete class
  class CostedNestedArrayElem[Item](val iso: Iso[CostedNestedArrayData[Item], CostedNestedArray[Item]])(implicit val eItem: Elem[Item])
    extends ConcreteCostedElem[WArray[WArray[Item]], CostedNestedArray[Item]]
    with ConcreteElem[CostedNestedArrayData[Item], CostedNestedArray[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(wArrayElement(wArrayElement(element[Item]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[WArray[WArray[Item]]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedNestedArray: missing fields List(rows)")
    override def getDefaultRep = RCostedNestedArray(element[Col[Costed[WArray[Item]]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedNestedArray[Item]]
    }
  }

  // state representation type
  type CostedNestedArrayData[Item] = Col[Costed[WArray[Item]]]

  // 3) Iso for concrete class
  class CostedNestedArrayIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedNestedArrayData[Item], CostedNestedArray[Item]] with Def[CostedNestedArrayIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedNestedArray[Item]] => p.rows }
    override def from(p: Rep[CostedNestedArray[Item]]) =
      tryConvert[CostedNestedArray[Item], Col[Costed[WArray[Item]]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Col[Costed[WArray[Item]]]]) = {
      val rows = p
      RCostedNestedArray(rows)
    }
    lazy val eFrom = element[Col[Costed[WArray[Item]]]]
    lazy val eTo = new CostedNestedArrayElem[Item](self)
    lazy val selfType = new CostedNestedArrayIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CostedNestedArrayIsoElem[Item](eItem: Elem[Item]) extends Elem[CostedNestedArrayIso[Item]] {
    def getDefaultRep = reifyObject(new CostedNestedArrayIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedNestedArrayIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedNestedArrayCompanionCtor extends CompanionDef[CostedNestedArrayCompanionCtor] with CostedNestedArrayCompanion {
    def selfType = CostedNestedArrayCompanionElem
    override def toString = "CostedNestedArrayCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](rows: Rep[Col[Costed[WArray[Item]]]]): Rep[CostedNestedArray[Item]] =
      mkCostedNestedArray(rows)

    def unapply[Item](p: Rep[ConcreteCosted[WArray[WArray[Item]]]]) = unmkCostedNestedArray(p)
  }
  lazy val CostedNestedArrayRep: Rep[CostedNestedArrayCompanionCtor] = new CostedNestedArrayCompanionCtor
  lazy val RCostedNestedArray: CostedNestedArrayCompanionCtor = proxyCostedNestedArrayCompanion(CostedNestedArrayRep)
  implicit def proxyCostedNestedArrayCompanion(p: Rep[CostedNestedArrayCompanionCtor]): CostedNestedArrayCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedNestedArrayCompanionCtor])
      p.rhs.asInstanceOf[CostedNestedArrayCompanionCtor]
    else
      proxyOps[CostedNestedArrayCompanionCtor](p)
  }

  implicit case object CostedNestedArrayCompanionElem extends CompanionElem[CostedNestedArrayCompanionCtor] {
    lazy val tag = weakTypeTag[CostedNestedArrayCompanionCtor]
    protected def getDefaultRep = CostedNestedArrayRep
  }

  implicit def proxyCostedNestedArray[Item](p: Rep[CostedNestedArray[Item]]): CostedNestedArray[Item] =
    proxyOps[CostedNestedArray[Item]](p)

  implicit class ExtendedCostedNestedArray[Item](p: Rep[CostedNestedArray[Item]]) {
    def toData: Rep[CostedNestedArrayData[Item]] = {
      implicit val eItem = p.rows.eA.typeArgs("Val")._1.asElem[WArray[Item]].typeArgs("T")._1.asElem[Item]
      isoCostedNestedArray(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedNestedArray[Item](implicit eItem: Elem[Item]): Iso[CostedNestedArrayData[Item], CostedNestedArray[Item]] =
    reifyObject(new CostedNestedArrayIso[Item]()(eItem))

  def mkCostedNestedArray[Item]
    (rows: Rep[Col[Costed[WArray[Item]]]]): Rep[CostedNestedArray[Item]] = {
    new CostedNestedArrayCtor[Item](rows)
  }
  def unmkCostedNestedArray[Item](p: Rep[ConcreteCosted[WArray[WArray[Item]]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedNestedArrayElem[Item] @unchecked =>
      Some((p.asRep[CostedNestedArray[Item]].rows))
    case _ =>
      None
  }

    object CostedNestedArrayMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedNestedArrayCompanionMethods {
  }
} // of object CostedNestedArray
  registerEntityObject("CostedNestedArray", CostedNestedArray)

object CostedNestedCol extends EntityObject("CostedNestedCol") {
  case class CostedNestedColCtor[Item]
      (override val rows: Rep[Col[Costed[Col[Item]]]])
    extends CostedNestedCol[Item](rows) with Def[CostedNestedCol[Item]] {
    implicit lazy val eItem = rows.eA.typeArgs("Val")._1.asElem[Col[Item]].typeArgs("A")._1.asElem[Item]
    override lazy val eVal: Elem[Col[Col[Item]]] = implicitly[Elem[Col[Col[Item]]]]
    lazy val selfType = element[CostedNestedCol[Item]]

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        this.getClass.getMethod("cost"),
        List(),
        true, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        this.getClass.getMethod("dataSize"),
        List(),
        true, element[Long]))
    }
  }
  // elem for concrete class
  class CostedNestedColElem[Item](val iso: Iso[CostedNestedColData[Item], CostedNestedCol[Item]])(implicit val eItem: Elem[Item])
    extends ConcreteCostedElem[Col[Col[Item]], CostedNestedCol[Item]]
    with ConcreteElem[CostedNestedColData[Item], CostedNestedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(concreteCostedElement(colElement(colElement(element[Item]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertConcreteCosted(x: Rep[ConcreteCosted[Col[Col[Item]]]]) = // Converter is not generated by meta
!!!("Cannot convert from ConcreteCosted to CostedNestedCol: missing fields List(rows)")
    override def getDefaultRep = RCostedNestedCol(element[Col[Costed[Col[Item]]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedNestedCol[Item]]
    }
  }

  // state representation type
  type CostedNestedColData[Item] = Col[Costed[Col[Item]]]

  // 3) Iso for concrete class
  class CostedNestedColIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedNestedColData[Item], CostedNestedCol[Item]] with Def[CostedNestedColIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedNestedCol[Item]] => p.rows }
    override def from(p: Rep[CostedNestedCol[Item]]) =
      tryConvert[CostedNestedCol[Item], Col[Costed[Col[Item]]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Col[Costed[Col[Item]]]]) = {
      val rows = p
      RCostedNestedCol(rows)
    }
    lazy val eFrom = element[Col[Costed[Col[Item]]]]
    lazy val eTo = new CostedNestedColElem[Item](self)
    lazy val selfType = new CostedNestedColIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CostedNestedColIsoElem[Item](eItem: Elem[Item]) extends Elem[CostedNestedColIso[Item]] {
    def getDefaultRep = reifyObject(new CostedNestedColIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedNestedColIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedNestedColCompanionCtor extends CompanionDef[CostedNestedColCompanionCtor] with CostedNestedColCompanion {
    def selfType = CostedNestedColCompanionElem
    override def toString = "CostedNestedColCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](rows: Rep[Col[Costed[Col[Item]]]]): Rep[CostedNestedCol[Item]] =
      mkCostedNestedCol(rows)

    def unapply[Item](p: Rep[ConcreteCosted[Col[Col[Item]]]]) = unmkCostedNestedCol(p)
  }
  lazy val CostedNestedColRep: Rep[CostedNestedColCompanionCtor] = new CostedNestedColCompanionCtor
  lazy val RCostedNestedCol: CostedNestedColCompanionCtor = proxyCostedNestedColCompanion(CostedNestedColRep)
  implicit def proxyCostedNestedColCompanion(p: Rep[CostedNestedColCompanionCtor]): CostedNestedColCompanionCtor = {
    if (p.rhs.isInstanceOf[CostedNestedColCompanionCtor])
      p.rhs.asInstanceOf[CostedNestedColCompanionCtor]
    else
      proxyOps[CostedNestedColCompanionCtor](p)
  }

  implicit case object CostedNestedColCompanionElem extends CompanionElem[CostedNestedColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedNestedColCompanionCtor]
    protected def getDefaultRep = CostedNestedColRep
  }

  implicit def proxyCostedNestedCol[Item](p: Rep[CostedNestedCol[Item]]): CostedNestedCol[Item] =
    proxyOps[CostedNestedCol[Item]](p)

  implicit class ExtendedCostedNestedCol[Item](p: Rep[CostedNestedCol[Item]]) {
    def toData: Rep[CostedNestedColData[Item]] = {
      implicit val eItem = p.rows.eA.typeArgs("Val")._1.asElem[Col[Item]].typeArgs("A")._1.asElem[Item]
      isoCostedNestedCol(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedNestedCol[Item](implicit eItem: Elem[Item]): Iso[CostedNestedColData[Item], CostedNestedCol[Item]] =
    reifyObject(new CostedNestedColIso[Item]()(eItem))

  def mkCostedNestedCol[Item]
    (rows: Rep[Col[Costed[Col[Item]]]]): Rep[CostedNestedCol[Item]] = {
    new CostedNestedColCtor[Item](rows)
  }
  def unmkCostedNestedCol[Item](p: Rep[ConcreteCosted[Col[Col[Item]]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedNestedColElem[Item] @unchecked =>
      Some((p.asRep[CostedNestedCol[Item]].rows))
    case _ =>
      None
  }

    object CostedNestedColMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedNestedColCompanionMethods {
  }
} // of object CostedNestedCol
  registerEntityObject("CostedNestedCol", CostedNestedCol)

object ConcreteCostedBuilder extends EntityObject("ConcreteCostedBuilder") {
  case class ConcreteCostedBuilderCtor
      ()
    extends ConcreteCostedBuilder() with Def[ConcreteCostedBuilder] {
    lazy val selfType = element[ConcreteCostedBuilder]

    override def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(self,
        this.getClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        List(x, optCost),
        true, element[Costed[T]]))
    }

    override def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(self,
        this.getClass.getMethod("defaultValue", classOf[Sym]),
        List(valueType),
        true, element[T]))
    }
  }
  // elem for concrete class
  class ConcreteCostedBuilderElem(val iso: Iso[ConcreteCostedBuilderData, ConcreteCostedBuilder])
    extends CostedBuilderElem[ConcreteCostedBuilder]
    with ConcreteElem[ConcreteCostedBuilderData, ConcreteCostedBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(costedBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertCostedBuilder(x: Rep[CostedBuilder]) = RConcreteCostedBuilder()
    override def getDefaultRep = RConcreteCostedBuilder()
    override lazy val tag = {
      weakTypeTag[ConcreteCostedBuilder]
    }
  }

  // state representation type
  type ConcreteCostedBuilderData = Unit

  // 3) Iso for concrete class
  class ConcreteCostedBuilderIso
    extends EntityIso[ConcreteCostedBuilderData, ConcreteCostedBuilder] with Def[ConcreteCostedBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[ConcreteCostedBuilder] => () }
    override def from(p: Rep[ConcreteCostedBuilder]) =
      tryConvert[ConcreteCostedBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RConcreteCostedBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ConcreteCostedBuilderElem(self)
    lazy val selfType = new ConcreteCostedBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ConcreteCostedBuilderIsoElem() extends Elem[ConcreteCostedBuilderIso] {
    def getDefaultRep = reifyObject(new ConcreteCostedBuilderIso())
    lazy val tag = {
      weakTypeTag[ConcreteCostedBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ConcreteCostedBuilderCompanionCtor extends CompanionDef[ConcreteCostedBuilderCompanionCtor] with ConcreteCostedBuilderCompanion {
    def selfType = ConcreteCostedBuilderCompanionElem
    override def toString = "ConcreteCostedBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ConcreteCostedBuilderData]): Rep[ConcreteCostedBuilder] = {
      isoConcreteCostedBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ConcreteCostedBuilder] =
      mkConcreteCostedBuilder()

    def unapply(p: Rep[CostedBuilder]) = unmkConcreteCostedBuilder(p)
  }
  lazy val ConcreteCostedBuilderRep: Rep[ConcreteCostedBuilderCompanionCtor] = new ConcreteCostedBuilderCompanionCtor
  lazy val RConcreteCostedBuilder: ConcreteCostedBuilderCompanionCtor = proxyConcreteCostedBuilderCompanion(ConcreteCostedBuilderRep)
  implicit def proxyConcreteCostedBuilderCompanion(p: Rep[ConcreteCostedBuilderCompanionCtor]): ConcreteCostedBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[ConcreteCostedBuilderCompanionCtor])
      p.rhs.asInstanceOf[ConcreteCostedBuilderCompanionCtor]
    else
      proxyOps[ConcreteCostedBuilderCompanionCtor](p)
  }

  implicit case object ConcreteCostedBuilderCompanionElem extends CompanionElem[ConcreteCostedBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[ConcreteCostedBuilderCompanionCtor]
    protected def getDefaultRep = ConcreteCostedBuilderRep
  }

  implicit def proxyConcreteCostedBuilder(p: Rep[ConcreteCostedBuilder]): ConcreteCostedBuilder =
    proxyOps[ConcreteCostedBuilder](p)

  implicit class ExtendedConcreteCostedBuilder(p: Rep[ConcreteCostedBuilder]) {
    def toData: Rep[ConcreteCostedBuilderData] = {
      isoConcreteCostedBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoConcreteCostedBuilder: Iso[ConcreteCostedBuilderData, ConcreteCostedBuilder] =
    reifyObject(new ConcreteCostedBuilderIso())

  def mkConcreteCostedBuilder
    (): Rep[ConcreteCostedBuilder] = {
    new ConcreteCostedBuilderCtor()
  }
  def unmkConcreteCostedBuilder(p: Rep[CostedBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConcreteCostedBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object ConcreteCostedBuilderMethods {
    object monoidBuilder {
      def unapply(d: Def[_]): Nullable[Rep[ConcreteCostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConcreteCostedBuilderElem] && method.getName == "monoidBuilder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ConcreteCostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ConcreteCostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costedValue {
      def unapply(d: Def[_]): Nullable[(Rep[ConcreteCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ConcreteCostedBuilderElem] && method.getName == "costedValue" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ConcreteCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ConcreteCostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object defaultValue {
      def unapply(d: Def[_]): Nullable[(Rep[ConcreteCostedBuilder], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ConcreteCostedBuilderElem] && method.getName == "defaultValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ConcreteCostedBuilder], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ConcreteCostedBuilder], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ConcreteCostedBuilderCompanionMethods {
  }
} // of object ConcreteCostedBuilder
  registerEntityObject("ConcreteCostedBuilder", ConcreteCostedBuilder)

  registerModule(ConcreteCostsModule)
}

object ConcreteCostsModule extends scalan.ModuleInfo("special.collection", "ConcreteCosts")
}

trait ConcreteCostsModule extends special.collection.impl.ConcreteCostsDefs {self: Library =>}
