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
import CCostedColl._
import CCostedFunc._
import CCostedOption._
import CCostedPair._
import CCostedPrim._
import CSizeColl._
import CSizeFunc._
import CSizeOption._
import CSizePair._
import CSizePrim._
import Coll._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedFunc._
import CostedOption._
import CostedPair._
import CostedPrim._
import MonoidBuilderInst._
import Size._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import WOption._
import WRType._
import WSpecialPredef._

object CCostedPrim extends EntityObject("CCostedPrim") {
  case class CCostedPrimCtor[Val]
      (override val value: Rep[Val], override val cost: Rep[Int], override val size: Rep[Size[Val]])
    extends CCostedPrim[Val](value, cost, size) with Def[CCostedPrim[Val]] {
    implicit lazy val eVal = value.elem

    lazy val selfType = element[CCostedPrim[Val]]
    override def transform(t: Transformer) = CCostedPrimCtor[Val](t(value), t(cost), t(size))
  }
  // elem for concrete class
  class CCostedPrimElem[Val](val iso: Iso[CCostedPrimData[Val], CCostedPrim[Val]])(implicit override val eVal: Elem[Val])
    extends CostedPrimElem[Val, CCostedPrim[Val]]
    with ConcreteElem[CCostedPrimData[Val], CCostedPrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPrimElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override def convertCostedPrim(x: Rep[CostedPrim[Val]]) = RCCostedPrim(x.value, x.cost, x.size)
    override def getDefaultRep = RCCostedPrim(element[Val].defaultRepValue, 0, element[Size[Val]].defaultRepValue)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CCostedPrim[Val]]
    }
  }

  // state representation type
  type CCostedPrimData[Val] = (Val, (Int, Size[Val]))

  // 3) Iso for concrete class
  class CCostedPrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CCostedPrimData[Val], CCostedPrim[Val]] with Def[CCostedPrimIso[Val]] {
    override def transform(t: Transformer) = new CCostedPrimIso[Val]()(eVal)
    private lazy val _safeFrom = fun { p: Rep[CCostedPrim[Val]] => (p.value, p.cost, p.size) }
    override def from(p: Rep[CCostedPrim[Val]]) =
      tryConvert[CCostedPrim[Val], (Val, (Int, Size[Val]))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Val, (Int, Size[Val]))]) = {
      val Pair(value, Pair(cost, size)) = p
      RCCostedPrim(value, cost, size)
    }
    lazy val eFrom = pairElement(element[Val], pairElement(element[Int], element[Size[Val]]))
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

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[Val](value: Rep[Val], cost: Rep[Int], size: Rep[Size[Val]]): Rep[CCostedPrim[Val]] = {
      assert(if (cost.rhs.isInstanceOf[OpCost]) value.rhs.nodeId == cost.rhs.asInstanceOf[OpCost].costedValueId else true,
        s"${value.rhs} value node id (${value.rhs.nodeId}) is not equal to OpCost.costedValueId (${cost.rhs.asInstanceOf[OpCost].costedValueId})")
      mkCCostedPrim(value, cost, size)
    }

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
    (value: Rep[Val], cost: Rep[Int], size: Rep[Size[Val]]): Rep[CCostedPrim[Val]] = {
    new CCostedPrimCtor[Val](value, cost, size)
  }
  def unmkCCostedPrim[Val](p: Rep[CostedPrim[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPrimElem[Val] @unchecked =>
      Some((asRep[CCostedPrim[Val]](p).value, asRep[CCostedPrim[Val]](p).cost, asRep[CCostedPrim[Val]](p).size))
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
      (override val l: Rep[Costed[L]], override val r: Rep[Costed[R]], override val accCost: Rep[Int])
    extends CCostedPair[L, R](l, r, accCost) with Def[CCostedPair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[CCostedPair[L, R]]
    override def transform(t: Transformer) = CCostedPairCtor[L, R](t(l), t(r), t(accCost))
    private val thisClass = classOf[CostedPair[_, _]]

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }
  }
  // elem for concrete class
  class CCostedPairElem[L, R](val iso: Iso[CCostedPairData[L, R], CCostedPair[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends CostedPairElem[L, R, CCostedPair[L, R]]
    with ConcreteElem[CCostedPairData[L, R], CCostedPair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPairElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCostedPair(x: Rep[CostedPair[L, R]]) = RCCostedPair(x.l, x.r, x.accCost)
    override def getDefaultRep = RCCostedPair(element[Costed[L]].defaultRepValue, element[Costed[R]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CCostedPair[L, R]]
    }
  }

  // state representation type
  type CCostedPairData[L, R] = (Costed[L], (Costed[R], Int))

  // 3) Iso for concrete class
  class CCostedPairIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CCostedPairData[L, R], CCostedPair[L, R]] with Def[CCostedPairIso[L, R]] {
    override def transform(t: Transformer) = new CCostedPairIso[L, R]()(eL, eR)
    private lazy val _safeFrom = fun { p: Rep[CCostedPair[L, R]] => (p.l, p.r, p.accCost) }
    override def from(p: Rep[CCostedPair[L, R]]) =
      tryConvert[CCostedPair[L, R], (Costed[L], (Costed[R], Int))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[L], (Costed[R], Int))]) = {
      val Pair(l, Pair(r, accCost)) = p
      RCCostedPair(l, r, accCost)
    }
    lazy val eFrom = pairElement(element[Costed[L]], pairElement(element[Costed[R]], element[Int]))
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

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Rep[Costed[L]], r: Rep[Costed[R]], accCost: Rep[Int]): Rep[CCostedPair[L, R]] = {
      val value = Pair(l, r)
      assert(if (accCost.rhs.isInstanceOf[OpCost]) value.rhs.nodeId == accCost.rhs.asInstanceOf[OpCost].costedValueId else true,
        s"${value.rhs} value node id (${value.rhs.nodeId}) is not equal to OpCost.costedValueId (${accCost.rhs.asInstanceOf[OpCost].costedValueId})")
      mkCCostedPair(l, r, accCost)
    }

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
    (l: Rep[Costed[L]], r: Rep[Costed[R]], accCost: Rep[Int]): Rep[CCostedPair[L, R]] = {
    new CCostedPairCtor[L, R](l, r, accCost)
  }
  def unmkCCostedPair[L, R](p: Rep[CostedPair[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPairElem[L, R] @unchecked =>
      Some((asRep[CCostedPair[L, R]](p).l, asRep[CCostedPair[L, R]](p).r, asRep[CCostedPair[L, R]](p).accCost))
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

    object size {
      def unapply(d: Def[_]): Nullable[Rep[CCostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedPairElem[_, _]] && method.getName == "size" =>
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

object CCostedFunc extends EntityObject("CCostedFunc") {
  case class CCostedFuncCtor[Env, Arg, Res]
      (override val envCosted: Rep[Costed[Env]], override val func: Rep[Costed[Arg] => Costed[Res]], override val cost: Rep[Int], override val size: Rep[Size[Arg => Res]])
    extends CCostedFunc[Env, Arg, Res](envCosted, func, cost, size) with Def[CCostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = envCosted.eVal;
implicit lazy val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg];
implicit lazy val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val selfType = element[CCostedFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CCostedFuncCtor[Env, Arg, Res](t(envCosted), t(func), t(cost), t(size))
    private val thisClass = classOf[CostedFunc[_, _, _]]

    override def value: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(self,
        thisClass.getMethod("value"),
        List(),
        true, false, element[Arg => Res]))
    }
  }
  // elem for concrete class
  class CCostedFuncElem[Env, Arg, Res](val iso: Iso[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]])(implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends CostedFuncElem[Env, Arg, Res, CCostedFunc[Env, Arg, Res]]
    with ConcreteElem[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(costedFuncElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override def convertCostedFunc(x: Rep[CostedFunc[Env, Arg, Res]]) = RCCostedFunc(x.envCosted, x.func, x.cost, x.size)
    override def getDefaultRep = RCCostedFunc(element[Costed[Env]].defaultRepValue, constFun[Costed[Arg], Costed[Res]](element[Costed[Res]].defaultRepValue), 0, element[Size[Arg => Res]].defaultRepValue)
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CCostedFunc[Env, Arg, Res]]
    }
  }

  // state representation type
  type CCostedFuncData[Env, Arg, Res] = (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Size[Arg => Res])))

  // 3) Iso for concrete class
  class CCostedFuncIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] with Def[CCostedFuncIso[Env, Arg, Res]] {
    override def transform(t: Transformer) = new CCostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes)
    private lazy val _safeFrom = fun { p: Rep[CCostedFunc[Env, Arg, Res]] => (p.envCosted, p.func, p.cost, p.size) }
    override def from(p: Rep[CCostedFunc[Env, Arg, Res]]) =
      tryConvert[CCostedFunc[Env, Arg, Res], (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Size[Arg => Res])))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Env], (Costed[Arg] => Costed[Res], (Int, Size[Arg => Res])))]) = {
      val Pair(envCosted, Pair(func, Pair(cost, size))) = p
      RCCostedFunc(envCosted, func, cost, size)
    }
    lazy val eFrom = pairElement(element[Costed[Env]], pairElement(element[Costed[Arg] => Costed[Res]], pairElement(element[Int], element[Size[Arg => Res]])))
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
    def apply[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], size: Rep[Size[Arg => Res]]): Rep[CCostedFunc[Env, Arg, Res]] =
      mkCCostedFunc(envCosted, func, cost, size)

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
    (envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], size: Rep[Size[Arg => Res]]): Rep[CCostedFunc[Env, Arg, Res]] = {
    new CCostedFuncCtor[Env, Arg, Res](envCosted, func, cost, size)
  }
  def unmkCCostedFunc[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedFuncElem[Env, Arg, Res] @unchecked =>
      Some((asRep[CCostedFunc[Env, Arg, Res]](p).envCosted, asRep[CCostedFunc[Env, Arg, Res]](p).func, asRep[CCostedFunc[Env, Arg, Res]](p).cost, asRep[CCostedFunc[Env, Arg, Res]](p).size))
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

    object sliceCalc {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "sliceCalc" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceCost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "sliceCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceCostEx {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "sliceCostEx" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceSize {
      def unapply(d: Def[_]): Nullable[Rep[CCostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedFuncElem[_, _, _]] && method.getName == "sliceSize" =>
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

object CCostedColl extends EntityObject("CCostedColl") {
  case class CCostedCollCtor[Item]
      (override val values: Rep[Coll[Item]], override val costs: Rep[Coll[Int]], override val sizes: Rep[Coll[Size[Item]]], override val valuesCost: Rep[Int])
    extends CCostedColl[Item](values, costs, sizes, valuesCost) with Def[CCostedColl[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    lazy val selfType = element[CCostedColl[Item]]
    override def transform(t: Transformer) = CCostedCollCtor[Item](t(values), t(costs), t(sizes), t(valuesCost))
    private val thisClass = classOf[CostedColl[_]]

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    override def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedColl[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedColl[Res]](mkMethodCall(self,
        thisClass.getMethod("mapCosted", classOf[Sym]),
        List(f),
        true, false, element[CostedColl[Res]]))
    }

    override def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedColl[Item]] = {
      asRep[CostedColl[Item]](mkMethodCall(self,
        thisClass.getMethod("filterCosted", classOf[Sym]),
        List(f),
        true, false, element[CostedColl[Item]]))
    }

    override def foldCosted[B](zero: Rep[Costed[B]], op: Rep[Costed[(B, Item)] => Costed[B]]): Rep[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(self,
        thisClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[Costed[B]]))
    }
  }
  // elem for concrete class
  class CCostedCollElem[Item](val iso: Iso[CCostedCollData[Item], CCostedColl[Item]])(implicit override val eItem: Elem[Item])
    extends CostedCollElem[Item, CCostedColl[Item]]
    with ConcreteElem[CCostedCollData[Item], CCostedColl[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedCollElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCostedColl(x: Rep[CostedColl[Item]]) = RCCostedColl(x.values, x.costs, x.sizes, x.valuesCost)
    override def getDefaultRep = RCCostedColl(element[Coll[Item]].defaultRepValue, element[Coll[Int]].defaultRepValue, element[Coll[Size[Item]]].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedColl[Item]]
    }
  }

  // state representation type
  type CCostedCollData[Item] = (Coll[Item], (Coll[Int], (Coll[Size[Item]], Int)))

  // 3) Iso for concrete class
  class CCostedCollIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CCostedCollData[Item], CCostedColl[Item]] with Def[CCostedCollIso[Item]] {
    override def transform(t: Transformer) = new CCostedCollIso[Item]()(eItem)
    private lazy val _safeFrom = fun { p: Rep[CCostedColl[Item]] => (p.values, p.costs, p.sizes, p.valuesCost) }
    override def from(p: Rep[CCostedColl[Item]]) =
      tryConvert[CCostedColl[Item], (Coll[Item], (Coll[Int], (Coll[Size[Item]], Int)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Coll[Item], (Coll[Int], (Coll[Size[Item]], Int)))]) = {
      val Pair(values, Pair(costs, Pair(sizes, valuesCost))) = p
      RCCostedColl(values, costs, sizes, valuesCost)
    }
    lazy val eFrom = pairElement(element[Coll[Item]], pairElement(element[Coll[Int]], pairElement(element[Coll[Size[Item]]], element[Int])))
    lazy val eTo = new CCostedCollElem[Item](self)
    lazy val selfType = new CCostedCollIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CCostedCollIsoElem[Item](eItem: Elem[Item]) extends Elem[CCostedCollIso[Item]] {
    def getDefaultRep = reifyObject(new CCostedCollIso[Item]()(eItem))
    lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CCostedCollIso[Item]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CCostedCollCompanionCtor extends CompanionDef[CCostedCollCompanionCtor] with CCostedCollCompanion {
    def selfType = CCostedCollCompanionElem
    override def toString = "CCostedCollCompanion"
    @scalan.OverloadId("fromData")
    def apply[Item](p: Rep[CCostedCollData[Item]]): Rep[CCostedColl[Item]] = {
      implicit val eItem = p._1.eA
      isoCCostedColl[Item].to(p)
    }

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[Item](values: Rep[Coll[Item]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[Item]]], valuesCost: Rep[Int]): Rep[CCostedColl[Item]] = {
      assert(if (costs.rhs.isInstanceOf[OpCost]) values.rhs.nodeId == costs.rhs.asInstanceOf[OpCost].costedValueId else true,
        s"${values.rhs} value node id (${values.rhs.nodeId}) is not equal to OpCost.costedValueId (${costs.rhs.asInstanceOf[OpCost].costedValueId})")
      mkCCostedColl(values, costs, sizes, valuesCost)
    }

    def unapply[Item](p: Rep[CostedColl[Item]]) = unmkCCostedColl(p)
  }
  lazy val CCostedCollRep: Rep[CCostedCollCompanionCtor] = new CCostedCollCompanionCtor
  lazy val RCCostedColl: CCostedCollCompanionCtor = proxyCCostedCollCompanion(CCostedCollRep)
  implicit def proxyCCostedCollCompanion(p: Rep[CCostedCollCompanionCtor]): CCostedCollCompanionCtor = {
    if (p.rhs.isInstanceOf[CCostedCollCompanionCtor])
      p.rhs.asInstanceOf[CCostedCollCompanionCtor]
    else
      proxyOps[CCostedCollCompanionCtor](p)
  }

  implicit case object CCostedCollCompanionElem extends CompanionElem[CCostedCollCompanionCtor] {
    lazy val tag = weakTypeTag[CCostedCollCompanionCtor]
    protected def getDefaultRep = CCostedCollRep
  }

  implicit def proxyCCostedColl[Item](p: Rep[CCostedColl[Item]]): CCostedColl[Item] =
    proxyOps[CCostedColl[Item]](p)

  implicit class ExtendedCCostedColl[Item](p: Rep[CCostedColl[Item]]) {
    def toData: Rep[CCostedCollData[Item]] = {
      implicit val eItem = p.values.eA
      isoCCostedColl(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCCostedColl[Item](implicit eItem: Elem[Item]): Iso[CCostedCollData[Item], CCostedColl[Item]] =
    reifyObject(new CCostedCollIso[Item]()(eItem))

  def mkCCostedColl[Item]
    (values: Rep[Coll[Item]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[Item]]], valuesCost: Rep[Int]): Rep[CCostedColl[Item]] = {
    new CCostedCollCtor[Item](values, costs, sizes, valuesCost)
  }
  def unmkCCostedColl[Item](p: Rep[CostedColl[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedCollElem[Item] @unchecked =>
      Some((asRep[CCostedColl[Item]](p).values, asRep[CCostedColl[Item]](p).costs, asRep[CCostedColl[Item]](p).sizes, asRep[CCostedColl[Item]](p).valuesCost))
    case _ =>
      None
  }

    object CCostedCollMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object size {
      def unapply(d: Def[_]): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "size" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CCostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CCostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "mapCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "filterCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedCollElem[_]] && method.getName == "foldCosted" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CCostedCollCompanionMethods {
  }
} // of object CCostedColl
  registerEntityObject("CCostedColl", CCostedColl)

object CCostedBuilder extends EntityObject("CCostedBuilder") {
  case class CCostedBuilderCtor
      ()
    extends CCostedBuilder() with Def[CCostedBuilder] {
    lazy val selfType = element[CCostedBuilder]
    override def transform(t: Transformer) = CCostedBuilderCtor()
    private val thisClass = classOf[CostedBuilder]

    override def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(self,
        thisClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        List(x, optCost),
        true, false, element[Costed[T]]))
    }

    override def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(self,
        thisClass.getMethod("defaultValue", classOf[Sym]),
        List(valueType),
        true, false, element[T]))
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
    override def transform(t: Transformer) = new CCostedBuilderIso()
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

    object mkSizePrim {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkSizePrim" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizePair {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkSizePair" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeColl {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkSizeColl" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkSizeFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeOption {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkSizeOption" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedPrim" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedPair" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedColl {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedColl" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CCostedBuilderElem] && method.getName == "mkCostedOption" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CCostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = exp match {
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
