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
import Closure._
import ConcreteCostedBuilder._
import Costed._
import WOption._
import WArray._
import WEither._
import Col._
import MonoidBuilderInst._
import CostedBuilder._
import CostedPrim._
import CostedPair._
import ClosureBase._
import CostedFunc._
import CostedOption._
import CostedArray._
import CostedCol._
import CostedPairArray._
import CostedPairCol._
import CostedNestedArray._
import CostedNestedCol._

object Closure extends EntityObject("Closure") {
  // entityProxy: single proxy for each type family
  implicit def proxyClosure[Env, Arg, Res](p: Rep[Closure[Env, Arg, Res]]): Closure[Env, Arg, Res] = {
    proxyOps[Closure[Env, Arg, Res]](p)(scala.reflect.classTag[Closure[Env, Arg, Res]])
  }

  // familyElem
  class ClosureElem[Env, Arg, Res, To <: Closure[Env, Arg, Res]](implicit _eEnv: Elem[Env], _eArg: Elem[Arg], _eRes: Elem[Res])
    extends EntityElem[To] {
    def eEnv = _eEnv
    def eArg = _eArg
    def eRes = _eRes
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[Closure[Env, Arg, Res]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Closure[Env, Arg, Res]] => convertClosure(x) }
      tryConvert(element[Closure[Env, Arg, Res]], this, x, conv)
    }

    def convertClosure(x: Rep[Closure[Env, Arg, Res]]): Rep[To] = {
      x.elem match {
        case _: ClosureElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ClosureElem[_, _, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def closureElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[Closure[Env, Arg, Res]] =
    cachedElem[ClosureElem[Env, Arg, Res, Closure[Env, Arg, Res]]](eEnv, eArg, eRes)

  implicit case object ClosureCompanionElem extends CompanionElem[ClosureCompanionCtor] {
    lazy val tag = weakTypeTag[ClosureCompanionCtor]
    protected def getDefaultRep = RClosure
  }

  abstract class ClosureCompanionCtor extends CompanionDef[ClosureCompanionCtor] with ClosureCompanion {
    def selfType = ClosureCompanionElem
    override def toString = "Closure"
  }
  implicit def proxyClosureCompanionCtor(p: Rep[ClosureCompanionCtor]): ClosureCompanionCtor =
    proxyOps[ClosureCompanionCtor](p)

  lazy val RClosure: Rep[ClosureCompanionCtor] = new ClosureCompanionCtor {
  }

  object ClosureMethods {
    object env {
      def unapply(d: Def[_]): Option[Rep[Closure[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ClosureElem[_, _, _, _]] && method.getName == "env" =>
          Some(receiver).asInstanceOf[Option[Rep[Closure[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Closure[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply_with_env {
      def unapply(d: Def[_]): Option[(Rep[Closure[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, Seq(e, a, _*), _) if receiver.elem.isInstanceOf[ClosureElem[_, _, _, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply_with_env" } =>
          Some((receiver, e, a)).asInstanceOf[Option[(Rep[Closure[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Closure[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_apply {
      def unapply(d: Def[_]): Option[(Rep[Closure[Env, Arg, Res]], Rep[Arg]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ClosureElem[_, _, _, _]] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply" } =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[Closure[Env, Arg, Res]], Rep[Arg]) forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Closure[Env, Arg, Res]], Rep[Arg]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ClosureCompanionMethods {
  }
} // of object Closure
  registerEntityObject("Closure", Closure)

object CostedPrim extends EntityObject("CostedPrim") {
  case class CostedPrimCtor[Val]
      (override val value: Rep[Val], override val cost: Rep[Int])
    extends CostedPrim[Val](value, cost) with Def[CostedPrim[Val]] {
    implicit lazy val eVal = value.elem

    lazy val selfType = element[CostedPrim[Val]]
  }
  // elem for concrete class
  class CostedPrimElem[Val](val iso: Iso[CostedPrimData[Val], CostedPrim[Val]])(implicit override val eVal: Elem[Val])
    extends CostedElem[Val, CostedPrim[Val]]
    with ConcreteElem[CostedPrimData[Val], CostedPrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[Val]]) = RCostedPrim(x.value, x.cost)
    override def getDefaultRep = RCostedPrim(element[Val].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedPrim[Val]]
    }
  }

  // state representation type
  type CostedPrimData[Val] = (Val, Int)

  // 3) Iso for concrete class
  class CostedPrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CostedPrimData[Val], CostedPrim[Val]] with Def[CostedPrimIso[Val]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPrim[Val]] => (p.value, p.cost) }
    override def from(p: Rep[CostedPrim[Val]]) =
      tryConvert[CostedPrim[Val], (Val, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Val, Int)]) = {
      val Pair(value, cost) = p
      RCostedPrim(value, cost)
    }
    lazy val eFrom = pairElement(element[Val], element[Int])
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
    def apply[Val](value: Rep[Val], cost: Rep[Int]): Rep[CostedPrim[Val]] =
      mkCostedPrim(value, cost)

    def unapply[Val](p: Rep[Costed[Val]]) = unmkCostedPrim(p)
  }
  lazy val CostedPrimRep: Rep[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor
  lazy val RCostedPrim: CostedPrimCompanionCtor = proxyCostedPrimCompanion(CostedPrimRep)
  implicit def proxyCostedPrimCompanion(p: Rep[CostedPrimCompanionCtor]): CostedPrimCompanionCtor = {
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
    (value: Rep[Val], cost: Rep[Int]): Rep[CostedPrim[Val]] = {
    new CostedPrimCtor[Val](value, cost)
  }
  def unmkCostedPrim[Val](p: Rep[Costed[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPrimElem[Val] @unchecked =>
      Some((p.asRep[CostedPrim[Val]].value, p.asRep[CostedPrim[Val]].cost))
    case _ =>
      None
  }

    object CostedPrimMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPrimElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPrim[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
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
    extends CostedElem[(L, R), CostedPair[L, R]]
    with ConcreteElem[CostedPairData[L, R], CostedPair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[(L, R)]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedPair: missing fields List(l, r)")
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

    def unapply[L, R](p: Rep[Costed[(L, R)]]) = unmkCostedPair(p)
  }
  lazy val CostedPairRep: Rep[CostedPairCompanionCtor] = new CostedPairCompanionCtor
  lazy val RCostedPair: CostedPairCompanionCtor = proxyCostedPairCompanion(CostedPairRep)
  implicit def proxyCostedPairCompanion(p: Rep[CostedPairCompanionCtor]): CostedPairCompanionCtor = {
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
  def unmkCostedPair[L, R](p: Rep[Costed[(L, R)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairElem[L, R] @unchecked =>
      Some((p.asRep[CostedPair[L, R]].l, p.asRep[CostedPair[L, R]].r))
    case _ =>
      None
  }

    object CostedPairMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedPairCompanionMethods {
  }
} // of object CostedPair
  registerEntityObject("CostedPair", CostedPair)

object ClosureBase extends EntityObject("ClosureBase") {
  case class ClosureBaseCtor[Env, Arg, Res]
      (override val env: Rep[Env], override val func: Rep[((Env, Arg)) => Res])
    extends ClosureBase[Env, Arg, Res](env, func) with Def[ClosureBase[Env, Arg, Res]] {
    implicit lazy val eEnv = env.elem;
implicit lazy val eArg = func.elem.eDom.eSnd;
implicit lazy val eRes = func.elem.eRange

    lazy val selfType = element[ClosureBase[Env, Arg, Res]]
  }
  // elem for concrete class
  class ClosureBaseElem[Env, Arg, Res](val iso: Iso[ClosureBaseData[Env, Arg, Res], ClosureBase[Env, Arg, Res]])(implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends ClosureElem[Env, Arg, Res, ClosureBase[Env, Arg, Res]]
    with ConcreteElem[ClosureBaseData[Env, Arg, Res], ClosureBase[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(closureElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override def convertClosure(x: Rep[Closure[Env, Arg, Res]]) = // Converter is not generated by meta
!!!("Cannot convert from Closure to ClosureBase: missing fields List(func)")
    override def getDefaultRep = RClosureBase(element[Env].defaultRepValue, constFun[(Env, Arg), Res](element[Res].defaultRepValue))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[ClosureBase[Env, Arg, Res]]
    }
  }

  // state representation type
  type ClosureBaseData[Env, Arg, Res] = (Env, ((Env, Arg)) => Res)

  // 3) Iso for concrete class
  class ClosureBaseIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[ClosureBaseData[Env, Arg, Res], ClosureBase[Env, Arg, Res]] with Def[ClosureBaseIso[Env, Arg, Res]] {
    private lazy val _safeFrom = fun { p: Rep[ClosureBase[Env, Arg, Res]] => (p.env, p.func) }
    override def from(p: Rep[ClosureBase[Env, Arg, Res]]) =
      tryConvert[ClosureBase[Env, Arg, Res], (Env, ((Env, Arg)) => Res)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Env, ((Env, Arg)) => Res)]) = {
      val Pair(env, func) = p
      RClosureBase(env, func)
    }
    lazy val eFrom = pairElement(element[Env], element[((Env, Arg)) => Res])
    lazy val eTo = new ClosureBaseElem[Env, Arg, Res](self)
    lazy val selfType = new ClosureBaseIsoElem[Env, Arg, Res](eEnv, eArg, eRes)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eEnv
      case 1 => eArg
      case 2 => eRes
    }
  }
  case class ClosureBaseIsoElem[Env, Arg, Res](eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]) extends Elem[ClosureBaseIso[Env, Arg, Res]] {
    def getDefaultRep = reifyObject(new ClosureBaseIso[Env, Arg, Res]()(eEnv, eArg, eRes))
    lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[ClosureBaseIso[Env, Arg, Res]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ClosureBaseCompanionCtor extends CompanionDef[ClosureBaseCompanionCtor] with ClosureBaseCompanion {
    def selfType = ClosureBaseCompanionElem
    override def toString = "ClosureBaseCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Rep[ClosureBaseData[Env, Arg, Res]]): Rep[ClosureBase[Env, Arg, Res]] = {
      implicit val eEnv = p._1.elem;
implicit val eArg = p._2.elem.eDom.eSnd;
implicit val eRes = p._2.elem.eRange
      isoClosureBase[Env, Arg, Res].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](env: Rep[Env], func: Rep[((Env, Arg)) => Res]): Rep[ClosureBase[Env, Arg, Res]] =
      mkClosureBase(env, func)

    def unapply[Env, Arg, Res](p: Rep[Closure[Env, Arg, Res]]) = unmkClosureBase(p)
  }
  lazy val ClosureBaseRep: Rep[ClosureBaseCompanionCtor] = new ClosureBaseCompanionCtor
  lazy val RClosureBase: ClosureBaseCompanionCtor = proxyClosureBaseCompanion(ClosureBaseRep)
  implicit def proxyClosureBaseCompanion(p: Rep[ClosureBaseCompanionCtor]): ClosureBaseCompanionCtor = {
    proxyOps[ClosureBaseCompanionCtor](p)
  }

  implicit case object ClosureBaseCompanionElem extends CompanionElem[ClosureBaseCompanionCtor] {
    lazy val tag = weakTypeTag[ClosureBaseCompanionCtor]
    protected def getDefaultRep = ClosureBaseRep
  }

  implicit def proxyClosureBase[Env, Arg, Res](p: Rep[ClosureBase[Env, Arg, Res]]): ClosureBase[Env, Arg, Res] =
    proxyOps[ClosureBase[Env, Arg, Res]](p)

  implicit class ExtendedClosureBase[Env, Arg, Res](p: Rep[ClosureBase[Env, Arg, Res]]) {
    def toData: Rep[ClosureBaseData[Env, Arg, Res]] = {
      implicit val eEnv = p.env.elem;
implicit val eArg = p.func.elem.eDom.eSnd;
implicit val eRes = p.func.elem.eRange
      isoClosureBase(eEnv, eArg, eRes).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoClosureBase[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Iso[ClosureBaseData[Env, Arg, Res], ClosureBase[Env, Arg, Res]] =
    reifyObject(new ClosureBaseIso[Env, Arg, Res]()(eEnv, eArg, eRes))

  def mkClosureBase[Env, Arg, Res]
    (env: Rep[Env], func: Rep[((Env, Arg)) => Res]): Rep[ClosureBase[Env, Arg, Res]] = {
    new ClosureBaseCtor[Env, Arg, Res](env, func)
  }
  def unmkClosureBase[Env, Arg, Res](p: Rep[Closure[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ClosureBaseElem[Env, Arg, Res] @unchecked =>
      Some((p.asRep[ClosureBase[Env, Arg, Res]].env, p.asRep[ClosureBase[Env, Arg, Res]].func))
    case _ =>
      None
  }

    object ClosureBaseMethods {
    object apply {
      def unapply(d: Def[_]): Option[(Rep[ClosureBase[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, Seq(e, a, _*), _) if receiver.elem.isInstanceOf[ClosureBaseElem[_, _, _]] && method.getName == "apply" =>
          Some((receiver, e, a)).asInstanceOf[Option[(Rep[ClosureBase[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ClosureBase[Env, Arg, Res]], Rep[Env], Rep[Arg]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ClosureBaseCompanionMethods {
  }
} // of object ClosureBase
  registerEntityObject("ClosureBase", ClosureBase)

object CostedFunc extends EntityObject("CostedFunc") {
  case class CostedFuncCtor[Env, Arg, Res]
      (override val envCost: Rep[Costed[Env]], override val func: Rep[Closure[Env, Arg, Res]], override val costFunc: Rep[Closure[Env, Arg, Long]])
    extends CostedFunc[Env, Arg, Res](envCost, func, costFunc) with Def[CostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = envCost.eVal;
implicit lazy val eArg = func.eArg;
implicit lazy val eRes = func.eRes
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val selfType = element[CostedFunc[Env, Arg, Res]]
  }
  // elem for concrete class
  class CostedFuncElem[Env, Arg, Res](val iso: Iso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]])(implicit val eEnv: Elem[Env], val eArg: Elem[Arg], val eRes: Elem[Res])
    extends CostedElem[Arg => Res, CostedFunc[Env, Arg, Res]]
    with ConcreteElem[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[Arg => Res]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedFunc: missing fields List(envCost, func, costFunc)")
    override def getDefaultRep = RCostedFunc(element[Costed[Env]].defaultRepValue, element[Closure[Env, Arg, Res]].defaultRepValue, element[Closure[Env, Arg, Long]].defaultRepValue)
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CostedFunc[Env, Arg, Res]]
    }
  }

  // state representation type
  type CostedFuncData[Env, Arg, Res] = (Costed[Env], (Closure[Env, Arg, Res], Closure[Env, Arg, Long]))

  // 3) Iso for concrete class
  class CostedFuncIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] with Def[CostedFuncIso[Env, Arg, Res]] {
    private lazy val _safeFrom = fun { p: Rep[CostedFunc[Env, Arg, Res]] => (p.envCost, p.func, p.costFunc) }
    override def from(p: Rep[CostedFunc[Env, Arg, Res]]) =
      tryConvert[CostedFunc[Env, Arg, Res], (Costed[Env], (Closure[Env, Arg, Res], Closure[Env, Arg, Long]))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Costed[Env], (Closure[Env, Arg, Res], Closure[Env, Arg, Long]))]) = {
      val Pair(envCost, Pair(func, costFunc)) = p
      RCostedFunc(envCost, func, costFunc)
    }
    lazy val eFrom = pairElement(element[Costed[Env]], pairElement(element[Closure[Env, Arg, Res]], element[Closure[Env, Arg, Long]]))
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
implicit val eArg = p._2.eArg;
implicit val eRes = p._2.eRes
      isoCostedFunc[Env, Arg, Res].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](envCost: Rep[Costed[Env]], func: Rep[Closure[Env, Arg, Res]], costFunc: Rep[Closure[Env, Arg, Long]]): Rep[CostedFunc[Env, Arg, Res]] =
      mkCostedFunc(envCost, func, costFunc)

    def unapply[Env, Arg, Res](p: Rep[Costed[Arg => Res]]) = unmkCostedFunc(p)
  }
  lazy val CostedFuncRep: Rep[CostedFuncCompanionCtor] = new CostedFuncCompanionCtor
  lazy val RCostedFunc: CostedFuncCompanionCtor = proxyCostedFuncCompanion(CostedFuncRep)
  implicit def proxyCostedFuncCompanion(p: Rep[CostedFuncCompanionCtor]): CostedFuncCompanionCtor = {
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
      implicit val eEnv = p.envCost.eVal;
implicit val eArg = p.func.eArg;
implicit val eRes = p.func.eRes
      isoCostedFunc(eEnv, eArg, eRes).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedFunc[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Iso[CostedFuncData[Env, Arg, Res], CostedFunc[Env, Arg, Res]] =
    reifyObject(new CostedFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))

  def mkCostedFunc[Env, Arg, Res]
    (envCost: Rep[Costed[Env]], func: Rep[Closure[Env, Arg, Res]], costFunc: Rep[Closure[Env, Arg, Long]]): Rep[CostedFunc[Env, Arg, Res]] = {
    new CostedFuncCtor[Env, Arg, Res](envCost, func, costFunc)
  }
  def unmkCostedFunc[Env, Arg, Res](p: Rep[Costed[Arg => Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedFuncElem[Env, Arg, Res] @unchecked =>
      Some((p.asRep[CostedFunc[Env, Arg, Res]].envCost, p.asRep[CostedFunc[Env, Arg, Res]].func, p.asRep[CostedFunc[Env, Arg, Res]].costFunc))
    case _ =>
      None
  }

    object CostedFuncMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedFuncCompanionMethods {
  }
} // of object CostedFunc
  registerEntityObject("CostedFunc", CostedFunc)

object CostedOption extends EntityObject("CostedOption") {
  case class CostedOptionCtor[T]
      (override val either: Rep[WEither[Int, Costed[T]]])
    extends CostedOption[T](either) with Def[CostedOption[T]] {
    implicit lazy val eT = either.eB.typeArgs("Val")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    lazy val selfType = element[CostedOption[T]]
  }
  // elem for concrete class
  class CostedOptionElem[T](val iso: Iso[CostedOptionData[T], CostedOption[T]])(implicit val eT: Elem[T])
    extends CostedElem[WOption[T], CostedOption[T]]
    with ConcreteElem[CostedOptionData[T], CostedOption[T]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(wOptionElement(element[T])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[WOption[T]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedOption: missing fields List(either)")
    override def getDefaultRep = RCostedOption(element[WEither[Int, Costed[T]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CostedOption[T]]
    }
  }

  // state representation type
  type CostedOptionData[T] = WEither[Int, Costed[T]]

  // 3) Iso for concrete class
  class CostedOptionIso[T](implicit eT: Elem[T])
    extends EntityIso[CostedOptionData[T], CostedOption[T]] with Def[CostedOptionIso[T]] {
    private lazy val _safeFrom = fun { p: Rep[CostedOption[T]] => p.either }
    override def from(p: Rep[CostedOption[T]]) =
      tryConvert[CostedOption[T], WEither[Int, Costed[T]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WEither[Int, Costed[T]]]) = {
      val either = p
      RCostedOption(either)
    }
    lazy val eFrom = element[WEither[Int, Costed[T]]]
    lazy val eTo = new CostedOptionElem[T](self)
    lazy val selfType = new CostedOptionIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class CostedOptionIsoElem[T](eT: Elem[T]) extends Elem[CostedOptionIso[T]] {
    def getDefaultRep = reifyObject(new CostedOptionIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CostedOptionIso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CostedOptionCompanionCtor extends CompanionDef[CostedOptionCompanionCtor] with CostedOptionCompanion {
    def selfType = CostedOptionCompanionElem
    override def toString = "CostedOptionCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](either: Rep[WEither[Int, Costed[T]]]): Rep[CostedOption[T]] =
      mkCostedOption(either)

    def unapply[T](p: Rep[Costed[WOption[T]]]) = unmkCostedOption(p)
  }
  lazy val CostedOptionRep: Rep[CostedOptionCompanionCtor] = new CostedOptionCompanionCtor
  lazy val RCostedOption: CostedOptionCompanionCtor = proxyCostedOptionCompanion(CostedOptionRep)
  implicit def proxyCostedOptionCompanion(p: Rep[CostedOptionCompanionCtor]): CostedOptionCompanionCtor = {
    proxyOps[CostedOptionCompanionCtor](p)
  }

  implicit case object CostedOptionCompanionElem extends CompanionElem[CostedOptionCompanionCtor] {
    lazy val tag = weakTypeTag[CostedOptionCompanionCtor]
    protected def getDefaultRep = CostedOptionRep
  }

  implicit def proxyCostedOption[T](p: Rep[CostedOption[T]]): CostedOption[T] =
    proxyOps[CostedOption[T]](p)

  implicit class ExtendedCostedOption[T](p: Rep[CostedOption[T]]) {
    def toData: Rep[CostedOptionData[T]] = {
      implicit val eT = p.either.eB.typeArgs("Val")._1.asElem[T]
      isoCostedOption(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedOption[T](implicit eT: Elem[T]): Iso[CostedOptionData[T], CostedOption[T]] =
    reifyObject(new CostedOptionIso[T]()(eT))

  def mkCostedOption[T]
    (either: Rep[WEither[Int, Costed[T]]]): Rep[CostedOption[T]] = {
    new CostedOptionCtor[T](either)
  }
  def unmkCostedOption[T](p: Rep[Costed[WOption[T]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedOptionElem[T] @unchecked =>
      Some((p.asRep[CostedOption[T]].either))
    case _ =>
      None
  }

    object CostedOptionMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedOption[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedOption[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedOption[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedOptionCompanionMethods {
  }
} // of object CostedOption
  registerEntityObject("CostedOption", CostedOption)

object CostedArray extends EntityObject("CostedArray") {
  case class CostedArrayCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Int]])
    extends CostedArray[Item](values, costs) with Def[CostedArray[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[WArray[Item]] = implicitly[Elem[WArray[Item]]]
    lazy val selfType = element[CostedArray[Item]]
  }
  // elem for concrete class
  class CostedArrayElem[Item](val iso: Iso[CostedArrayData[Item], CostedArray[Item]])(implicit val eItem: Elem[Item])
    extends CostedElem[WArray[Item], CostedArray[Item]]
    with ConcreteElem[CostedArrayData[Item], CostedArray[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(wArrayElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[WArray[Item]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedArray: missing fields List(values, costs)")
    override def getDefaultRep = RCostedArray(element[Col[Item]].defaultRepValue, element[Col[Int]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedArray[Item]]
    }
  }

  // state representation type
  type CostedArrayData[Item] = (Col[Item], Col[Int])

  // 3) Iso for concrete class
  class CostedArrayIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedArrayData[Item], CostedArray[Item]] with Def[CostedArrayIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedArray[Item]] => (p.values, p.costs) }
    override def from(p: Rep[CostedArray[Item]]) =
      tryConvert[CostedArray[Item], (Col[Item], Col[Int])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], Col[Int])]) = {
      val Pair(values, costs) = p
      RCostedArray(values, costs)
    }
    lazy val eFrom = pairElement(element[Col[Item]], element[Col[Int]])
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
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Int]]): Rep[CostedArray[Item]] =
      mkCostedArray(values, costs)

    def unapply[Item](p: Rep[Costed[WArray[Item]]]) = unmkCostedArray(p)
  }
  lazy val CostedArrayRep: Rep[CostedArrayCompanionCtor] = new CostedArrayCompanionCtor
  lazy val RCostedArray: CostedArrayCompanionCtor = proxyCostedArrayCompanion(CostedArrayRep)
  implicit def proxyCostedArrayCompanion(p: Rep[CostedArrayCompanionCtor]): CostedArrayCompanionCtor = {
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
    (values: Rep[Col[Item]], costs: Rep[Col[Int]]): Rep[CostedArray[Item]] = {
    new CostedArrayCtor[Item](values, costs)
  }
  def unmkCostedArray[Item](p: Rep[Costed[WArray[Item]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedArrayElem[Item] @unchecked =>
      Some((p.asRep[CostedArray[Item]].values, p.asRep[CostedArray[Item]].costs))
    case _ =>
      None
  }

    object CostedArrayMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedArrayElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedArrayCompanionMethods {
  }
} // of object CostedArray
  registerEntityObject("CostedArray", CostedArray)

object CostedCol extends EntityObject("CostedCol") {
  case class CostedColCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Int]])
    extends CostedCol[Item](values, costs) with Def[CostedCol[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[Col[Item]] = implicitly[Elem[Col[Item]]]
    lazy val selfType = element[CostedCol[Item]]
  }
  // elem for concrete class
  class CostedColElem[Item](val iso: Iso[CostedColData[Item], CostedCol[Item]])(implicit val eItem: Elem[Item])
    extends CostedElem[Col[Item], CostedCol[Item]]
    with ConcreteElem[CostedColData[Item], CostedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[Col[Item]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedCol: missing fields List(values, costs)")
    override def getDefaultRep = RCostedCol(element[Col[Item]].defaultRepValue, element[Col[Int]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedCol[Item]]
    }
  }

  // state representation type
  type CostedColData[Item] = (Col[Item], Col[Int])

  // 3) Iso for concrete class
  class CostedColIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedColData[Item], CostedCol[Item]] with Def[CostedColIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedCol[Item]] => (p.values, p.costs) }
    override def from(p: Rep[CostedCol[Item]]) =
      tryConvert[CostedCol[Item], (Col[Item], Col[Int])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], Col[Int])]) = {
      val Pair(values, costs) = p
      RCostedCol(values, costs)
    }
    lazy val eFrom = pairElement(element[Col[Item]], element[Col[Int]])
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
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Int]]): Rep[CostedCol[Item]] =
      mkCostedCol(values, costs)

    def unapply[Item](p: Rep[Costed[Col[Item]]]) = unmkCostedCol(p)
  }
  lazy val CostedColRep: Rep[CostedColCompanionCtor] = new CostedColCompanionCtor
  lazy val RCostedCol: CostedColCompanionCtor = proxyCostedColCompanion(CostedColRep)
  implicit def proxyCostedColCompanion(p: Rep[CostedColCompanionCtor]): CostedColCompanionCtor = {
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
    (values: Rep[Col[Item]], costs: Rep[Col[Int]]): Rep[CostedCol[Item]] = {
    new CostedColCtor[Item](values, costs)
  }
  def unmkCostedCol[Item](p: Rep[Costed[Col[Item]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedColElem[Item] @unchecked =>
      Some((p.asRep[CostedCol[Item]].values, p.asRep[CostedCol[Item]].costs))
    case _ =>
      None
  }

    object CostedColMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
    extends CostedElem[WArray[(L, R)], CostedPairArray[L, R]]
    with ConcreteElem[CostedPairArrayData[L, R], CostedPairArray[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(wArrayElement(pairElement(element[L],element[R]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[WArray[(L, R)]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedPairArray: missing fields List(ls, rs)")
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

    def unapply[L, R](p: Rep[Costed[WArray[(L, R)]]]) = unmkCostedPairArray(p)
  }
  lazy val CostedPairArrayRep: Rep[CostedPairArrayCompanionCtor] = new CostedPairArrayCompanionCtor
  lazy val RCostedPairArray: CostedPairArrayCompanionCtor = proxyCostedPairArrayCompanion(CostedPairArrayRep)
  implicit def proxyCostedPairArrayCompanion(p: Rep[CostedPairArrayCompanionCtor]): CostedPairArrayCompanionCtor = {
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
  def unmkCostedPairArray[L, R](p: Rep[Costed[WArray[(L, R)]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairArrayElem[L, R] @unchecked =>
      Some((p.asRep[CostedPairArray[L, R]].ls, p.asRep[CostedPairArray[L, R]].rs))
    case _ =>
      None
  }

    object CostedPairArrayMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairArrayElem[_, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairArray[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
    extends CostedElem[Col[(L, R)], CostedPairCol[L, R]]
    with ConcreteElem[CostedPairColData[L, R], CostedPairCol[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(pairElement(element[L],element[R]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[Col[(L, R)]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedPairCol: missing fields List(ls, rs)")
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

    def unapply[L, R](p: Rep[Costed[Col[(L, R)]]]) = unmkCostedPairCol(p)
  }
  lazy val CostedPairColRep: Rep[CostedPairColCompanionCtor] = new CostedPairColCompanionCtor
  lazy val RCostedPairCol: CostedPairColCompanionCtor = proxyCostedPairColCompanion(CostedPairColRep)
  implicit def proxyCostedPairColCompanion(p: Rep[CostedPairColCompanionCtor]): CostedPairColCompanionCtor = {
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
  def unmkCostedPairCol[L, R](p: Rep[Costed[Col[(L, R)]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairColElem[L, R] @unchecked =>
      Some((p.asRep[CostedPairCol[L, R]].ls, p.asRep[CostedPairCol[L, R]].rs))
    case _ =>
      None
  }

    object CostedPairColMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
  }
  // elem for concrete class
  class CostedNestedArrayElem[Item](val iso: Iso[CostedNestedArrayData[Item], CostedNestedArray[Item]])(implicit val eItem: Elem[Item])
    extends CostedElem[WArray[WArray[Item]], CostedNestedArray[Item]]
    with ConcreteElem[CostedNestedArrayData[Item], CostedNestedArray[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(wArrayElement(wArrayElement(element[Item]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[WArray[WArray[Item]]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedNestedArray: missing fields List(rows)")
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

    def unapply[Item](p: Rep[Costed[WArray[WArray[Item]]]]) = unmkCostedNestedArray(p)
  }
  lazy val CostedNestedArrayRep: Rep[CostedNestedArrayCompanionCtor] = new CostedNestedArrayCompanionCtor
  lazy val RCostedNestedArray: CostedNestedArrayCompanionCtor = proxyCostedNestedArrayCompanion(CostedNestedArrayRep)
  implicit def proxyCostedNestedArrayCompanion(p: Rep[CostedNestedArrayCompanionCtor]): CostedNestedArrayCompanionCtor = {
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
  def unmkCostedNestedArray[Item](p: Rep[Costed[WArray[WArray[Item]]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedNestedArrayElem[Item] @unchecked =>
      Some((p.asRep[CostedNestedArray[Item]].rows))
    case _ =>
      None
  }

    object CostedNestedArrayMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedArrayElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedArray[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedArray[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
  }
  // elem for concrete class
  class CostedNestedColElem[Item](val iso: Iso[CostedNestedColData[Item], CostedNestedCol[Item]])(implicit val eItem: Elem[Item])
    extends CostedElem[Col[Col[Item]], CostedNestedCol[Item]]
    with ConcreteElem[CostedNestedColData[Item], CostedNestedCol[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(colElement(element[Item]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override def convertCosted(x: Rep[Costed[Col[Col[Item]]]]) = // Converter is not generated by meta
!!!("Cannot convert from Costed to CostedNestedCol: missing fields List(rows)")
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

    def unapply[Item](p: Rep[Costed[Col[Col[Item]]]]) = unmkCostedNestedCol(p)
  }
  lazy val CostedNestedColRep: Rep[CostedNestedColCompanionCtor] = new CostedNestedColCompanionCtor
  lazy val RCostedNestedCol: CostedNestedColCompanionCtor = proxyCostedNestedColCompanion(CostedNestedColRep)
  implicit def proxyCostedNestedColCompanion(p: Rep[CostedNestedColCompanionCtor]): CostedNestedColCompanionCtor = {
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
  def unmkCostedNestedCol[Item](p: Rep[Costed[Col[Col[Item]]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedNestedColElem[Item] @unchecked =>
      Some((p.asRep[CostedNestedCol[Item]].rows))
    case _ =>
      None
  }

    object CostedNestedColMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedNestedCol[Item]] forSome {type Item}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedNestedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
      def unapply(d: Def[_]): Option[Rep[ConcreteCostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConcreteCostedBuilderElem] && method.getName == "monoidBuilder" =>
          Some(receiver).asInstanceOf[Option[Rep[ConcreteCostedBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ConcreteCostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
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
