package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CostsDefs extends scalan.Scalan with Costs {
  self: Library =>
import IsoUR._
import Converter._
import Coll._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedFunc._
import CostedOption._
import CostedPair._
import CostedPrim._
import MonoidBuilder._
import Size._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import WOption._
import WRType._

object Costed extends EntityObject("Costed") {
  // entityAdapter for Costed trait
  case class CostedAdapter[Val](source: Rep[Costed[Val]])
      extends Costed[Val] with Def[Costed[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[Costed[Val]] = element[Costed[Val]]
    override def transform(t: Transformer) = CostedAdapter[Val](t(source))
    private val thisClass = classOf[Costed[Val]]

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Val] = {
      asRep[Val](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Val]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def size: Rep[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[Val]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCosted[Val](p: Rep[Costed[Val]]): Costed[Val] = {
    if (p.rhs.isInstanceOf[Costed[Val]@unchecked]) p.rhs.asInstanceOf[Costed[Val]]
    else
      CostedAdapter(p)
  }

  // familyElem
  class CostedElem[Val, To <: Costed[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[Costed[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Costed[Val]] => convertCosted(x) }
      tryConvert(element[Costed[Val]], this, x, conv)
    }

    def convertCosted(x: Rep[Costed[Val]]): Rep[To] = {
      x.elem match {
        case _: CostedElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedElem[_, _], but got $e", x)
      }
    }
  }

  implicit def costedElement[Val](implicit eVal: Elem[Val]): Elem[Costed[Val]] =
    cachedElemByClass(eVal)(classOf[CostedElem[Val, Costed[Val]]])

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor] {
    lazy val tag = weakTypeTag[CostedCompanionCtor]
  }

  abstract class CostedCompanionCtor extends CompanionDef[CostedCompanionCtor] with CostedCompanion {
    def selfType = CostedCompanionElem
    override def toString = "Costed"
  }
  implicit def proxyCostedCompanionCtor(p: Rep[CostedCompanionCtor]): CostedCompanionCtor =
    proxyOps[CostedCompanionCtor](p)

  lazy val RCosted: Rep[CostedCompanionCtor] = new CostedCompanionCtor {
    private val thisClass = classOf[CostedCompanion]
  }

  object CostedMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object size {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "size" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedCompanionMethods {
  }
} // of object Costed
  registerEntityObject("Costed", Costed)

object CostedPrim extends EntityObject("CostedPrim") {
  // entityAdapter for CostedPrim trait
  case class CostedPrimAdapter[Val](source: Rep[CostedPrim[Val]])
      extends CostedPrim[Val] with Def[CostedPrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[CostedPrim[Val]] = element[CostedPrim[Val]]
    override def transform(t: Transformer) = CostedPrimAdapter[Val](t(source))
    private val thisClass = classOf[CostedPrim[Val]]

    def value: Rep[Val] = {
      asRep[Val](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Val]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def size: Rep[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[Val]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedPrim[Val](p: Rep[CostedPrim[Val]]): CostedPrim[Val] = {
    if (p.rhs.isInstanceOf[CostedPrim[Val]@unchecked]) p.rhs.asInstanceOf[CostedPrim[Val]]
    else
      CostedPrimAdapter(p)
  }

  // familyElem
  class CostedPrimElem[Val, To <: CostedPrim[Val]](implicit _eVal: Elem[Val])
    extends CostedElem[Val, To] {
    override def eVal = _eVal

    override lazy val parent: Option[Elem[_]] = Some(costedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedPrim[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedPrim[Val]] => convertCostedPrim(x) }
      tryConvert(element[CostedPrim[Val]], this, x, conv)
    }

    def convertCostedPrim(x: Rep[CostedPrim[Val]]): Rep[To] = {
      x.elem match {
        case _: CostedPrimElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedPrimElem[_, _], but got $e", x)
      }
    }
  }

  implicit def costedPrimElement[Val](implicit eVal: Elem[Val]): Elem[CostedPrim[Val]] =
    cachedElemByClass(eVal)(classOf[CostedPrimElem[Val, CostedPrim[Val]]])

  implicit case object CostedPrimCompanionElem extends CompanionElem[CostedPrimCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPrimCompanionCtor]
  }

  abstract class CostedPrimCompanionCtor extends CompanionDef[CostedPrimCompanionCtor] with CostedPrimCompanion {
    def selfType = CostedPrimCompanionElem
    override def toString = "CostedPrim"
  }
  implicit def proxyCostedPrimCompanionCtor(p: Rep[CostedPrimCompanionCtor]): CostedPrimCompanionCtor =
    proxyOps[CostedPrimCompanionCtor](p)

  lazy val RCostedPrim: Rep[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor {
    private val thisClass = classOf[CostedPrimCompanion]
  }

  object CostedPrimMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPrimElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPrimElem[_, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object size {
      def unapply(d: Def[_]): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPrimElem[_, _]] && method.getName == "size" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedPrimCompanionMethods {
  }
} // of object CostedPrim
  registerEntityObject("CostedPrim", CostedPrim)

object CostedPair extends EntityObject("CostedPair") {
  // entityAdapter for CostedPair trait
  case class CostedPairAdapter[L, R](source: Rep[CostedPair[L, R]])
      extends CostedPair[L, R] with Def[CostedPair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[CostedPair[L, R]] = element[CostedPair[L, R]]
    override def transform(t: Transformer) = CostedPairAdapter[L, R](t(source))
    private val thisClass = classOf[CostedPair[L, R]]

    def l: Rep[Costed[L]] = {
      asRep[Costed[L]](mkMethodCall(source,
        thisClass.getMethod("l"),
        List(),
        true, true, element[Costed[L]]))
    }

    def r: Rep[Costed[R]] = {
      asRep[Costed[R]](mkMethodCall(source,
        thisClass.getMethod("r"),
        List(),
        true, true, element[Costed[R]]))
    }

    def accCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("accCost"),
        List(),
        true, true, element[Int]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[(L, R)]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def size: Rep[Size[(L, R)]] = {
      asRep[Size[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[(L, R)]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedPair[L, R](p: Rep[CostedPair[L, R]]): CostedPair[L, R] = {
    if (p.rhs.isInstanceOf[CostedPair[L, R]@unchecked]) p.rhs.asInstanceOf[CostedPair[L, R]]
    else
      CostedPairAdapter(p)
  }

  // familyElem
  class CostedPairElem[L, R, To <: CostedPair[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CostedElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(costedElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPair[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedPair[L, R]] => convertCostedPair(x) }
      tryConvert(element[CostedPair[L, R]], this, x, conv)
    }

    def convertCostedPair(x: Rep[CostedPair[L, R]]): Rep[To] = {
      x.elem match {
        case _: CostedPairElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedPairElem[_, _, _], but got $e", x)
      }
    }
  }

  implicit def costedPairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedPair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[CostedPairElem[L, R, CostedPair[L, R]]])

  implicit case object CostedPairCompanionElem extends CompanionElem[CostedPairCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairCompanionCtor]
  }

  abstract class CostedPairCompanionCtor extends CompanionDef[CostedPairCompanionCtor] with CostedPairCompanion {
    def selfType = CostedPairCompanionElem
    override def toString = "CostedPair"
  }
  implicit def proxyCostedPairCompanionCtor(p: Rep[CostedPairCompanionCtor]): CostedPairCompanionCtor =
    proxyOps[CostedPairCompanionCtor](p)

  lazy val RCostedPair: Rep[CostedPairCompanionCtor] = new CostedPairCompanionCtor {
    private val thisClass = classOf[CostedPairCompanion]
  }

  object CostedPairMethods {
    object l {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _, _]] && method.getName == "l" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object r {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _, _]] && method.getName == "r" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object accCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedPair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairElem[_, _, _]] && method.getName == "accCost" =>
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

object CostedFunc extends EntityObject("CostedFunc") {
  // entityAdapter for CostedFunc trait
  case class CostedFuncAdapter[Env, Arg, Res](source: Rep[CostedFunc[Env, Arg, Res]])
      extends CostedFunc[Env, Arg, Res] with Def[CostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asElem[Env];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asElem[Arg];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val selfType: Elem[CostedFunc[Env, Arg, Res]] = element[CostedFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CostedFuncAdapter[Env, Arg, Res](t(source))
    private val thisClass = classOf[CostedFunc[Env, Arg, Res]]

    def envCosted: Rep[Costed[Env]] = {
      asRep[Costed[Env]](mkMethodCall(source,
        thisClass.getMethod("envCosted"),
        List(),
        true, true, element[Costed[Env]]))
    }

    def func: Rep[Costed[Arg] => Costed[Res]] = {
      asRep[Costed[Arg] => Costed[Res]](mkMethodCall(source,
        thisClass.getMethod("func"),
        List(),
        true, true, element[Costed[Arg] => Costed[Res]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def sliceCalc: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        thisClass.getMethod("sliceCalc"),
        List(),
        true, true, element[Arg => Res]))
    }

    def sliceCost: Rep[((Int, Size[Arg])) => Int] = {
      asRep[((Int, Size[Arg])) => Int](mkMethodCall(source,
        thisClass.getMethod("sliceCost"),
        List(),
        true, true, element[((Int, Size[Arg])) => Int]))
    }

    def sliceCostEx: Rep[((Arg, (Int, Size[Arg]))) => Int] = {
      asRep[((Arg, (Int, Size[Arg]))) => Int](mkMethodCall(source,
        thisClass.getMethod("sliceCostEx"),
        List(),
        true, true, element[((Arg, (Int, Size[Arg]))) => Int]))
    }

    def sliceSize: Rep[Size[Arg] => Size[Res]] = {
      asRep[Size[Arg] => Size[Res]](mkMethodCall(source,
        thisClass.getMethod("sliceSize"),
        List(),
        true, true, element[Size[Arg] => Size[Res]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Arg => Res]))
    }

    def size: Rep[Size[Arg => Res]] = {
      asRep[Size[Arg => Res]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[Arg => Res]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedFunc[Env, Arg, Res](p: Rep[CostedFunc[Env, Arg, Res]]): CostedFunc[Env, Arg, Res] = {
    if (p.rhs.isInstanceOf[CostedFunc[Env, Arg, Res]@unchecked]) p.rhs.asInstanceOf[CostedFunc[Env, Arg, Res]]
    else
      CostedFuncAdapter(p)
  }

  // familyElem
  class CostedFuncElem[Env, Arg, Res, To <: CostedFunc[Env, Arg, Res]](implicit _eEnv: Elem[Env], _eArg: Elem[Arg], _eRes: Elem[Res])
    extends CostedElem[Arg => Res, To] {
    def eEnv = _eEnv
    def eArg = _eArg
    def eRes = _eRes

    override lazy val parent: Option[Elem[_]] = Some(costedElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[CostedFunc[Env, Arg, Res]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedFunc[Env, Arg, Res]] => convertCostedFunc(x) }
      tryConvert(element[CostedFunc[Env, Arg, Res]], this, x, conv)
    }

    def convertCostedFunc(x: Rep[CostedFunc[Env, Arg, Res]]): Rep[To] = {
      x.elem match {
        case _: CostedFuncElem[_, _, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedFuncElem[_, _, _, _], but got $e", x)
      }
    }
  }

  implicit def costedFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CostedFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[CostedFuncElem[Env, Arg, Res, CostedFunc[Env, Arg, Res]]])

  implicit case object CostedFuncCompanionElem extends CompanionElem[CostedFuncCompanionCtor] {
    lazy val tag = weakTypeTag[CostedFuncCompanionCtor]
  }

  abstract class CostedFuncCompanionCtor extends CompanionDef[CostedFuncCompanionCtor] with CostedFuncCompanion {
    def selfType = CostedFuncCompanionElem
    override def toString = "CostedFunc"
  }
  implicit def proxyCostedFuncCompanionCtor(p: Rep[CostedFuncCompanionCtor]): CostedFuncCompanionCtor =
    proxyOps[CostedFuncCompanionCtor](p)

  lazy val RCostedFunc: Rep[CostedFuncCompanionCtor] = new CostedFuncCompanionCtor {
    private val thisClass = classOf[CostedFuncCompanion]
  }

  object CostedFuncMethods {
    object envCosted {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "envCosted" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object func {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "func" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceCalc {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "sliceCalc" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "sliceCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceCostEx {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "sliceCostEx" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sliceSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "sliceSize" =>
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

object CostedColl extends EntityObject("CostedColl") {
  // entityAdapter for CostedColl trait
  case class CostedCollAdapter[Item](source: Rep[CostedColl[Item]])
      extends CostedColl[Item] with Def[CostedColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val selfType: Elem[CostedColl[Item]] = element[CostedColl[Item]]
    override def transform(t: Transformer) = CostedCollAdapter[Item](t(source))
    private val thisClass = classOf[CostedColl[Item]]

    def values: Rep[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        thisClass.getMethod("values"),
        List(),
        true, true, element[Coll[Item]]))
    }

    def costs: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        thisClass.getMethod("costs"),
        List(),
        true, true, element[Coll[Int]]))
    }

    def sizes: Rep[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        thisClass.getMethod("sizes"),
        List(),
        true, true, element[Coll[Size[Item]]]))
    }

    def valuesCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("valuesCost"),
        List(),
        true, true, element[Int]))
    }

    def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedColl[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedColl[Res]](mkMethodCall(source,
        thisClass.getMethod("mapCosted", classOf[Sym]),
        List(f),
        true, true, element[CostedColl[Res]]))
    }

    def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedColl[Item]] = {
      asRep[CostedColl[Item]](mkMethodCall(source,
        thisClass.getMethod("filterCosted", classOf[Sym]),
        List(f),
        true, true, element[CostedColl[Item]]))
    }

    def foldCosted[B](zero: Rep[Costed[B]], op: Rep[Costed[(B, Item)] => Costed[B]]): Rep[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(source,
        thisClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[Costed[B]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Coll[Item]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def size: Rep[Size[Coll[Item]]] = {
      asRep[Size[Coll[Item]]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[Coll[Item]]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedColl[Item](p: Rep[CostedColl[Item]]): CostedColl[Item] = {
    if (p.rhs.isInstanceOf[CostedColl[Item]@unchecked]) p.rhs.asInstanceOf[CostedColl[Item]]
    else
      CostedCollAdapter(p)
  }

  // familyElem
  class CostedCollElem[Item, To <: CostedColl[Item]](implicit _eItem: Elem[Item])
    extends CostedElem[Coll[Item], To] {
    def eItem = _eItem

    override lazy val parent: Option[Elem[_]] = Some(costedElement(collElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedColl[Item]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedColl[Item]] => convertCostedColl(x) }
      tryConvert(element[CostedColl[Item]], this, x, conv)
    }

    def convertCostedColl(x: Rep[CostedColl[Item]]): Rep[To] = {
      x.elem match {
        case _: CostedCollElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedCollElem[_, _], but got $e", x)
      }
    }
  }

  implicit def costedCollElement[Item](implicit eItem: Elem[Item]): Elem[CostedColl[Item]] =
    cachedElemByClass(eItem)(classOf[CostedCollElem[Item, CostedColl[Item]]])

  implicit case object CostedCollCompanionElem extends CompanionElem[CostedCollCompanionCtor] {
    lazy val tag = weakTypeTag[CostedCollCompanionCtor]
  }

  abstract class CostedCollCompanionCtor extends CompanionDef[CostedCollCompanionCtor] with CostedCollCompanion {
    def selfType = CostedCollCompanionElem
    override def toString = "CostedColl"
  }
  implicit def proxyCostedCollCompanionCtor(p: Rep[CostedCollCompanionCtor]): CostedCollCompanionCtor =
    proxyOps[CostedCollCompanionCtor](p)

  lazy val RCostedColl: Rep[CostedCollCompanionCtor] = new CostedCollCompanionCtor {
    private val thisClass = classOf[CostedCollCompanion]
  }

  object CostedCollMethods {
    object values {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "values" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costs {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "costs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sizes {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "sizes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valuesCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "valuesCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "mapCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "filterCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedCollElem[_, _]] && method.getName == "foldCosted" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedCollCompanionMethods {
  }
} // of object CostedColl
  registerEntityObject("CostedColl", CostedColl)

object CostedOption extends EntityObject("CostedOption") {
  // entityAdapter for CostedOption trait
  case class CostedOptionAdapter[T](source: Rep[CostedOption[T]])
      extends CostedOption[T] with Def[CostedOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val selfType: Elem[CostedOption[T]] = element[CostedOption[T]]
    override def transform(t: Transformer) = CostedOptionAdapter[T](t(source))
    private val thisClass = classOf[CostedOption[T]]

    def costOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        thisClass.getMethod("costOpt"),
        List(),
        true, true, element[WOption[Int]]))
    }

    def sizeOpt: Rep[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        thisClass.getMethod("sizeOpt"),
        List(),
        true, true, element[WOption[Size[T]]]))
    }

    def accumulatedCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("accumulatedCost"),
        List(),
        true, true, element[Int]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[WOption[T]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def size: Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Size[WOption[T]]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedOption[T](p: Rep[CostedOption[T]]): CostedOption[T] = {
    if (p.rhs.isInstanceOf[CostedOption[T]@unchecked]) p.rhs.asInstanceOf[CostedOption[T]]
    else
      CostedOptionAdapter(p)
  }

  // familyElem
  class CostedOptionElem[T, To <: CostedOption[T]](implicit _eT: Elem[T])
    extends CostedElem[WOption[T], To] {
    def eT = _eT

    override lazy val parent: Option[Elem[_]] = Some(costedElement(wOptionElement(element[T])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[CostedOption[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedOption[T]] => convertCostedOption(x) }
      tryConvert(element[CostedOption[T]], this, x, conv)
    }

    def convertCostedOption(x: Rep[CostedOption[T]]): Rep[To] = {
      x.elem match {
        case _: CostedOptionElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedOptionElem[_, _], but got $e", x)
      }
    }
  }

  implicit def costedOptionElement[T](implicit eT: Elem[T]): Elem[CostedOption[T]] =
    cachedElemByClass(eT)(classOf[CostedOptionElem[T, CostedOption[T]]])

  implicit case object CostedOptionCompanionElem extends CompanionElem[CostedOptionCompanionCtor] {
    lazy val tag = weakTypeTag[CostedOptionCompanionCtor]
  }

  abstract class CostedOptionCompanionCtor extends CompanionDef[CostedOptionCompanionCtor] with CostedOptionCompanion {
    def selfType = CostedOptionCompanionElem
    override def toString = "CostedOption"
  }
  implicit def proxyCostedOptionCompanionCtor(p: Rep[CostedOptionCompanionCtor]): CostedOptionCompanionCtor =
    proxyOps[CostedOptionCompanionCtor](p)

  lazy val RCostedOption: Rep[CostedOptionCompanionCtor] = new CostedOptionCompanionCtor {
    private val thisClass = classOf[CostedOptionCompanion]
  }

  object CostedOptionMethods {
    object costOpt {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "costOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sizeOpt {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "sizeOpt" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object accumulatedCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "accumulatedCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedOptionCompanionMethods {
  }
} // of object CostedOption
  registerEntityObject("CostedOption", CostedOption)

object CostedBuilder extends EntityObject("CostedBuilder") {
  // entityAdapter for CostedBuilder trait
  case class CostedBuilderAdapter(source: Rep[CostedBuilder])
      extends CostedBuilder with Def[CostedBuilder] {
    val selfType: Elem[CostedBuilder] = element[CostedBuilder]
    override def transform(t: Transformer) = CostedBuilderAdapter(t(source))
    private val thisClass = classOf[CostedBuilder]

    def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(source,
        thisClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        List(x, optCost),
        true, true, element[Costed[T]]))
    }

    def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(source,
        thisClass.getMethod("defaultValue", classOf[Sym]),
        List(valueType),
        true, true, element[T]))
    }

    def monoidBuilder: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        thisClass.getMethod("monoidBuilder"),
        List(),
        true, true, element[MonoidBuilder]))
    }

    def mkSizePrim[T](dataSize: Rep[Long], tT: Rep[WRType[T]]): Rep[SizePrim[T]] = {
      implicit val eT = tT.eA
      asRep[SizePrim[T]](mkMethodCall(source,
        thisClass.getMethod("mkSizePrim", classOf[Sym], classOf[Sym]),
        List(dataSize, tT),
        true, true, element[SizePrim[T]]))
    }

    def mkSizePair[L, R](l: Rep[Size[L]], r: Rep[Size[R]]): Rep[SizePair[L, R]] = {
      implicit val eL = l.eVal
implicit val eR = r.eVal
      asRep[SizePair[L, R]](mkMethodCall(source,
        thisClass.getMethod("mkSizePair", classOf[Sym], classOf[Sym]),
        List(l, r),
        true, true, element[SizePair[L, R]]))
    }

    def mkSizeColl[T](sizes: Rep[Coll[Size[T]]]): Rep[SizeColl[T]] = {
      implicit val eT = sizes.eA.typeArgs("Val")._1.asElem[T]
      asRep[SizeColl[T]](mkMethodCall(source,
        thisClass.getMethod("mkSizeColl", classOf[Sym]),
        List(sizes),
        true, true, element[SizeColl[T]]))
    }

    def mkSizeFunc[E, A, R](sizeEnv: Rep[Size[E]], sizeFunc: Rep[Long], tA: Rep[WRType[A]], tR: Rep[WRType[R]]): Rep[SizeFunc[E, A, R]] = {
      implicit val eE = sizeEnv.eVal
implicit val eA = tA.eA
implicit val eR = tR.eA
      asRep[SizeFunc[E, A, R]](mkMethodCall(source,
        thisClass.getMethod("mkSizeFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(sizeEnv, sizeFunc, tA, tR),
        true, true, element[SizeFunc[E, A, R]]))
    }

    def mkSizeOption[T](sizeOpt: Rep[WOption[Size[T]]]): Rep[SizeOption[T]] = {
      implicit val eT = sizeOpt.eA.typeArgs("Val")._1.asElem[T]
      asRep[SizeOption[T]](mkMethodCall(source,
        thisClass.getMethod("mkSizeOption", classOf[Sym]),
        List(sizeOpt),
        true, true, element[SizeOption[T]]))
    }

    def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Size[T]]): Rep[CostedPrim[T]] = {
      implicit val eT = value.elem
      asRep[CostedPrim[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedPrim", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(value, cost, size),
        true, true, element[CostedPrim[T]]))
    }

    def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]], accCost: Rep[Int]): Rep[CostedPair[L, R]] = {
      implicit val eL = first.eVal
implicit val eR = second.eVal
      asRep[CostedPair[L, R]](mkMethodCall(source,
        thisClass.getMethod("mkCostedPair", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(first, second, accCost),
        true, true, element[CostedPair[L, R]]))
    }

    def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], size: Rep[Size[Arg => Res]]): Rep[CostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = envCosted.eVal
implicit val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg]
implicit val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedFunc[Env, Arg, Res]](mkMethodCall(source,
        thisClass.getMethod("mkCostedFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(envCosted, func, cost, size),
        true, true, element[CostedFunc[Env, Arg, Res]]))
    }

    def mkCostedColl[T](values: Rep[Coll[T]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[T]]], valuesCost: Rep[Int]): Rep[CostedColl[T]] = {
      implicit val eT = values.eA
      asRep[CostedColl[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedColl", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(values, costs, sizes, valuesCost),
        true, true, element[CostedColl[T]]))
    }

    def mkCostedOption[T](value: Rep[WOption[T]], costOpt: Rep[WOption[Int]], sizeOpt: Rep[WOption[Size[T]]], accumulatedCost: Rep[Int]): Rep[CostedOption[T]] = {
      implicit val eT = value.eA
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedOption", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(value, costOpt, sizeOpt, accumulatedCost),
        true, true, element[CostedOption[T]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedBuilder(p: Rep[CostedBuilder]): CostedBuilder = {
    if (p.rhs.isInstanceOf[CostedBuilder@unchecked]) p.rhs.asInstanceOf[CostedBuilder]
    else
      CostedBuilderAdapter(p)
  }

  // familyElem
  class CostedBuilderElem[To <: CostedBuilder]
    extends EntityElem[To] {
    override lazy val tag = {
      weakTypeTag[CostedBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedBuilder] => convertCostedBuilder(x) }
      tryConvert(element[CostedBuilder], this, x, conv)
    }

    def convertCostedBuilder(x: Rep[CostedBuilder]): Rep[To] = {
      x.elem match {
        case _: CostedBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedBuilderElem[_], but got $e", x)
      }
    }
  }

  implicit lazy val costedBuilderElement: Elem[CostedBuilder] =
    new CostedBuilderElem[CostedBuilder]

  implicit case object CostedBuilderCompanionElem extends CompanionElem[CostedBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CostedBuilderCompanionCtor]
  }

  abstract class CostedBuilderCompanionCtor extends CompanionDef[CostedBuilderCompanionCtor] with CostedBuilderCompanion {
    def selfType = CostedBuilderCompanionElem
    override def toString = "CostedBuilder"
  }
  implicit def proxyCostedBuilderCompanionCtor(p: Rep[CostedBuilderCompanionCtor]): CostedBuilderCompanionCtor =
    proxyOps[CostedBuilderCompanionCtor](p)

  lazy val RCostedBuilder: Rep[CostedBuilderCompanionCtor] = new CostedBuilderCompanionCtor {
    private val thisClass = classOf[CostedBuilderCompanion]
  }

  object CostedBuilderMethods {
    object ConstructTupleCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "ConstructTupleCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object ConstructSumCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "ConstructSumCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SelectFieldCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "SelectFieldCost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SumTagSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "SumTagSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costedValue {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "costedValue" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object defaultValue {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "defaultValue" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object monoidBuilder {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "monoidBuilder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizePrim {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkSizePrim" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizePair {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkSizePair" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeColl {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkSizeColl" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkSizeFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeOption {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkSizeOption" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedPrim" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedPair" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedColl {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedColl" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedOption" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedBuilderCompanionMethods {
  }
} // of object CostedBuilder
  registerEntityObject("CostedBuilder", CostedBuilder)

  registerModule(CostsModule)
}

object CostsModule extends scalan.ModuleInfo("special.collection", "Costs")
}

trait CostsModule extends special.collection.impl.CostsDefs {self: Library =>}
