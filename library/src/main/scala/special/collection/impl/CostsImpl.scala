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
import Col._
import Costed._
import CostedBuilder._
import CostedCol._
import CostedFunc._
import CostedNestedCol._
import CostedOption._
import CostedPair._
import CostedPairCol._
import CostedPrim._
import CostedSum._
import MonoidBuilder._
import WArray._
import WEither._
import WOption._
import WRType._  // manual fix

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

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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

    lazy val parent: Option[Elem[_]] = None
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedElement[Val](implicit eVal: Elem[Val]): Elem[Costed[Val]] =
    cachedElem[CostedElem[Val, Costed[Val]]](eVal)

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor] {
    lazy val tag = weakTypeTag[CostedCompanionCtor]
    protected def getDefaultRep = RCosted
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

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "dataSize" =>
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

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedPrimElement[Val](implicit eVal: Elem[Val]): Elem[CostedPrim[Val]] =
    cachedElem[CostedPrimElem[Val, CostedPrim[Val]]](eVal)

  implicit case object CostedPrimCompanionElem extends CompanionElem[CostedPrimCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPrimCompanionCtor]
    protected def getDefaultRep = RCostedPrim
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

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedPrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPrimElem[_, _]] && method.getName == "dataSize" =>
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

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedPairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedPair[L, R]] =
    cachedElem[CostedPairElem[L, R, CostedPair[L, R]]](eL, eR)

  implicit case object CostedPairCompanionElem extends CompanionElem[CostedPairCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairCompanionCtor]
    protected def getDefaultRep = RCostedPair
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
  }

  object CostedPairCompanionMethods {
  }
} // of object CostedPair
  registerEntityObject("CostedPair", CostedPair)

object CostedSum extends EntityObject("CostedSum") {
  // entityAdapter for CostedSum trait
  case class CostedSumAdapter[L, R](source: Rep[CostedSum[L, R]])
      extends CostedSum[L, R] with Def[CostedSum[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[WEither[L, R]] = implicitly[Elem[WEither[L, R]]]
    val selfType: Elem[CostedSum[L, R]] = element[CostedSum[L, R]]
    override def transform(t: Transformer) = CostedSumAdapter[L, R](t(source))
    private val thisClass = classOf[CostedSum[L, R]]

    def value: Rep[WEither[L, R]] = {
      asRep[WEither[L, R]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[WEither[L, R]]))
    }

    def left: Rep[Costed[Unit]] = {
      asRep[Costed[Unit]](mkMethodCall(source,
        thisClass.getMethod("left"),
        List(),
        true, true, element[Costed[Unit]]))
    }

    def right: Rep[Costed[Unit]] = {
      asRep[Costed[Unit]](mkMethodCall(source,
        thisClass.getMethod("right"),
        List(),
        true, true, element[Costed[Unit]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedSum[L, R](p: Rep[CostedSum[L, R]]): CostedSum[L, R] = {
    if (p.rhs.isInstanceOf[CostedSum[L, R]@unchecked]) p.rhs.asInstanceOf[CostedSum[L, R]]
    else
      CostedSumAdapter(p)
  }

  // familyElem
  class CostedSumElem[L, R, To <: CostedSum[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CostedElem[WEither[L, R], To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(costedElement(wEitherElement(element[L], element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedSum[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedSum[L, R]] => convertCostedSum(x) }
      tryConvert(element[CostedSum[L, R]], this, x, conv)
    }

    def convertCostedSum(x: Rep[CostedSum[L, R]]): Rep[To] = {
      x.elem match {
        case _: CostedSumElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedSumElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedSumElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedSum[L, R]] =
    cachedElem[CostedSumElem[L, R, CostedSum[L, R]]](eL, eR)

  implicit case object CostedSumCompanionElem extends CompanionElem[CostedSumCompanionCtor] {
    lazy val tag = weakTypeTag[CostedSumCompanionCtor]
    protected def getDefaultRep = RCostedSum
  }

  abstract class CostedSumCompanionCtor extends CompanionDef[CostedSumCompanionCtor] with CostedSumCompanion {
    def selfType = CostedSumCompanionElem
    override def toString = "CostedSum"
  }
  implicit def proxyCostedSumCompanionCtor(p: Rep[CostedSumCompanionCtor]): CostedSumCompanionCtor =
    proxyOps[CostedSumCompanionCtor](p)

  lazy val RCostedSum: Rep[CostedSumCompanionCtor] = new CostedSumCompanionCtor {
    private val thisClass = classOf[CostedSumCompanion]
  }

  object CostedSumMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSumElem[_, _, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object left {
      def unapply(d: Def[_]): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSumElem[_, _, _]] && method.getName == "left" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object right {
      def unapply(d: Def[_]): Nullable[Rep[CostedSum[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedSumElem[_, _, _]] && method.getName == "right" =>
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

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CostedFunc[Env, Arg, Res]] =
    cachedElem[CostedFuncElem[Env, Arg, Res, CostedFunc[Env, Arg, Res]]](eEnv, eArg, eRes)

  implicit case object CostedFuncCompanionElem extends CompanionElem[CostedFuncCompanionCtor] {
    lazy val tag = weakTypeTag[CostedFuncCompanionCtor]
    protected def getDefaultRep = RCostedFunc
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

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedFuncElem[_, _, _, _]] && method.getName == "dataSize" =>
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

object CostedCol extends EntityObject("CostedCol") {
  // entityAdapter for CostedCol trait
  case class CostedColAdapter[Item](source: Rep[CostedCol[Item]])
      extends CostedCol[Item] with Def[CostedCol[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Col[Item]] = implicitly[Elem[Col[Item]]]
    val selfType: Elem[CostedCol[Item]] = element[CostedCol[Item]]
    override def transform(t: Transformer) = CostedColAdapter[Item](t(source))
    private val thisClass = classOf[CostedCol[Item]]

    def values: Rep[Col[Item]] = {
      asRep[Col[Item]](mkMethodCall(source,
        thisClass.getMethod("values"),
        List(),
        true, true, element[Col[Item]]))
    }

    def costs: Rep[Col[Int]] = {
      asRep[Col[Int]](mkMethodCall(source,
        thisClass.getMethod("costs"),
        List(),
        true, true, element[Col[Int]]))
    }

    def sizes: Rep[Col[Long]] = {
      asRep[Col[Long]](mkMethodCall(source,
        thisClass.getMethod("sizes"),
        List(),
        true, true, element[Col[Long]]))
    }

    def valuesCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("valuesCost"),
        List(),
        true, true, element[Int]))
    }

    def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedCol[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedCol[Res]](mkMethodCall(source,
        thisClass.getMethod("mapCosted", classOf[Sym]),
        List(f),
        true, true, element[CostedCol[Res]]))
    }

    def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedCol[Item]] = {
      asRep[CostedCol[Item]](mkMethodCall(source,
        thisClass.getMethod("filterCosted", classOf[Sym]),
        List(f),
        true, true, element[CostedCol[Item]]))
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

    def value: Rep[Col[Item]] = {
      asRep[Col[Item]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Col[Item]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedCol[Item](p: Rep[CostedCol[Item]]): CostedCol[Item] = {
    if (p.rhs.isInstanceOf[CostedCol[Item]@unchecked]) p.rhs.asInstanceOf[CostedCol[Item]]
    else
      CostedColAdapter(p)
  }

  // familyElem
  class CostedColElem[Item, To <: CostedCol[Item]](implicit _eItem: Elem[Item])
    extends CostedElem[Col[Item], To] {
    def eItem = _eItem

    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedCol[Item]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedCol[Item]] => convertCostedCol(x) }
      tryConvert(element[CostedCol[Item]], this, x, conv)
    }

    def convertCostedCol(x: Rep[CostedCol[Item]]): Rep[To] = {
      x.elem match {
        case _: CostedColElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedColElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedColElement[Item](implicit eItem: Elem[Item]): Elem[CostedCol[Item]] =
    cachedElem[CostedColElem[Item, CostedCol[Item]]](eItem)

  implicit case object CostedColCompanionElem extends CompanionElem[CostedColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedColCompanionCtor]
    protected def getDefaultRep = RCostedCol
  }

  abstract class CostedColCompanionCtor extends CompanionDef[CostedColCompanionCtor] with CostedColCompanion {
    def selfType = CostedColCompanionElem
    override def toString = "CostedCol"
  }
  implicit def proxyCostedColCompanionCtor(p: Rep[CostedColCompanionCtor]): CostedColCompanionCtor =
    proxyOps[CostedColCompanionCtor](p)

  lazy val RCostedCol: Rep[CostedColCompanionCtor] = new CostedColCompanionCtor {
    private val thisClass = classOf[CostedColCompanion]
  }

  object CostedColMethods {
    object values {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "values" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costs {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "costs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sizes {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "sizes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedCol[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object valuesCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "valuesCost" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "mapCosted" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "filterCosted" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedCol[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedColElem[_, _]] && method.getName == "foldCosted" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedCol[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CostedColCompanionMethods {
  }
} // of object CostedCol
  registerEntityObject("CostedCol", CostedCol)

object CostedPairCol extends EntityObject("CostedPairCol") {
  // entityAdapter for CostedPairCol trait
  case class CostedPairColAdapter[L, R](source: Rep[CostedPairCol[L, R]])
      extends CostedPairCol[L, R] with Def[CostedPairCol[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[Col[(L, R)]] = implicitly[Elem[Col[(L, R)]]]
    val selfType: Elem[CostedPairCol[L, R]] = element[CostedPairCol[L, R]]
    override def transform(t: Transformer) = CostedPairColAdapter[L, R](t(source))
    private val thisClass = classOf[CostedPairCol[L, R]]

    def ls: Rep[Costed[Col[L]]] = {
      asRep[Costed[Col[L]]](mkMethodCall(source,
        thisClass.getMethod("ls"),
        List(),
        true, true, element[Costed[Col[L]]]))
    }

    def rs: Rep[Costed[Col[R]]] = {
      asRep[Costed[Col[R]]](mkMethodCall(source,
        thisClass.getMethod("rs"),
        List(),
        true, true, element[Costed[Col[R]]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Col[(L, R)]] = {
      asRep[Col[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Col[(L, R)]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedPairCol[L, R](p: Rep[CostedPairCol[L, R]]): CostedPairCol[L, R] = {
    if (p.rhs.isInstanceOf[CostedPairCol[L, R]@unchecked]) p.rhs.asInstanceOf[CostedPairCol[L, R]]
    else
      CostedPairColAdapter(p)
  }

  // familyElem
  class CostedPairColElem[L, R, To <: CostedPairCol[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CostedElem[Col[(L, R)], To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(pairElement(element[L],element[R]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPairCol[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedPairCol[L, R]] => convertCostedPairCol(x) }
      tryConvert(element[CostedPairCol[L, R]], this, x, conv)
    }

    def convertCostedPairCol(x: Rep[CostedPairCol[L, R]]): Rep[To] = {
      x.elem match {
        case _: CostedPairColElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedPairColElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedPairColElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedPairCol[L, R]] =
    cachedElem[CostedPairColElem[L, R, CostedPairCol[L, R]]](eL, eR)

  implicit case object CostedPairColCompanionElem extends CompanionElem[CostedPairColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedPairColCompanionCtor]
    protected def getDefaultRep = RCostedPairCol
  }

  abstract class CostedPairColCompanionCtor extends CompanionDef[CostedPairColCompanionCtor] with CostedPairColCompanion {
    def selfType = CostedPairColCompanionElem
    override def toString = "CostedPairCol"
  }
  implicit def proxyCostedPairColCompanionCtor(p: Rep[CostedPairColCompanionCtor]): CostedPairColCompanionCtor =
    proxyOps[CostedPairColCompanionCtor](p)

  lazy val RCostedPairCol: Rep[CostedPairColCompanionCtor] = new CostedPairColCompanionCtor {
    private val thisClass = classOf[CostedPairColCompanion]
  }

  object CostedPairColMethods {
    object ls {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _, _]] && method.getName == "ls" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object rs {
      def unapply(d: Def[_]): Nullable[Rep[CostedPairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedPairColElem[_, _, _]] && method.getName == "rs" =>
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

object CostedNestedCol extends EntityObject("CostedNestedCol") {
  // entityAdapter for CostedNestedCol trait
  case class CostedNestedColAdapter[Item](source: Rep[CostedNestedCol[Item]])
      extends CostedNestedCol[Item] with Def[CostedNestedCol[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Col[Col[Item]]] = implicitly[Elem[Col[Col[Item]]]]
    val selfType: Elem[CostedNestedCol[Item]] = element[CostedNestedCol[Item]]
    override def transform(t: Transformer) = CostedNestedColAdapter[Item](t(source))
    private val thisClass = classOf[CostedNestedCol[Item]]

    def rows: Rep[Col[Costed[Col[Item]]]] = {
      asRep[Col[Costed[Col[Item]]]](mkMethodCall(source,
        thisClass.getMethod("rows"),
        List(),
        true, true, element[Col[Costed[Col[Item]]]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Col[Col[Item]]] = {
      asRep[Col[Col[Item]]](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[Col[Col[Item]]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("cost"),
        List(),
        true, true, element[Int]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedNestedCol[Item](p: Rep[CostedNestedCol[Item]]): CostedNestedCol[Item] = {
    if (p.rhs.isInstanceOf[CostedNestedCol[Item]@unchecked]) p.rhs.asInstanceOf[CostedNestedCol[Item]]
    else
      CostedNestedColAdapter(p)
  }

  // familyElem
  class CostedNestedColElem[Item, To <: CostedNestedCol[Item]](implicit _eItem: Elem[Item])
    extends CostedElem[Col[Col[Item]], To] {
    def eItem = _eItem

    override lazy val parent: Option[Elem[_]] = Some(costedElement(colElement(colElement(element[Item]))))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedNestedCol[Item]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedNestedCol[Item]] => convertCostedNestedCol(x) }
      tryConvert(element[CostedNestedCol[Item]], this, x, conv)
    }

    def convertCostedNestedCol(x: Rep[CostedNestedCol[Item]]): Rep[To] = {
      x.elem match {
        case _: CostedNestedColElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have CostedNestedColElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedNestedColElement[Item](implicit eItem: Elem[Item]): Elem[CostedNestedCol[Item]] =
    cachedElem[CostedNestedColElem[Item, CostedNestedCol[Item]]](eItem)

  implicit case object CostedNestedColCompanionElem extends CompanionElem[CostedNestedColCompanionCtor] {
    lazy val tag = weakTypeTag[CostedNestedColCompanionCtor]
    protected def getDefaultRep = RCostedNestedCol
  }

  abstract class CostedNestedColCompanionCtor extends CompanionDef[CostedNestedColCompanionCtor] with CostedNestedColCompanion {
    def selfType = CostedNestedColCompanionElem
    override def toString = "CostedNestedCol"
  }
  implicit def proxyCostedNestedColCompanionCtor(p: Rep[CostedNestedColCompanionCtor]): CostedNestedColCompanionCtor =
    proxyOps[CostedNestedColCompanionCtor](p)

  lazy val RCostedNestedCol: Rep[CostedNestedColCompanionCtor] = new CostedNestedColCompanionCtor {
    private val thisClass = classOf[CostedNestedColCompanion]
  }

  object CostedNestedColMethods {
    object rows {
      def unapply(d: Def[_]): Nullable[Rep[CostedNestedCol[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedNestedColElem[_, _]] && method.getName == "rows" =>
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

object CostedOption extends EntityObject("CostedOption") {
  // entityAdapter for CostedOption trait
  case class CostedOptionAdapter[T](source: Rep[CostedOption[T]])
      extends CostedOption[T] with Def[CostedOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val selfType: Elem[CostedOption[T]] = element[CostedOption[T]]
    override def transform(t: Transformer) = CostedOptionAdapter[T](t(source))
    private val thisClass = classOf[CostedOption[T]]

    def get: Rep[Costed[T]] = {
      asRep[Costed[T]](mkMethodCall(source,
        thisClass.getMethod("get"),
        List(),
        true, true, element[Costed[T]]))
    }

    def getOrElse(default: Rep[Costed[T]]): Rep[Costed[T]] = {
      asRep[Costed[T]](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym]),
        List(default),
        true, true, element[Costed[T]]))
    }

    def fold[B](ifEmpty: Rep[Costed[B]], f: Rep[Costed[T => B]]): Rep[Costed[B]] = {
      implicit val eB = ifEmpty.eVal
      asRep[Costed[B]](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(ifEmpty, f),
        true, true, element[Costed[B]]))
    }

    def isEmpty: Rep[Costed[Boolean]] = {
      asRep[Costed[Boolean]](mkMethodCall(source,
        thisClass.getMethod("isEmpty"),
        List(),
        true, true, element[Costed[Boolean]]))
    }

    def isDefined: Rep[Costed[Boolean]] = {
      asRep[Costed[Boolean]](mkMethodCall(source,
        thisClass.getMethod("isDefined"),
        List(),
        true, true, element[Costed[Boolean]]))
    }

    def filter(p: Rep[Costed[T => Boolean]]): Rep[Costed[WOption[T]]] = {
      asRep[Costed[WOption[T]]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Costed[WOption[T]]]))
    }

    def flatMap[B](f: Rep[Costed[T => WOption[B]]]): Rep[Costed[WOption[B]]] = {
      implicit val eB = f.eVal.eRange.typeArgs("A")._1.asElem[B]
      asRep[Costed[WOption[B]]](mkMethodCall(source,
        thisClass.getMethod("flatMap", classOf[Sym]),
        List(f),
        true, true, element[Costed[WOption[B]]]))
    }

    def map[B](f: Rep[Costed[T => B]]): Rep[Costed[WOption[B]]] = {
      implicit val eB = f.eVal.eRange
      asRep[Costed[WOption[B]]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Costed[WOption[B]]]))
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

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedOptionElement[T](implicit eT: Elem[T]): Elem[CostedOption[T]] =
    cachedElem[CostedOptionElem[T, CostedOption[T]]](eT)

  implicit case object CostedOptionCompanionElem extends CompanionElem[CostedOptionCompanionCtor] {
    lazy val tag = weakTypeTag[CostedOptionCompanionCtor]
    protected def getDefaultRep = RCostedOption
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
    object get {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "get" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[CostedOption[T]], Rep[Costed[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedOption[T]], Rep[Costed[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedOption[T]], Rep[Costed[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[CostedOption[T]], Rep[Costed[B]], Rep[Costed[T => B]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedOption[T]], Rep[Costed[B]], Rep[Costed[T => B]]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedOption[T]], Rep[Costed[B]], Rep[Costed[T => B]]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "isEmpty" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isDefined {
      def unapply(d: Def[_]): Nullable[Rep[CostedOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "isDefined" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => Boolean]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedOption[T]], Rep[Costed[T => Boolean]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => Boolean]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => WOption[B]]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "flatMap" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedOption[T]], Rep[Costed[T => WOption[B]]]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => WOption[B]]]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => B]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedOptionElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedOption[T]], Rep[Costed[T => B]]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedOption[T]], Rep[Costed[T => B]]) forSome {type T; type B}] = exp match {
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

    def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Long]): Rep[CostedPrim[T]] = {
      implicit val eT = value.elem
      asRep[CostedPrim[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedPrim", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(value, cost, size),
        true, true, element[CostedPrim[T]]))
    }

    def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]]): Rep[CostedPair[L, R]] = {
      implicit val eL = first.eVal
implicit val eR = second.eVal
      asRep[CostedPair[L, R]](mkMethodCall(source,
        thisClass.getMethod("mkCostedPair", classOf[Sym], classOf[Sym]),
        List(first, second),
        true, true, element[CostedPair[L, R]]))
    }

    def mkCostedSum[L, R](value: Rep[WEither[L, R]], left: Rep[Costed[Unit]], right: Rep[Costed[Unit]]): Rep[CostedSum[L, R]] = {
      implicit val eL = value.eA
implicit val eR = value.eB
      asRep[CostedSum[L, R]](mkMethodCall(source,
        thisClass.getMethod("mkCostedSum", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(value, left, right),
        true, true, element[CostedSum[L, R]]))
    }

    def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], dataSize: Rep[Long]): Rep[CostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = envCosted.eVal
implicit val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg]
implicit val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedFunc[Env, Arg, Res]](mkMethodCall(source,
        thisClass.getMethod("mkCostedFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(envCosted, func, cost, dataSize),
        true, true, element[CostedFunc[Env, Arg, Res]]))
    }

    def mkCostedCol[T](values: Rep[Col[T]], costs: Rep[Col[Int]], sizes: Rep[Col[Long]], valuesCost: Rep[Int]): Rep[CostedCol[T]] = {
      implicit val eT = values.eA
      asRep[CostedCol[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedCol", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(values, costs, sizes, valuesCost),
        true, true, element[CostedCol[T]]))
    }

    def mkCostedPairCol[L, R](ls: Rep[Costed[Col[L]]], rs: Rep[Costed[Col[R]]]): Rep[CostedPairCol[L, R]] = {
      implicit val eL = ls.eVal.typeArgs("A")._1.asElem[L]
implicit val eR = rs.eVal.typeArgs("A")._1.asElem[R]
      asRep[CostedPairCol[L, R]](mkMethodCall(source,
        thisClass.getMethod("mkCostedPairCol", classOf[Sym], classOf[Sym]),
        List(ls, rs),
        true, true, element[CostedPairCol[L, R]]))
    }

    def mkCostedNestedCol[Item](rows: Rep[Col[Costed[Col[Item]]]]): Rep[CostedNestedCol[Item]] = {
      implicit val eItem = rows.eA.typeArgs("Val")._1.asElem[Col[Item]].typeArgs("A")._1.asElem[Item]
      asRep[CostedNestedCol[Item]](mkMethodCall(source,
        thisClass.getMethod("mkCostedNestedCol", classOf[Sym]),
        List(rows),
        true, true, element[CostedNestedCol[Item]]))
    }

    def mkCostedSome[T](costedValue: Rep[Costed[T]]): Rep[CostedOption[T]] = {
      implicit val eT = costedValue.eVal
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedSome", classOf[Sym]),
        List(costedValue),
        true, true, element[CostedOption[T]]))
    }

    def mkCostedNone[T](cost: Rep[Int])(implicit eT: Elem[T]): Rep[CostedOption[T]] = {
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedNone", classOf[Sym], classOf[Elem[_]]),
        List(cost, eT),
        true, true, element[CostedOption[T]]))
    }

    def mkCostedOption[T](value: Rep[WOption[T]], none: Rep[Costed[Unit]], some: Rep[Costed[Unit]]): Rep[CostedOption[T]] = {
      implicit val eT = value.eA
      asRep[CostedOption[T]](mkMethodCall(source,
        thisClass.getMethod("mkCostedOption", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(value, none, some),
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
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val costedBuilderElement: Elem[CostedBuilder] =
    new CostedBuilderElem[CostedBuilder]

  implicit case object CostedBuilderCompanionElem extends CompanionElem[CostedBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CostedBuilderCompanionCtor]
    protected def getDefaultRep = RCostedBuilder
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

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedPrim" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Long]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedPair" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedSum {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedSum" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WEither[L, R]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedFunc" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Long]) forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedCol {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedCol" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Col[T]], Rep[Col[Int]], Rep[Col[Long]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedPairCol {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedPairCol" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[Col[L]]], Rep[Costed[Col[R]]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedNestedCol {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedNestedCol" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Col[Costed[Col[Item]]]]) forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedSome {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedSome" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedNone {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedNone" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "mkCostedOption" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[Costed[Unit]], Rep[Costed[Unit]]) forSome {type T}] = exp match {
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
