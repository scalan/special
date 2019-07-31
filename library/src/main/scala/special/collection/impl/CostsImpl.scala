package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

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
  private val CostedClass = classOf[Costed[_]]

  // entityAdapter for Costed trait
  case class CostedAdapter[Val](source: Rep[Costed[Val]])
      extends Costed[Val]
      with Def[Costed[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[Costed[Val]] = element[Costed[Val]]
    override def transform(t: Transformer) = CostedAdapter[Val](t(source))

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Val] = {
      asRep[Val](mkMethodCall(source,
        CostedClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Val]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Rep[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        CostedClass.getMethod("size"),
        WrappedArray.empty,
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
  }

  implicit def costedElement[Val](implicit eVal: Elem[Val]): Elem[Costed[Val]] =
    cachedElemByClass(eVal)(classOf[CostedElem[Val, Costed[Val]]])

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor]

  abstract class CostedCompanionCtor extends CompanionDef[CostedCompanionCtor] with CostedCompanion {
    def selfType = CostedCompanionElem
    override def toString = "Costed"
  }
  implicit def proxyCostedCompanionCtor(p: Rep[CostedCompanionCtor]): CostedCompanionCtor =
    p.rhs.asInstanceOf[CostedCompanionCtor]

  lazy val RCosted: Rep[CostedCompanionCtor] = new CostedCompanionCtor {
    private val thisClass = classOf[CostedCompanion]
  }

  object CostedMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "builder" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = unapply(exp.rhs)
    }

    object value {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "value" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = unapply(exp.rhs)
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "cost" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = unapply(exp.rhs)
    }

    object size {
      def unapply(d: Def[_]): Nullable[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "size" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Costed[Val]] forSome {type Val}] = unapply(exp.rhs)
    }
  }

  object CostedCompanionMethods {
  }
} // of object Costed
  registerEntityObject("Costed", Costed)

object CostedPrim extends EntityObject("CostedPrim") {
  private val CostedPrimClass = classOf[CostedPrim[_]]

  // entityAdapter for CostedPrim trait
  case class CostedPrimAdapter[Val](source: Rep[CostedPrim[Val]])
      extends CostedPrim[Val]
      with Def[CostedPrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[CostedPrim[Val]] = element[CostedPrim[Val]]
    override def transform(t: Transformer) = CostedPrimAdapter[Val](t(source))

    def value: Rep[Val] = {
      asRep[Val](mkMethodCall(source,
        CostedPrimClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Val]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPrimClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Rep[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        CostedPrimClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[Val]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedPrimClass.getMethod("builder"),
        WrappedArray.empty,
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
  }

  implicit def costedPrimElement[Val](implicit eVal: Elem[Val]): Elem[CostedPrim[Val]] =
    cachedElemByClass(eVal)(classOf[CostedPrimElem[Val, CostedPrim[Val]]])

  implicit case object CostedPrimCompanionElem extends CompanionElem[CostedPrimCompanionCtor]

  abstract class CostedPrimCompanionCtor extends CompanionDef[CostedPrimCompanionCtor] with CostedPrimCompanion {
    def selfType = CostedPrimCompanionElem
    override def toString = "CostedPrim"
  }
  implicit def proxyCostedPrimCompanionCtor(p: Rep[CostedPrimCompanionCtor]): CostedPrimCompanionCtor =
    p.rhs.asInstanceOf[CostedPrimCompanionCtor]

  lazy val RCostedPrim: Rep[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor {
    private val thisClass = classOf[CostedPrimCompanion]
  }
} // of object CostedPrim
  registerEntityObject("CostedPrim", CostedPrim)

object CostedPair extends EntityObject("CostedPair") {
  private val CostedPairClass = classOf[CostedPair[_, _]]

  // entityAdapter for CostedPair trait
  case class CostedPairAdapter[L, R](source: Rep[CostedPair[L, R]])
      extends CostedPair[L, R]
      with Def[CostedPair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[CostedPair[L, R]] = element[CostedPair[L, R]]
    override def transform(t: Transformer) = CostedPairAdapter[L, R](t(source))

    def l: Rep[Costed[L]] = {
      asRep[Costed[L]](mkMethodCall(source,
        CostedPairClass.getMethod("l"),
        WrappedArray.empty,
        true, true, element[Costed[L]]))
    }

    def r: Rep[Costed[R]] = {
      asRep[Costed[R]](mkMethodCall(source,
        CostedPairClass.getMethod("r"),
        WrappedArray.empty,
        true, true, element[Costed[R]]))
    }

    def accCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPairClass.getMethod("accCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedPairClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        CostedPairClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[(L, R)]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPairClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Rep[Size[(L, R)]] = {
      asRep[Size[(L, R)]](mkMethodCall(source,
        CostedPairClass.getMethod("size"),
        WrappedArray.empty,
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
  }

  implicit def costedPairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedPair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[CostedPairElem[L, R, CostedPair[L, R]]])

  implicit case object CostedPairCompanionElem extends CompanionElem[CostedPairCompanionCtor]

  abstract class CostedPairCompanionCtor extends CompanionDef[CostedPairCompanionCtor] with CostedPairCompanion {
    def selfType = CostedPairCompanionElem
    override def toString = "CostedPair"
  }
  implicit def proxyCostedPairCompanionCtor(p: Rep[CostedPairCompanionCtor]): CostedPairCompanionCtor =
    p.rhs.asInstanceOf[CostedPairCompanionCtor]

  lazy val RCostedPair: Rep[CostedPairCompanionCtor] = new CostedPairCompanionCtor {
    private val thisClass = classOf[CostedPairCompanion]
  }
} // of object CostedPair
  registerEntityObject("CostedPair", CostedPair)

object CostedFunc extends EntityObject("CostedFunc") {
  private val CostedFuncClass = classOf[CostedFunc[_, _, _]]

  // entityAdapter for CostedFunc trait
  case class CostedFuncAdapter[Env, Arg, Res](source: Rep[CostedFunc[Env, Arg, Res]])
      extends CostedFunc[Env, Arg, Res]
      with Def[CostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asElem[Env];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asElem[Arg];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val selfType: Elem[CostedFunc[Env, Arg, Res]] = element[CostedFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CostedFuncAdapter[Env, Arg, Res](t(source))

    def envCosted: Rep[Costed[Env]] = {
      asRep[Costed[Env]](mkMethodCall(source,
        CostedFuncClass.getMethod("envCosted"),
        WrappedArray.empty,
        true, true, element[Costed[Env]]))
    }

    def func: Rep[Costed[Arg] => Costed[Res]] = {
      asRep[Costed[Arg] => Costed[Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("func"),
        WrappedArray.empty,
        true, true, element[Costed[Arg] => Costed[Res]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedFuncClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def sliceCalc: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCalc"),
        WrappedArray.empty,
        true, true, element[Arg => Res]))
    }

    def sliceCost: Rep[((Int, Size[Arg])) => Int] = {
      asRep[((Int, Size[Arg])) => Int](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCost"),
        WrappedArray.empty,
        true, true, element[((Int, Size[Arg])) => Int]))
    }

    def sliceCostEx: Rep[((Arg, (Int, Size[Arg]))) => Int] = {
      asRep[((Arg, (Int, Size[Arg]))) => Int](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCostEx"),
        WrappedArray.empty,
        true, true, element[((Arg, (Int, Size[Arg]))) => Int]))
    }

    def sliceSize: Rep[Size[Arg] => Size[Res]] = {
      asRep[Size[Arg] => Size[Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceSize"),
        WrappedArray.empty,
        true, true, element[Size[Arg] => Size[Res]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedFuncClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        CostedFuncClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Arg => Res]))
    }

    def size: Rep[Size[Arg => Res]] = {
      asRep[Size[Arg => Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("size"),
        WrappedArray.empty,
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
  }

  implicit def costedFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CostedFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[CostedFuncElem[Env, Arg, Res, CostedFunc[Env, Arg, Res]]])

  implicit case object CostedFuncCompanionElem extends CompanionElem[CostedFuncCompanionCtor]

  abstract class CostedFuncCompanionCtor extends CompanionDef[CostedFuncCompanionCtor] with CostedFuncCompanion {
    def selfType = CostedFuncCompanionElem
    override def toString = "CostedFunc"
  }
  implicit def proxyCostedFuncCompanionCtor(p: Rep[CostedFuncCompanionCtor]): CostedFuncCompanionCtor =
    p.rhs.asInstanceOf[CostedFuncCompanionCtor]

  lazy val RCostedFunc: Rep[CostedFuncCompanionCtor] = new CostedFuncCompanionCtor {
    private val thisClass = classOf[CostedFuncCompanion]
  }
} // of object CostedFunc
  registerEntityObject("CostedFunc", CostedFunc)

object CostedColl extends EntityObject("CostedColl") {
  private val CostedCollClass = classOf[CostedColl[_]]

  // entityAdapter for CostedColl trait
  case class CostedCollAdapter[Item](source: Rep[CostedColl[Item]])
      extends CostedColl[Item]
      with Def[CostedColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val selfType: Elem[CostedColl[Item]] = element[CostedColl[Item]]
    override def transform(t: Transformer) = CostedCollAdapter[Item](t(source))

    def values: Rep[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("values"),
        WrappedArray.empty,
        true, true, element[Coll[Item]]))
    }

    def costs: Rep[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        CostedCollClass.getMethod("costs"),
        WrappedArray.empty,
        true, true, element[Coll[Int]]))
    }

    def sizes: Rep[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        CostedCollClass.getMethod("sizes"),
        WrappedArray.empty,
        true, true, element[Coll[Size[Item]]]))
    }

    def valuesCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedCollClass.getMethod("valuesCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def mapCosted[Res](f: Rep[Costed[Item] => Costed[Res]]): Rep[CostedColl[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedColl[Res]](mkMethodCall(source,
        CostedCollClass.getMethod("mapCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[CostedColl[Res]]))
    }

    def filterCosted(f: Rep[Costed[Item] => Costed[Boolean]]): Rep[CostedColl[Item]] = {
      asRep[CostedColl[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("filterCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[CostedColl[Item]]))
    }

    def foldCosted[B](zero: Rep[Costed[B]], op: Rep[Costed[(B, Item)] => Costed[B]]): Rep[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(source,
        CostedCollClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[Costed[B]]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedCollClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Rep[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Coll[Item]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedCollClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Rep[Size[Coll[Item]]] = {
      asRep[Size[Coll[Item]]](mkMethodCall(source,
        CostedCollClass.getMethod("size"),
        WrappedArray.empty,
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
  }

  implicit def costedCollElement[Item](implicit eItem: Elem[Item]): Elem[CostedColl[Item]] =
    cachedElemByClass(eItem)(classOf[CostedCollElem[Item, CostedColl[Item]]])

  implicit case object CostedCollCompanionElem extends CompanionElem[CostedCollCompanionCtor]

  abstract class CostedCollCompanionCtor extends CompanionDef[CostedCollCompanionCtor] with CostedCollCompanion {
    def selfType = CostedCollCompanionElem
    override def toString = "CostedColl"
  }
  implicit def proxyCostedCollCompanionCtor(p: Rep[CostedCollCompanionCtor]): CostedCollCompanionCtor =
    p.rhs.asInstanceOf[CostedCollCompanionCtor]

  lazy val RCostedColl: Rep[CostedCollCompanionCtor] = new CostedCollCompanionCtor {
    private val thisClass = classOf[CostedCollCompanion]
  }

  object CostedCollMethods {
    object values {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "values" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = unapply(exp.rhs)
    }

    object costs {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "costs" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = unapply(exp.rhs)
    }

    object sizes {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizes" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = unapply(exp.rhs)
    }

    object valuesCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "valuesCost" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedColl[Item]] forSome {type Item}] = unapply(exp.rhs)
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mapCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = unapply(exp.rhs)
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filterCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = unapply(exp.rhs)
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "foldCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedColl[Item]], Rep[Costed[B]], Rep[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = unapply(exp.rhs)
    }
  }

  object CostedCollCompanionMethods {
  }
} // of object CostedColl
  registerEntityObject("CostedColl", CostedColl)

object CostedOption extends EntityObject("CostedOption") {
  private val CostedOptionClass = classOf[CostedOption[_]]

  // entityAdapter for CostedOption trait
  case class CostedOptionAdapter[T](source: Rep[CostedOption[T]])
      extends CostedOption[T]
      with Def[CostedOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val selfType: Elem[CostedOption[T]] = element[CostedOption[T]]
    override def transform(t: Transformer) = CostedOptionAdapter[T](t(source))

    def costOpt: Rep[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        CostedOptionClass.getMethod("costOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Int]]))
    }

    def sizeOpt: Rep[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        CostedOptionClass.getMethod("sizeOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Size[T]]]))
    }

    def accumulatedCost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedOptionClass.getMethod("accumulatedCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def builder: Rep[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedOptionClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        CostedOptionClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[WOption[T]]))
    }

    def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedOptionClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Rep[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        CostedOptionClass.getMethod("size"),
        WrappedArray.empty,
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
  }

  implicit def costedOptionElement[T](implicit eT: Elem[T]): Elem[CostedOption[T]] =
    cachedElemByClass(eT)(classOf[CostedOptionElem[T, CostedOption[T]]])

  implicit case object CostedOptionCompanionElem extends CompanionElem[CostedOptionCompanionCtor]

  abstract class CostedOptionCompanionCtor extends CompanionDef[CostedOptionCompanionCtor] with CostedOptionCompanion {
    def selfType = CostedOptionCompanionElem
    override def toString = "CostedOption"
  }
  implicit def proxyCostedOptionCompanionCtor(p: Rep[CostedOptionCompanionCtor]): CostedOptionCompanionCtor =
    p.rhs.asInstanceOf[CostedOptionCompanionCtor]

  lazy val RCostedOption: Rep[CostedOptionCompanionCtor] = new CostedOptionCompanionCtor {
    private val thisClass = classOf[CostedOptionCompanion]
  }
} // of object CostedOption
  registerEntityObject("CostedOption", CostedOption)

object CostedBuilder extends EntityObject("CostedBuilder") {
  private val CostedBuilderClass = classOf[CostedBuilder]

  // entityAdapter for CostedBuilder trait
  case class CostedBuilderAdapter(source: Rep[CostedBuilder])
      extends CostedBuilder
      with Def[CostedBuilder] {
    val selfType: Elem[CostedBuilder] = element[CostedBuilder]
    override def transform(t: Transformer) = CostedBuilderAdapter(t(source))

    def costedValue[T](x: Rep[T], optCost: Rep[WOption[Int]]): Rep[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, optCost),
        true, true, element[Costed[T]]))
    }

    def defaultValue[T](valueType: Rep[WRType[T]]): Rep[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(source,
        CostedBuilderClass.getMethod("defaultValue", classOf[Sym]),
        Array[AnyRef](valueType),
        true, true, element[T]))
    }

    def monoidBuilder: Rep[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        CostedBuilderClass.getMethod("monoidBuilder"),
        WrappedArray.empty,
        true, true, element[MonoidBuilder]))
    }

    def mkSizePrim[T](dataSize: Rep[Long], tT: Rep[WRType[T]]): Rep[SizePrim[T]] = {
      implicit val eT = tT.eA
      asRep[SizePrim[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizePrim", classOf[Sym], classOf[Sym]),
        Array[AnyRef](dataSize, tT),
        true, true, element[SizePrim[T]]))
    }

    def mkSizePair[L, R](l: Rep[Size[L]], r: Rep[Size[R]]): Rep[SizePair[L, R]] = {
      implicit val eL = l.eVal
implicit val eR = r.eVal
      asRep[SizePair[L, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizePair", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, true, element[SizePair[L, R]]))
    }

    def mkSizeColl[T](sizes: Rep[Coll[Size[T]]]): Rep[SizeColl[T]] = {
      implicit val eT = sizes.eA.typeArgs("Val")._1.asElem[T]
      asRep[SizeColl[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeColl", classOf[Sym]),
        Array[AnyRef](sizes),
        true, true, element[SizeColl[T]]))
    }

    def mkSizeFunc[E, A, R](sizeEnv: Rep[Size[E]], sizeFunc: Rep[Long], tA: Rep[WRType[A]], tR: Rep[WRType[R]]): Rep[SizeFunc[E, A, R]] = {
      implicit val eE = sizeEnv.eVal
implicit val eA = tA.eA
implicit val eR = tR.eA
      asRep[SizeFunc[E, A, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](sizeEnv, sizeFunc, tA, tR),
        true, true, element[SizeFunc[E, A, R]]))
    }

    def mkSizeOption[T](sizeOpt: Rep[WOption[Size[T]]]): Rep[SizeOption[T]] = {
      implicit val eT = sizeOpt.eA.typeArgs("Val")._1.asElem[T]
      asRep[SizeOption[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeOption", classOf[Sym]),
        Array[AnyRef](sizeOpt),
        true, true, element[SizeOption[T]]))
    }

    def mkCostedPrim[T](value: Rep[T], cost: Rep[Int], size: Rep[Size[T]]): Rep[CostedPrim[T]] = {
      implicit val eT = value.elem
      asRep[CostedPrim[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedPrim", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](value, cost, size),
        true, true, element[CostedPrim[T]]))
    }

    def mkCostedPair[L, R](first: Rep[Costed[L]], second: Rep[Costed[R]], accCost: Rep[Int]): Rep[CostedPair[L, R]] = {
      implicit val eL = first.eVal
implicit val eR = second.eVal
      asRep[CostedPair[L, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedPair", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](first, second, accCost),
        true, true, element[CostedPair[L, R]]))
    }

    def mkCostedFunc[Env, Arg, Res](envCosted: Rep[Costed[Env]], func: Rep[Costed[Arg] => Costed[Res]], cost: Rep[Int], size: Rep[Size[Arg => Res]]): Rep[CostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = envCosted.eVal
implicit val eArg = func.elem.eDom.typeArgs("Val")._1.asElem[Arg]
implicit val eRes = func.elem.eRange.typeArgs("Val")._1.asElem[Res]
      asRep[CostedFunc[Env, Arg, Res]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](envCosted, func, cost, size),
        true, true, element[CostedFunc[Env, Arg, Res]]))
    }

    def mkCostedColl[T](values: Rep[Coll[T]], costs: Rep[Coll[Int]], sizes: Rep[Coll[Size[T]]], valuesCost: Rep[Int]): Rep[CostedColl[T]] = {
      implicit val eT = values.eA
      asRep[CostedColl[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedColl", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](values, costs, sizes, valuesCost),
        true, true, element[CostedColl[T]]))
    }

    def mkCostedOption[T](value: Rep[WOption[T]], costOpt: Rep[WOption[Int]], sizeOpt: Rep[WOption[Size[T]]], accumulatedCost: Rep[Int]): Rep[CostedOption[T]] = {
      implicit val eT = value.eA
      asRep[CostedOption[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedOption", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](value, costOpt, sizeOpt, accumulatedCost),
        true, true, element[CostedOption[T]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedBuilder(p: Rep[CostedBuilder]): CostedBuilder = {
    if (p.rhs.isInstanceOf[CostedBuilder]) p.rhs.asInstanceOf[CostedBuilder]
    else
      CostedBuilderAdapter(p)
  }

  // familyElem
  class CostedBuilderElem[To <: CostedBuilder]
    extends EntityElem[To] {
  }

  implicit lazy val costedBuilderElement: Elem[CostedBuilder] =
    new CostedBuilderElem[CostedBuilder]

  implicit case object CostedBuilderCompanionElem extends CompanionElem[CostedBuilderCompanionCtor]

  abstract class CostedBuilderCompanionCtor extends CompanionDef[CostedBuilderCompanionCtor] with CostedBuilderCompanion {
    def selfType = CostedBuilderCompanionElem
    override def toString = "CostedBuilder"
  }
  implicit def proxyCostedBuilderCompanionCtor(p: Rep[CostedBuilderCompanionCtor]): CostedBuilderCompanionCtor =
    p.rhs.asInstanceOf[CostedBuilderCompanionCtor]

  lazy val RCostedBuilder: Rep[CostedBuilderCompanionCtor] = new CostedBuilderCompanionCtor {
    private val thisClass = classOf[CostedBuilderCompanion]
  }

  object CostedBuilderMethods {
    object ConstructTupleCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "ConstructTupleCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = unapply(exp.rhs)
    }

    object ConstructSumCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "ConstructSumCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = unapply(exp.rhs)
    }

    object SelectFieldCost {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "SelectFieldCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = unapply(exp.rhs)
    }

    object SumTagSize {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "SumTagSize" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = unapply(exp.rhs)
    }

    object costedValue {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "costedValue" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = unapply(exp.rhs)
    }

    object defaultValue {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "defaultValue" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = unapply(exp.rhs)
    }

    object monoidBuilder {
      def unapply(d: Def[_]): Nullable[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "monoidBuilder" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CostedBuilder]] = unapply(exp.rhs)
    }

    object mkSizePrim {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizePrim" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Long], Rep[WRType[T]]) forSome {type T}] = unapply(exp.rhs)
    }

    object mkSizePair {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizePair" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Size[L]], Rep[Size[R]]) forSome {type L; type R}] = unapply(exp.rhs)
    }

    object mkSizeColl {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeColl" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Coll[Size[T]]]) forSome {type T}] = unapply(exp.rhs)
    }

    object mkSizeFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeFunc" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Size[E]], Rep[Long], Rep[WRType[A]], Rep[WRType[R]]) forSome {type E; type A; type R}] = unapply(exp.rhs)
    }

    object mkSizeOption {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeOption" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WOption[Size[T]]]) forSome {type T}] = unapply(exp.rhs)
    }

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedPrim" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[T], Rep[Int], Rep[Size[T]]) forSome {type T}] = unapply(exp.rhs)
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedPair" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[L]], Rep[Costed[R]], Rep[Int]) forSome {type L; type R}] = unapply(exp.rhs)
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedFunc" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Costed[Env]], Rep[Costed[Arg] => Costed[Res]], Rep[Int], Rep[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = unapply(exp.rhs)
    }

    object mkCostedColl {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedColl" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[Coll[T]], Rep[Coll[Int]], Rep[Coll[Size[T]]], Rep[Int]) forSome {type T}] = unapply(exp.rhs)
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedOption" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CostedBuilder], Rep[WOption[T]], Rep[WOption[Int]], Rep[WOption[Size[T]]], Rep[Int]) forSome {type T}] = unapply(exp.rhs)
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
