package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SizesDefs extends scalan.Scalan with Sizes {
  self: Library =>
import IsoUR._
import Converter._
import Coll._
import Size._
import WOption._
import WRType._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._

object Size extends EntityObject("Size") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSize[Val] = special.collection.Size[Val]
  case class SizeConst[SVal, Val](
        constValue: SSize[SVal],
        lVal: Liftable[SVal, Val]
      ) extends Size[Val] with LiftedConst[SSize[SVal], Size[Val]]
        with Def[Size[Val]] with SizeConstMethods[Val] {
    implicit def eVal: Elem[Val] = lVal.eW

    val liftable: Liftable[SSize[SVal], Size[Val]] = liftableSize(lVal)
    val selfType: Elem[Size[Val]] = liftable.eW
  }

  trait SizeConstMethods[Val] extends Size[Val]  { thisConst: Def[_] =>
    implicit def eVal: Elem[Val]
    private val SizeClass = classOf[Size[Val]]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        SizeClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }

  case class LiftableSize[SVal, Val](lVal: Liftable[SVal, Val])
    extends Liftable[SSize[SVal], Size[Val]] {
    lazy val eW: Elem[Size[Val]] = sizeElement(lVal.eW)
    lazy val sourceType: RType[SSize[SVal]] = {
            implicit val tagSVal = lVal.sourceType.asInstanceOf[RType[SVal]]
      RType[SSize[SVal]]
    }
    def lift(x: SSize[SVal]): Rep[Size[Val]] = SizeConst(x, lVal)
    def unlift(w: Rep[Size[Val]]): SSize[SVal] = w match {
      case Def(SizeConst(x: SSize[_], _lVal))
            if _lVal == lVal => x.asInstanceOf[SSize[SVal]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSize[SVal, Val](implicit lVal: Liftable[SVal,Val]): Liftable[SSize[SVal], Size[Val]] =
    LiftableSize(lVal)

  private val SizeClass = classOf[Size[_]]

  // entityAdapter for Size trait
  case class SizeAdapter[Val](source: Rep[Size[Val]])
      extends Size[Val]
      with Def[Size[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[Size[Val]] = element[Size[Val]]
    override def transform(t: Transformer) = SizeAdapter[Val](t(source))

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySize[Val](p: Rep[Size[Val]]): Size[Val] = {
    if (p.rhs.isInstanceOf[Size[Val]@unchecked]) p.rhs.asInstanceOf[Size[Val]]
    else
      SizeAdapter(p)
  }

  // familyElem
  class SizeElem[Val, To <: Size[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSize[_], To](liftableSize(_eVal.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Size[Val]], classOf[SSize[_]], Set(
        "dataSize"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[Size[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Size[Val]] => convertSize(x) }
      tryConvert(element[Size[Val]], this, x, conv)
    }

    def convertSize(x: Rep[Size[Val]]): Rep[To] = {
      x.elem match {
        case _: SizeElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeElem[_, _], but got $e", x)
      }
    }
  }

  implicit def sizeElement[Val](implicit eVal: Elem[Val]): Elem[Size[Val]] =
    cachedElemByClass(eVal)(classOf[SizeElem[Val, Size[Val]]])

  implicit case object SizeCompanionElem extends CompanionElem[SizeCompanionCtor] {
  }

  abstract class SizeCompanionCtor extends CompanionDef[SizeCompanionCtor] with SizeCompanion {
    def selfType = SizeCompanionElem
    override def toString = "Size"
  }
  implicit def proxySizeCompanionCtor(p: Rep[SizeCompanionCtor]): SizeCompanionCtor =
    proxyOps[SizeCompanionCtor](p)

  lazy val RSize: Rep[SizeCompanionCtor] = new SizeCompanionCtor {
    private val thisClass = classOf[SizeCompanion]
  }

  object SizeMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[Size[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataSize" && receiver.elem.isInstanceOf[SizeElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Size[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Size[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeCompanionMethods {
  }
} // of object Size
  registerEntityObject("Size", Size)

object SizePrim extends EntityObject("SizePrim") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizePrim[Val] = special.collection.SizePrim[Val]
  case class SizePrimConst[SVal, Val](
        constValue: SSizePrim[SVal],
        lVal: Liftable[SVal, Val]
      ) extends SizePrim[Val] with LiftedConst[SSizePrim[SVal], SizePrim[Val]]
        with Def[SizePrim[Val]] with SizePrimConstMethods[Val] {
    implicit def eVal: Elem[Val] = lVal.eW

    val liftable: Liftable[SSizePrim[SVal], SizePrim[Val]] = liftableSizePrim(lVal)
    val selfType: Elem[SizePrim[Val]] = liftable.eW
  }

  trait SizePrimConstMethods[Val] extends SizePrim[Val] with SizeConstMethods[Val] { thisConst: Def[_] =>
    implicit def eVal: Elem[Val]
    private val SizePrimClass = classOf[SizePrim[Val]]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        SizePrimClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }

    override def tVal: Rep[WRType[Val]] = {
      asRep[WRType[Val]](mkMethodCall(self,
        SizePrimClass.getMethod("tVal"),
        List(),
        true, false, element[WRType[Val]]))
    }
  }

  case class LiftableSizePrim[SVal, Val](lVal: Liftable[SVal, Val])
    extends Liftable[SSizePrim[SVal], SizePrim[Val]] {
    lazy val eW: Elem[SizePrim[Val]] = sizePrimElement(lVal.eW)
    lazy val sourceType: RType[SSizePrim[SVal]] = {
            implicit val tagSVal = lVal.sourceType.asInstanceOf[RType[SVal]]
      RType[SSizePrim[SVal]]
    }
    def lift(x: SSizePrim[SVal]): Rep[SizePrim[Val]] = SizePrimConst(x, lVal)
    def unlift(w: Rep[SizePrim[Val]]): SSizePrim[SVal] = w match {
      case Def(SizePrimConst(x: SSizePrim[_], _lVal))
            if _lVal == lVal => x.asInstanceOf[SSizePrim[SVal]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSizePrim[SVal, Val](implicit lVal: Liftable[SVal,Val]): Liftable[SSizePrim[SVal], SizePrim[Val]] =
    LiftableSizePrim(lVal)

  private val SizePrimClass = classOf[SizePrim[_]]

  // entityAdapter for SizePrim trait
  case class SizePrimAdapter[Val](source: Rep[SizePrim[Val]])
      extends SizePrim[Val]
      with Def[SizePrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[SizePrim[Val]] = element[SizePrim[Val]]
    override def transform(t: Transformer) = SizePrimAdapter[Val](t(source))

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizePrimClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }

    def tVal: Rep[WRType[Val]] = {
      asRep[WRType[Val]](mkMethodCall(source,
        SizePrimClass.getMethod("tVal"),
        List(),
        true, true, element[WRType[Val]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizePrim[Val](p: Rep[SizePrim[Val]]): SizePrim[Val] = {
    if (p.rhs.isInstanceOf[SizePrim[Val]@unchecked]) p.rhs.asInstanceOf[SizePrim[Val]]
    else
      SizePrimAdapter(p)
  }

  // familyElem
  class SizePrimElem[Val, To <: SizePrim[Val]](implicit _eVal: Elem[Val])
    extends SizeElem[Val, To] {
    override def eVal = _eVal

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizePrim[_], To](liftableSizePrim(_eVal.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizePrim[Val]], classOf[SSizePrim[_]], Set(
        "dataSize", "tVal"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[SizePrim[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizePrim[Val]] => convertSizePrim(x) }
      tryConvert(element[SizePrim[Val]], this, x, conv)
    }

    def convertSizePrim(x: Rep[SizePrim[Val]]): Rep[To] = {
      x.elem match {
        case _: SizePrimElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizePrimElem[_, _], but got $e", x)
      }
    }
  }

  implicit def sizePrimElement[Val](implicit eVal: Elem[Val]): Elem[SizePrim[Val]] =
    cachedElemByClass(eVal)(classOf[SizePrimElem[Val, SizePrim[Val]]])

  implicit case object SizePrimCompanionElem extends CompanionElem[SizePrimCompanionCtor] {
  }

  abstract class SizePrimCompanionCtor extends CompanionDef[SizePrimCompanionCtor] with SizePrimCompanion {
    def selfType = SizePrimCompanionElem
    override def toString = "SizePrim"
  }
  implicit def proxySizePrimCompanionCtor(p: Rep[SizePrimCompanionCtor]): SizePrimCompanionCtor =
    proxyOps[SizePrimCompanionCtor](p)

  lazy val RSizePrim: Rep[SizePrimCompanionCtor] = new SizePrimCompanionCtor {
    private val thisClass = classOf[SizePrimCompanion]
  }

  object SizePrimMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[SizePrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "dataSize" && receiver.elem.isInstanceOf[SizePrimElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizePrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizePrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tVal {
      def unapply(d: Def[_]): Nullable[Rep[SizePrim[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "tVal" && receiver.elem.isInstanceOf[SizePrimElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizePrim[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizePrim[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizePrimCompanionMethods {
  }
} // of object SizePrim
  registerEntityObject("SizePrim", SizePrim)

object SizePair extends EntityObject("SizePair") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizePair[L, R] = special.collection.SizePair[L, R]
  case class SizePairConst[SL, SR, L, R](
        constValue: SSizePair[SL, SR],
        lL: Liftable[SL, L], lR: Liftable[SR, R]
      ) extends SizePair[L, R] with LiftedConst[SSizePair[SL, SR], SizePair[L, R]]
        with Def[SizePair[L, R]] with SizePairConstMethods[L, R] {
    implicit def eL: Elem[L] = lL.eW
    implicit def eR: Elem[R] = lR.eW
    implicit def eVal: Elem[(L, R)] = element[(L, R)]

    val liftable: Liftable[SSizePair[SL, SR], SizePair[L, R]] = liftableSizePair(lL,lR)
    val selfType: Elem[SizePair[L, R]] = liftable.eW
  }

  trait SizePairConstMethods[L, R] extends SizePair[L, R] with SizeConstMethods[(L, R)] { thisConst: Def[_] =>
    implicit def eL: Elem[L]
    implicit def eR: Elem[R]
    private val SizePairClass = classOf[SizePair[L, R]]

    override def l: Rep[Size[L]] = {
      asRep[Size[L]](mkMethodCall(self,
        SizePairClass.getMethod("l"),
        List(),
        true, false, element[Size[L]]))
    }

    override def r: Rep[Size[R]] = {
      asRep[Size[R]](mkMethodCall(self,
        SizePairClass.getMethod("r"),
        List(),
        true, false, element[Size[R]]))
    }
  }

  case class LiftableSizePair[SL, SR, L, R](lL: Liftable[SL, L],lR: Liftable[SR, R])
    extends Liftable[SSizePair[SL, SR], SizePair[L, R]] {
    lazy val eW: Elem[SizePair[L, R]] = sizePairElement(lL.eW,lR.eW)
    lazy val sourceType: RType[SSizePair[SL, SR]] = {
            implicit val tagSL = lL.sourceType.asInstanceOf[RType[SL]]
      implicit val tagSR = lR.sourceType.asInstanceOf[RType[SR]]
      RType[SSizePair[SL, SR]]
    }
    def lift(x: SSizePair[SL, SR]): Rep[SizePair[L, R]] = SizePairConst(x, lL,lR)
    def unlift(w: Rep[SizePair[L, R]]): SSizePair[SL, SR] = w match {
      case Def(SizePairConst(x: SSizePair[_,_], _lL,_lR))
            if _lL == lL && _lR == lR => x.asInstanceOf[SSizePair[SL, SR]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSizePair[SL, SR, L, R](implicit lL: Liftable[SL,L],lR: Liftable[SR,R]): Liftable[SSizePair[SL, SR], SizePair[L, R]] =
    LiftableSizePair(lL,lR)

  private val SizePairClass = classOf[SizePair[_, _]]

  // entityAdapter for SizePair trait
  case class SizePairAdapter[L, R](source: Rep[SizePair[L, R]])
      extends SizePair[L, R]
      with Def[SizePair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[SizePair[L, R]] = element[SizePair[L, R]]
    override def transform(t: Transformer) = SizePairAdapter[L, R](t(source))

    def l: Rep[Size[L]] = {
      asRep[Size[L]](mkMethodCall(source,
        SizePairClass.getMethod("l"),
        List(),
        true, true, element[Size[L]]))
    }

    def r: Rep[Size[R]] = {
      asRep[Size[R]](mkMethodCall(source,
        SizePairClass.getMethod("r"),
        List(),
        true, true, element[Size[R]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizePairClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizePair[L, R](p: Rep[SizePair[L, R]]): SizePair[L, R] = {
    if (p.rhs.isInstanceOf[SizePair[L, R]@unchecked]) p.rhs.asInstanceOf[SizePair[L, R]]
    else
      SizePairAdapter(p)
  }

  // familyElem
  class SizePairElem[L, R, To <: SizePair[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends SizeElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizePair[_,_], To](liftableSizePair(_eL.liftable, _eR.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizePair[L, R]], classOf[SSizePair[_,_]], Set(
        "l", "r"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[SizePair[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizePair[L, R]] => convertSizePair(x) }
      tryConvert(element[SizePair[L, R]], this, x, conv)
    }

    def convertSizePair(x: Rep[SizePair[L, R]]): Rep[To] = {
      x.elem match {
        case _: SizePairElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizePairElem[_, _, _], but got $e", x)
      }
    }
  }

  implicit def sizePairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[SizePair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[SizePairElem[L, R, SizePair[L, R]]])

  implicit case object SizePairCompanionElem extends CompanionElem[SizePairCompanionCtor] {
  }

  abstract class SizePairCompanionCtor extends CompanionDef[SizePairCompanionCtor] with SizePairCompanion {
    def selfType = SizePairCompanionElem
    override def toString = "SizePair"
  }
  implicit def proxySizePairCompanionCtor(p: Rep[SizePairCompanionCtor]): SizePairCompanionCtor =
    proxyOps[SizePairCompanionCtor](p)

  lazy val RSizePair: Rep[SizePairCompanionCtor] = new SizePairCompanionCtor {
    private val thisClass = classOf[SizePairCompanion]
  }

  object SizePairMethods {
    object l {
      def unapply(d: Def[_]): Nullable[Rep[SizePair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "l" && receiver.elem.isInstanceOf[SizePairElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizePair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizePair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object r {
      def unapply(d: Def[_]): Nullable[Rep[SizePair[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "r" && receiver.elem.isInstanceOf[SizePairElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizePair[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizePair[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizePairCompanionMethods {
  }
} // of object SizePair
  registerEntityObject("SizePair", SizePair)

object SizeColl extends EntityObject("SizeColl") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeColl[Item] = special.collection.SizeColl[Item]
  case class SizeCollConst[SItem, Item](
        constValue: SSizeColl[SItem],
        lItem: Liftable[SItem, Item]
      ) extends SizeColl[Item] with LiftedConst[SSizeColl[SItem], SizeColl[Item]]
        with Def[SizeColl[Item]] with SizeCollConstMethods[Item] {
    implicit def eItem: Elem[Item] = lItem.eW
    implicit def eVal: Elem[Coll[Item]] = element[Coll[Item]]

    val liftable: Liftable[SSizeColl[SItem], SizeColl[Item]] = liftableSizeColl(lItem)
    val selfType: Elem[SizeColl[Item]] = liftable.eW
  }

  trait SizeCollConstMethods[Item] extends SizeColl[Item] with SizeConstMethods[Coll[Item]] { thisConst: Def[_] =>
    implicit def eItem: Elem[Item]
    private val SizeCollClass = classOf[SizeColl[Item]]

    override def sizes: Rep[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(self,
        SizeCollClass.getMethod("sizes"),
        List(),
        true, false, element[Coll[Size[Item]]]))
    }
  }

  case class LiftableSizeColl[SItem, Item](lItem: Liftable[SItem, Item])
    extends Liftable[SSizeColl[SItem], SizeColl[Item]] {
    lazy val eW: Elem[SizeColl[Item]] = sizeCollElement(lItem.eW)
    lazy val sourceType: RType[SSizeColl[SItem]] = {
            implicit val tagSItem = lItem.sourceType.asInstanceOf[RType[SItem]]
      RType[SSizeColl[SItem]]
    }
    def lift(x: SSizeColl[SItem]): Rep[SizeColl[Item]] = SizeCollConst(x, lItem)
    def unlift(w: Rep[SizeColl[Item]]): SSizeColl[SItem] = w match {
      case Def(SizeCollConst(x: SSizeColl[_], _lItem))
            if _lItem == lItem => x.asInstanceOf[SSizeColl[SItem]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSizeColl[SItem, Item](implicit lItem: Liftable[SItem,Item]): Liftable[SSizeColl[SItem], SizeColl[Item]] =
    LiftableSizeColl(lItem)

  private val SizeCollClass = classOf[SizeColl[_]]

  // entityAdapter for SizeColl trait
  case class SizeCollAdapter[Item](source: Rep[SizeColl[Item]])
      extends SizeColl[Item]
      with Def[SizeColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val selfType: Elem[SizeColl[Item]] = element[SizeColl[Item]]
    override def transform(t: Transformer) = SizeCollAdapter[Item](t(source))

    def sizes: Rep[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        SizeCollClass.getMethod("sizes"),
        List(),
        true, true, element[Coll[Size[Item]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeCollClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeColl[Item](p: Rep[SizeColl[Item]]): SizeColl[Item] = {
    if (p.rhs.isInstanceOf[SizeColl[Item]@unchecked]) p.rhs.asInstanceOf[SizeColl[Item]]
    else
      SizeCollAdapter(p)
  }

  // familyElem
  class SizeCollElem[Item, To <: SizeColl[Item]](implicit _eItem: Elem[Item])
    extends SizeElem[Coll[Item], To] {
    def eItem = _eItem

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeColl[_], To](liftableSizeColl(_eItem.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeColl[Item]], classOf[SSizeColl[_]], Set(
        "sizes"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(collElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[SizeColl[Item]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeColl[Item]] => convertSizeColl(x) }
      tryConvert(element[SizeColl[Item]], this, x, conv)
    }

    def convertSizeColl(x: Rep[SizeColl[Item]]): Rep[To] = {
      x.elem match {
        case _: SizeCollElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeCollElem[_, _], but got $e", x)
      }
    }
  }

  implicit def sizeCollElement[Item](implicit eItem: Elem[Item]): Elem[SizeColl[Item]] =
    cachedElemByClass(eItem)(classOf[SizeCollElem[Item, SizeColl[Item]]])

  implicit case object SizeCollCompanionElem extends CompanionElem[SizeCollCompanionCtor] {
  }

  abstract class SizeCollCompanionCtor extends CompanionDef[SizeCollCompanionCtor] with SizeCollCompanion {
    def selfType = SizeCollCompanionElem
    override def toString = "SizeColl"
  }
  implicit def proxySizeCollCompanionCtor(p: Rep[SizeCollCompanionCtor]): SizeCollCompanionCtor =
    proxyOps[SizeCollCompanionCtor](p)

  lazy val RSizeColl: Rep[SizeCollCompanionCtor] = new SizeCollCompanionCtor {
    private val thisClass = classOf[SizeCollCompanion]
  }

  object SizeCollMethods {
    object sizes {
      def unapply(d: Def[_]): Nullable[Rep[SizeColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizes" && receiver.elem.isInstanceOf[SizeCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeColl[Item]] forSome {type Item}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeCollCompanionMethods {
  }
} // of object SizeColl
  registerEntityObject("SizeColl", SizeColl)

object SizeFunc extends EntityObject("SizeFunc") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeFunc[Env, Arg, Res] = special.collection.SizeFunc[Env, Arg, Res]
  case class SizeFuncConst[SEnv, SArg, SRes, Env, Arg, Res](
        constValue: SSizeFunc[SEnv, SArg, SRes],
        lEnv: Liftable[SEnv, Env], lArg: Liftable[SArg, Arg], lRes: Liftable[SRes, Res]
      ) extends SizeFunc[Env, Arg, Res] with LiftedConst[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]]
        with Def[SizeFunc[Env, Arg, Res]] with SizeFuncConstMethods[Env, Arg, Res] {
    implicit def eEnv: Elem[Env] = lEnv.eW
    implicit def eArg: Elem[Arg] = lArg.eW
    implicit def eRes: Elem[Res] = lRes.eW
    implicit def eVal: Elem[Arg => Res] = element[Arg => Res]

    val liftable: Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] = liftableSizeFunc(lEnv,lArg,lRes)
    val selfType: Elem[SizeFunc[Env, Arg, Res]] = liftable.eW
  }

  trait SizeFuncConstMethods[Env, Arg, Res] extends SizeFunc[Env, Arg, Res] with SizeConstMethods[Arg => Res] { thisConst: Def[_] =>
    implicit def eEnv: Elem[Env]
    implicit def eArg: Elem[Arg]
    implicit def eRes: Elem[Res]
    private val SizeFuncClass = classOf[SizeFunc[Env, Arg, Res]]

    override def sizeEnv: Rep[Size[Env]] = {
      asRep[Size[Env]](mkMethodCall(self,
        SizeFuncClass.getMethod("sizeEnv"),
        List(),
        true, false, element[Size[Env]]))
    }
  }

  case class LiftableSizeFunc[SEnv, SArg, SRes, Env, Arg, Res](lEnv: Liftable[SEnv, Env],lArg: Liftable[SArg, Arg],lRes: Liftable[SRes, Res])
    extends Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] {
    lazy val eW: Elem[SizeFunc[Env, Arg, Res]] = sizeFuncElement(lEnv.eW,lArg.eW,lRes.eW)
    lazy val sourceType: RType[SSizeFunc[SEnv, SArg, SRes]] = {
            implicit val tagSEnv = lEnv.sourceType.asInstanceOf[RType[SEnv]]
      implicit val tagSArg = lArg.sourceType.asInstanceOf[RType[SArg]]
      implicit val tagSRes = lRes.sourceType.asInstanceOf[RType[SRes]]
      RType[SSizeFunc[SEnv, SArg, SRes]]
    }
    def lift(x: SSizeFunc[SEnv, SArg, SRes]): Rep[SizeFunc[Env, Arg, Res]] = SizeFuncConst(x, lEnv,lArg,lRes)
    def unlift(w: Rep[SizeFunc[Env, Arg, Res]]): SSizeFunc[SEnv, SArg, SRes] = w match {
      case Def(SizeFuncConst(x: SSizeFunc[_,_,_], _lEnv,_lArg,_lRes))
            if _lEnv == lEnv && _lArg == lArg && _lRes == lRes => x.asInstanceOf[SSizeFunc[SEnv, SArg, SRes]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSizeFunc[SEnv, SArg, SRes, Env, Arg, Res](implicit lEnv: Liftable[SEnv,Env],lArg: Liftable[SArg,Arg],lRes: Liftable[SRes,Res]): Liftable[SSizeFunc[SEnv, SArg, SRes], SizeFunc[Env, Arg, Res]] =
    LiftableSizeFunc(lEnv,lArg,lRes)

  private val SizeFuncClass = classOf[SizeFunc[_, _, _]]

  // entityAdapter for SizeFunc trait
  case class SizeFuncAdapter[Env, Arg, Res](source: Rep[SizeFunc[Env, Arg, Res]])
      extends SizeFunc[Env, Arg, Res]
      with Def[SizeFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asElem[Env];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asElem[Arg];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val selfType: Elem[SizeFunc[Env, Arg, Res]] = element[SizeFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = SizeFuncAdapter[Env, Arg, Res](t(source))

    def sizeEnv: Rep[Size[Env]] = {
      asRep[Size[Env]](mkMethodCall(source,
        SizeFuncClass.getMethod("sizeEnv"),
        List(),
        true, true, element[Size[Env]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeFuncClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeFunc[Env, Arg, Res](p: Rep[SizeFunc[Env, Arg, Res]]): SizeFunc[Env, Arg, Res] = {
    if (p.rhs.isInstanceOf[SizeFunc[Env, Arg, Res]@unchecked]) p.rhs.asInstanceOf[SizeFunc[Env, Arg, Res]]
    else
      SizeFuncAdapter(p)
  }

  // familyElem
  class SizeFuncElem[Env, Arg, Res, To <: SizeFunc[Env, Arg, Res]](implicit _eEnv: Elem[Env], _eArg: Elem[Arg], _eRes: Elem[Res])
    extends SizeElem[Arg => Res, To] {
    def eEnv = _eEnv
    def eArg = _eArg
    def eRes = _eRes

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeFunc[_,_,_], To](liftableSizeFunc(_eEnv.liftable, _eArg.liftable, _eRes.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeFunc[Env, Arg, Res]], classOf[SSizeFunc[_,_,_]], Set(
        "sizeEnv"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagArg = eArg.tag
      implicit val tagRes = eRes.tag
      weakTypeTag[SizeFunc[Env, Arg, Res]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeFunc[Env, Arg, Res]] => convertSizeFunc(x) }
      tryConvert(element[SizeFunc[Env, Arg, Res]], this, x, conv)
    }

    def convertSizeFunc(x: Rep[SizeFunc[Env, Arg, Res]]): Rep[To] = {
      x.elem match {
        case _: SizeFuncElem[_, _, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeFuncElem[_, _, _, _], but got $e", x)
      }
    }
  }

  implicit def sizeFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[SizeFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[SizeFuncElem[Env, Arg, Res, SizeFunc[Env, Arg, Res]]])

  implicit case object SizeFuncCompanionElem extends CompanionElem[SizeFuncCompanionCtor] {
  }

  abstract class SizeFuncCompanionCtor extends CompanionDef[SizeFuncCompanionCtor] with SizeFuncCompanion {
    def selfType = SizeFuncCompanionElem
    override def toString = "SizeFunc"
  }
  implicit def proxySizeFuncCompanionCtor(p: Rep[SizeFuncCompanionCtor]): SizeFuncCompanionCtor =
    proxyOps[SizeFuncCompanionCtor](p)

  lazy val RSizeFunc: Rep[SizeFuncCompanionCtor] = new SizeFuncCompanionCtor {
    private val thisClass = classOf[SizeFuncCompanion]
  }

  object SizeFuncMethods {
    object sizeEnv {
      def unapply(d: Def[_]): Nullable[Rep[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizeEnv" && receiver.elem.isInstanceOf[SizeFuncElem[_, _, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeFunc[Env, Arg, Res]] forSome {type Env; type Arg; type Res}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeFuncCompanionMethods {
  }
} // of object SizeFunc
  registerEntityObject("SizeFunc", SizeFunc)

object SizeOption extends EntityObject("SizeOption") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSizeOption[T] = special.collection.SizeOption[T]
  case class SizeOptionConst[ST, T](
        constValue: SSizeOption[ST],
        lT: Liftable[ST, T]
      ) extends SizeOption[T] with LiftedConst[SSizeOption[ST], SizeOption[T]]
        with Def[SizeOption[T]] with SizeOptionConstMethods[T] {
    implicit def eT: Elem[T] = lT.eW
    implicit def eVal: Elem[WOption[T]] = element[WOption[T]]

    val liftable: Liftable[SSizeOption[ST], SizeOption[T]] = liftableSizeOption(lT)
    val selfType: Elem[SizeOption[T]] = liftable.eW
  }

  trait SizeOptionConstMethods[T] extends SizeOption[T] with SizeConstMethods[WOption[T]] { thisConst: Def[_] =>
    implicit def eT: Elem[T]
    private val SizeOptionClass = classOf[SizeOption[T]]

    override def sizeOpt: Rep[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(self,
        SizeOptionClass.getMethod("sizeOpt"),
        List(),
        true, false, element[WOption[Size[T]]]))
    }
  }

  case class LiftableSizeOption[ST, T](lT: Liftable[ST, T])
    extends Liftable[SSizeOption[ST], SizeOption[T]] {
    lazy val eW: Elem[SizeOption[T]] = sizeOptionElement(lT.eW)
    lazy val sourceType: RType[SSizeOption[ST]] = {
            implicit val tagST = lT.sourceType.asInstanceOf[RType[ST]]
      RType[SSizeOption[ST]]
    }
    def lift(x: SSizeOption[ST]): Rep[SizeOption[T]] = SizeOptionConst(x, lT)
    def unlift(w: Rep[SizeOption[T]]): SSizeOption[ST] = w match {
      case Def(SizeOptionConst(x: SSizeOption[_], _lT))
            if _lT == lT => x.asInstanceOf[SSizeOption[ST]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableSizeOption[ST, T](implicit lT: Liftable[ST,T]): Liftable[SSizeOption[ST], SizeOption[T]] =
    LiftableSizeOption(lT)

  private val SizeOptionClass = classOf[SizeOption[_]]

  // entityAdapter for SizeOption trait
  case class SizeOptionAdapter[T](source: Rep[SizeOption[T]])
      extends SizeOption[T]
      with Def[SizeOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val selfType: Elem[SizeOption[T]] = element[SizeOption[T]]
    override def transform(t: Transformer) = SizeOptionAdapter[T](t(source))

    def sizeOpt: Rep[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        SizeOptionClass.getMethod("sizeOpt"),
        List(),
        true, true, element[WOption[Size[T]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        SizeOptionClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySizeOption[T](p: Rep[SizeOption[T]]): SizeOption[T] = {
    if (p.rhs.isInstanceOf[SizeOption[T]@unchecked]) p.rhs.asInstanceOf[SizeOption[T]]
    else
      SizeOptionAdapter(p)
  }

  // familyElem
  class SizeOptionElem[T, To <: SizeOption[T]](implicit _eT: Elem[T])
    extends SizeElem[WOption[T], To] {
    def eT = _eT

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSizeOption[_], To](liftableSizeOption(_eT.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[SizeOption[T]], classOf[SSizeOption[_]], Set(
        "sizeOpt"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(sizeElement(wOptionElement(element[T])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[SizeOption[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SizeOption[T]] => convertSizeOption(x) }
      tryConvert(element[SizeOption[T]], this, x, conv)
    }

    def convertSizeOption(x: Rep[SizeOption[T]]): Rep[To] = {
      x.elem match {
        case _: SizeOptionElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have SizeOptionElem[_, _], but got $e", x)
      }
    }
  }

  implicit def sizeOptionElement[T](implicit eT: Elem[T]): Elem[SizeOption[T]] =
    cachedElemByClass(eT)(classOf[SizeOptionElem[T, SizeOption[T]]])

  implicit case object SizeOptionCompanionElem extends CompanionElem[SizeOptionCompanionCtor] {
  }

  abstract class SizeOptionCompanionCtor extends CompanionDef[SizeOptionCompanionCtor] with SizeOptionCompanion {
    def selfType = SizeOptionCompanionElem
    override def toString = "SizeOption"
  }
  implicit def proxySizeOptionCompanionCtor(p: Rep[SizeOptionCompanionCtor]): SizeOptionCompanionCtor =
    proxyOps[SizeOptionCompanionCtor](p)

  lazy val RSizeOption: Rep[SizeOptionCompanionCtor] = new SizeOptionCompanionCtor {
    private val thisClass = classOf[SizeOptionCompanion]
  }

  object SizeOptionMethods {
    object sizeOpt {
      def unapply(d: Def[_]): Nullable[Rep[SizeOption[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizeOpt" && receiver.elem.isInstanceOf[SizeOptionElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[SizeOption[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[SizeOption[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SizeOptionCompanionMethods {
  }
} // of object SizeOption
  registerEntityObject("SizeOption", SizeOption)

  registerModule(SizesModule)
}

object SizesModule extends scalan.ModuleInfo("special.collection", "Sizes")
}

trait SizesModule extends special.collection.impl.SizesDefs {self: Library =>}
