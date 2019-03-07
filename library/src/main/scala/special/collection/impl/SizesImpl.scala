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
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._

object Size extends EntityObject("Size") {
  // entityAdapter for Size trait
  case class SizeAdapter[Val](source: Rep[Size[Val]])
      extends Size[Val] with Def[Size[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[Size[Val]] = element[Size[Val]]
    override def transform(t: Transformer) = SizeAdapter[Val](t(source))
    private val thisClass = classOf[Size[Val]]

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
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

    lazy val parent: Option[Elem[_]] = None
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizeElement[Val](implicit eVal: Elem[Val]): Elem[Size[Val]] =
    cachedElem[SizeElem[Val, Size[Val]]](eVal)

  implicit case object SizeCompanionElem extends CompanionElem[SizeCompanionCtor] {
    lazy val tag = weakTypeTag[SizeCompanionCtor]
    protected def getDefaultRep = RSize
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeElem[_, _]] && method.getName == "dataSize" =>
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
  // entityAdapter for SizePrim trait
  case class SizePrimAdapter[Val](source: Rep[SizePrim[Val]])
      extends SizePrim[Val] with Def[SizePrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asElem[Val]

    val selfType: Elem[SizePrim[Val]] = element[SizePrim[Val]]
    override def transform(t: Transformer) = SizePrimAdapter[Val](t(source))
    private val thisClass = classOf[SizePrim[Val]]

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
        List(),
        true, true, element[Long]))
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizePrimElement[Val](implicit eVal: Elem[Val]): Elem[SizePrim[Val]] =
    cachedElem[SizePrimElem[Val, SizePrim[Val]]](eVal)

  implicit case object SizePrimCompanionElem extends CompanionElem[SizePrimCompanionCtor] {
    lazy val tag = weakTypeTag[SizePrimCompanionCtor]
    protected def getDefaultRep = RSizePrim
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizePrimElem[_, _]] && method.getName == "dataSize" =>
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
  // entityAdapter for SizePair trait
  case class SizePairAdapter[L, R](source: Rep[SizePair[L, R]])
      extends SizePair[L, R] with Def[SizePair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[SizePair[L, R]] = element[SizePair[L, R]]
    override def transform(t: Transformer) = SizePairAdapter[L, R](t(source))
    private val thisClass = classOf[SizePair[L, R]]

    def l: Rep[Size[L]] = {
      asRep[Size[L]](mkMethodCall(source,
        thisClass.getMethod("l"),
        List(),
        true, true, element[Size[L]]))
    }

    def r: Rep[Size[R]] = {
      asRep[Size[R]](mkMethodCall(source,
        thisClass.getMethod("r"),
        List(),
        true, true, element[Size[R]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizePairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[SizePair[L, R]] =
    cachedElem[SizePairElem[L, R, SizePair[L, R]]](eL, eR)

  implicit case object SizePairCompanionElem extends CompanionElem[SizePairCompanionCtor] {
    lazy val tag = weakTypeTag[SizePairCompanionCtor]
    protected def getDefaultRep = RSizePair
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizePairElem[_, _, _]] && method.getName == "l" =>
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizePairElem[_, _, _]] && method.getName == "r" =>
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
  // entityAdapter for SizeColl trait
  case class SizeCollAdapter[Item](source: Rep[SizeColl[Item]])
      extends SizeColl[Item] with Def[SizeColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asElem[Item]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val selfType: Elem[SizeColl[Item]] = element[SizeColl[Item]]
    override def transform(t: Transformer) = SizeCollAdapter[Item](t(source))
    private val thisClass = classOf[SizeColl[Item]]

    def sizes: Rep[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        thisClass.getMethod("sizes"),
        List(),
        true, true, element[Coll[Size[Item]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizeCollElement[Item](implicit eItem: Elem[Item]): Elem[SizeColl[Item]] =
    cachedElem[SizeCollElem[Item, SizeColl[Item]]](eItem)

  implicit case object SizeCollCompanionElem extends CompanionElem[SizeCollCompanionCtor] {
    lazy val tag = weakTypeTag[SizeCollCompanionCtor]
    protected def getDefaultRep = RSizeColl
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeCollElem[_, _]] && method.getName == "sizes" =>
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
  // entityAdapter for SizeFunc trait
  case class SizeFuncAdapter[Env, Arg, Res](source: Rep[SizeFunc[Env, Arg, Res]])
      extends SizeFunc[Env, Arg, Res] with Def[SizeFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asElem[Env];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asElem[Arg];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asElem[Res]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val selfType: Elem[SizeFunc[Env, Arg, Res]] = element[SizeFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = SizeFuncAdapter[Env, Arg, Res](t(source))
    private val thisClass = classOf[SizeFunc[Env, Arg, Res]]

    def sizeEnv: Rep[Size[Env]] = {
      asRep[Size[Env]](mkMethodCall(source,
        thisClass.getMethod("sizeEnv"),
        List(),
        true, true, element[Size[Env]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizeFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[SizeFunc[Env, Arg, Res]] =
    cachedElem[SizeFuncElem[Env, Arg, Res, SizeFunc[Env, Arg, Res]]](eEnv, eArg, eRes)

  implicit case object SizeFuncCompanionElem extends CompanionElem[SizeFuncCompanionCtor] {
    lazy val tag = weakTypeTag[SizeFuncCompanionCtor]
    protected def getDefaultRep = RSizeFunc
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeFuncElem[_, _, _, _]] && method.getName == "sizeEnv" =>
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
  // entityAdapter for SizeOption trait
  case class SizeOptionAdapter[T](source: Rep[SizeOption[T]])
      extends SizeOption[T] with Def[SizeOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val selfType: Elem[SizeOption[T]] = element[SizeOption[T]]
    override def transform(t: Transformer) = SizeOptionAdapter[T](t(source))
    private val thisClass = classOf[SizeOption[T]]

    def sizeOpt: Rep[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        thisClass.getMethod("sizeOpt"),
        List(),
        true, true, element[WOption[Size[T]]]))
    }

    def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(source,
        thisClass.getMethod("dataSize"),
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
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sizeOptionElement[T](implicit eT: Elem[T]): Elem[SizeOption[T]] =
    cachedElem[SizeOptionElem[T, SizeOption[T]]](eT)

  implicit case object SizeOptionCompanionElem extends CompanionElem[SizeOptionCompanionCtor] {
    lazy val tag = weakTypeTag[SizeOptionCompanionCtor]
    protected def getDefaultRep = RSizeOption
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
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SizeOptionElem[_, _]] && method.getName == "sizeOpt" =>
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
