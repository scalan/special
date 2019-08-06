package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
  import scalan.util.MemoizedFunc // manual fix

  // Abs -----------------------------------
trait ConcreteSizesDefs extends scalan.Scalan with ConcreteSizes {
  self: Library =>
import IsoUR._
import Converter._
import Size._  // manual fix
import Coll._  // manual fix
import WOption._  // manual fix
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import CSizeColl._
import CSizeFunc._
import CSizeOption._
import CSizePair._
import CSizePrim._
import WRType._  // manual fix

object CSizePrim extends EntityObject("CSizePrim") {
  case class CSizePrimCtor[Val]
      (override val dataSize: Ref[Long], override val tVal: Ref[WRType[Val]])
    extends CSizePrim[Val](dataSize, tVal) with Def[CSizePrim[Val]] {
    implicit lazy val eVal = tVal.eA

    lazy val resultType = element[CSizePrim[Val]]
    override def transform(t: Transformer) = CSizePrimCtor[Val](t(dataSize), t(tVal))
  }
  // elem for concrete class
  class CSizePrimElem[Val](val iso: Iso[CSizePrimData[Val], CSizePrim[Val]])(implicit override val eVal: Elem[Val])
    extends SizePrimElem[Val, CSizePrim[Val]]
    with ConcreteElem[CSizePrimData[Val], CSizePrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(sizePrimElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  // state representation type
  type CSizePrimData[Val] = (Long, WRType[Val])

  // 3) Iso for concrete class
  class CSizePrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CSizePrimData[Val], CSizePrim[Val]] with Def[CSizePrimIso[Val]] {
    override def transform(t: Transformer) = new CSizePrimIso[Val]()(eVal)
    private lazy val _safeFrom = fun { p: Ref[CSizePrim[Val]] => (p.dataSize, p.tVal) }
    override def from(p: Ref[CSizePrim[Val]]) =
      tryConvert[CSizePrim[Val], (Long, WRType[Val])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Long, WRType[Val])]) = {
      val Pair(dataSize, tVal) = p
      RCSizePrim(dataSize, tVal)
    }
    lazy val eFrom = pairElement(element[Long], element[WRType[Val]])
    lazy val eTo = new CSizePrimElem[Val](self)
    lazy val resultType = new CSizePrimIsoElem[Val](eVal)
    def productArity = 1
    def productElement(n: Int) = eVal
  }
  case class CSizePrimIsoElem[Val](eVal: Elem[Val]) extends Elem[CSizePrimIso[Val]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CSizePrimCompanionCtor extends CompanionDef[CSizePrimCompanionCtor] with CSizePrimCompanion {
    def resultType = CSizePrimCompanionElem
    override def toString = "CSizePrimCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val](p: Ref[CSizePrimData[Val]]): Ref[CSizePrim[Val]] = {
      implicit val eVal = p._2.eA
      isoCSizePrim[Val].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val](dataSize: Ref[Long], tVal: Ref[WRType[Val]]): Ref[CSizePrim[Val]] =
      mkCSizePrim(dataSize, tVal)

    def unapply[Val](p: Ref[SizePrim[Val]]) = unmkCSizePrim(p)
  }
  lazy val CSizePrimRep: Ref[CSizePrimCompanionCtor] = new CSizePrimCompanionCtor
  lazy val RCSizePrim: CSizePrimCompanionCtor = proxyCSizePrimCompanion(CSizePrimRep)
  implicit def proxyCSizePrimCompanion(p: Ref[CSizePrimCompanionCtor]): CSizePrimCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizePrimCompanionCtor])
      p.rhs.asInstanceOf[CSizePrimCompanionCtor]
    else
      unrefDelegate[CSizePrimCompanionCtor](p)
  }

  implicit case object CSizePrimCompanionElem extends CompanionElem[CSizePrimCompanionCtor]

  implicit def proxyCSizePrim[Val](p: Ref[CSizePrim[Val]]): CSizePrim[Val] = {
    if (p.rhs.isInstanceOf[CSizePrim[Val]@unchecked])
      p.rhs.asInstanceOf[CSizePrim[Val]]
    else
      unrefDelegate[CSizePrim[Val]](p)
  }

  implicit class ExtendedCSizePrim[Val](p: Ref[CSizePrim[Val]]) {
    def toData: Ref[CSizePrimData[Val]] = {
      implicit val eVal = p.tVal.eA
      isoCSizePrim(eVal).from(p)
    }
  }

  // 5) implicit resolution of Iso
  // manual fix
  private[ConcreteSizesDefs] val _isoCSizePrimMemo = new MemoizedFunc({ case eVal: Elem[v] =>
    reifyObject(new CSizePrimIso[v]()(eVal))
  })
  implicit def isoCSizePrim[Val](implicit eVal: Elem[Val]): Iso[CSizePrimData[Val], CSizePrim[Val]] =
    _isoCSizePrimMemo(eVal).asInstanceOf[Iso[CSizePrimData[Val], CSizePrim[Val]]]

  def mkCSizePrim[Val]
    (dataSize: Ref[Long], tVal: Ref[WRType[Val]]): Ref[CSizePrim[Val]] = {
    new CSizePrimCtor[Val](dataSize, tVal)
  }
  def unmkCSizePrim[Val](p: Ref[SizePrim[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizePrimElem[Val] @unchecked =>
      Some((asRep[CSizePrim[Val]](p).dataSize, asRep[CSizePrim[Val]](p).tVal))
    case _ =>
      None
  }
} // of object CSizePrim
  registerEntityObject("CSizePrim", CSizePrim)

object CSizePair extends EntityObject("CSizePair") {
  case class CSizePairCtor[L, R]
      (override val l: Ref[Size[L]], override val r: Ref[Size[R]])
    extends CSizePair[L, R](l, r) with Def[CSizePair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val resultType = element[CSizePair[L, R]]
    override def transform(t: Transformer) = CSizePairCtor[L, R](t(l), t(r))
    private val thisClass = classOf[SizePair[_, _]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizePairElem[L, R](val iso: Iso[CSizePairData[L, R], CSizePair[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends SizePairElem[L, R, CSizePair[L, R]]
    with ConcreteElem[CSizePairData[L, R], CSizePair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(sizePairElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  // state representation type
  type CSizePairData[L, R] = (Size[L], Size[R])

  // 3) Iso for concrete class
  class CSizePairIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CSizePairData[L, R], CSizePair[L, R]] with Def[CSizePairIso[L, R]] {
    override def transform(t: Transformer) = new CSizePairIso[L, R]()(eL, eR)
    private lazy val _safeFrom = fun { p: Ref[CSizePair[L, R]] => (p.l, p.r) }
    override def from(p: Ref[CSizePair[L, R]]) =
      tryConvert[CSizePair[L, R], (Size[L], Size[R])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Size[L], Size[R])]) = {
      val Pair(l, r) = p
      RCSizePair(l, r)
    }
    lazy val eFrom = pairElement(element[Size[L]], element[Size[R]])
    lazy val eTo = new CSizePairElem[L, R](self)
    lazy val resultType = new CSizePairIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class CSizePairIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[CSizePairIso[L, R]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CSizePairCompanionCtor extends CompanionDef[CSizePairCompanionCtor] with CSizePairCompanion {
    def resultType = CSizePairCompanionElem
    override def toString = "CSizePairCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Ref[CSizePairData[L, R]]): Ref[CSizePair[L, R]] = {
      implicit val eL = p._1.eVal;
implicit val eR = p._2.eVal
      isoCSizePair[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[CSizePair[L, R]] =
      mkCSizePair(l, r)

    def unapply[L, R](p: Ref[SizePair[L, R]]) = unmkCSizePair(p)
  }
  lazy val CSizePairRep: Ref[CSizePairCompanionCtor] = new CSizePairCompanionCtor
  lazy val RCSizePair: CSizePairCompanionCtor = proxyCSizePairCompanion(CSizePairRep)
  implicit def proxyCSizePairCompanion(p: Ref[CSizePairCompanionCtor]): CSizePairCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizePairCompanionCtor])
      p.rhs.asInstanceOf[CSizePairCompanionCtor]
    else
      unrefDelegate[CSizePairCompanionCtor](p)
  }

  implicit case object CSizePairCompanionElem extends CompanionElem[CSizePairCompanionCtor]

  implicit def proxyCSizePair[L, R](p: Ref[CSizePair[L, R]]): CSizePair[L, R] = {
    if (p.rhs.isInstanceOf[CSizePair[L, R]@unchecked])
      p.rhs.asInstanceOf[CSizePair[L, R]]
    else
      unrefDelegate[CSizePair[L, R]](p)
  }

  implicit class ExtendedCSizePair[L, R](p: Ref[CSizePair[L, R]]) {
    def toData: Ref[CSizePairData[L, R]] = {
      implicit val eL = p.l.eVal;
implicit val eR = p.r.eVal
      isoCSizePair(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizePair[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CSizePairData[L, R], CSizePair[L, R]] =
    reifyObject(new CSizePairIso[L, R]()(eL, eR))

  def mkCSizePair[L, R]
    (l: Ref[Size[L]], r: Ref[Size[R]]): Ref[CSizePair[L, R]] = {
    new CSizePairCtor[L, R](l, r)
  }
  def unmkCSizePair[L, R](p: Ref[SizePair[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizePairElem[L, R] @unchecked =>
      Some((asRep[CSizePair[L, R]](p).l, asRep[CSizePair[L, R]](p).r))
    case _ =>
      None
  }
} // of object CSizePair
  registerEntityObject("CSizePair", CSizePair)

object CSizeColl extends EntityObject("CSizeColl") {
  case class CSizeCollCtor[Item]
      (override val sizes: Ref[Coll[Size[Item]]])
    extends CSizeColl[Item](sizes) with Def[CSizeColl[Item]] {
    implicit lazy val eItem = sizes.eA.typeArgs("Val")._1.asElem[Item]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    lazy val resultType = element[CSizeColl[Item]]
    override def transform(t: Transformer) = CSizeCollCtor[Item](t(sizes))
    private val thisClass = classOf[SizeColl[_]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeCollElem[Item](val iso: Iso[CSizeCollData[Item], CSizeColl[Item]])(implicit override val eItem: Elem[Item])
    extends SizeCollElem[Item, CSizeColl[Item]]
    with ConcreteElem[CSizeCollData[Item], CSizeColl[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeCollElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  // state representation type
  type CSizeCollData[Item] = Coll[Size[Item]]

  // 3) Iso for concrete class
  class CSizeCollIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CSizeCollData[Item], CSizeColl[Item]] with Def[CSizeCollIso[Item]] {
    override def transform(t: Transformer) = new CSizeCollIso[Item]()(eItem)
    private lazy val _safeFrom = fun { p: Ref[CSizeColl[Item]] => p.sizes }
    override def from(p: Ref[CSizeColl[Item]]) =
      tryConvert[CSizeColl[Item], Coll[Size[Item]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Coll[Size[Item]]]) = {
      val sizes = p
      RCSizeColl(sizes)
    }
    lazy val eFrom = element[Coll[Size[Item]]]
    lazy val eTo = new CSizeCollElem[Item](self)
    lazy val resultType = new CSizeCollIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CSizeCollIsoElem[Item](eItem: Elem[Item]) extends Elem[CSizeCollIso[Item]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CSizeCollCompanionCtor extends CompanionDef[CSizeCollCompanionCtor] with CSizeCollCompanion {
    def resultType = CSizeCollCompanionElem
    override def toString = "CSizeCollCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](sizes: Ref[Coll[Size[Item]]]): Ref[CSizeColl[Item]] =
      mkCSizeColl(sizes)

    def unapply[Item](p: Ref[SizeColl[Item]]) = unmkCSizeColl(p)
  }
  lazy val CSizeCollRep: Ref[CSizeCollCompanionCtor] = new CSizeCollCompanionCtor
  lazy val RCSizeColl: CSizeCollCompanionCtor = proxyCSizeCollCompanion(CSizeCollRep)
  implicit def proxyCSizeCollCompanion(p: Ref[CSizeCollCompanionCtor]): CSizeCollCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeCollCompanionCtor])
      p.rhs.asInstanceOf[CSizeCollCompanionCtor]
    else
      unrefDelegate[CSizeCollCompanionCtor](p)
  }

  implicit case object CSizeCollCompanionElem extends CompanionElem[CSizeCollCompanionCtor]

  implicit def proxyCSizeColl[Item](p: Ref[CSizeColl[Item]]): CSizeColl[Item] = {
    if (p.rhs.isInstanceOf[CSizeColl[Item]@unchecked])
      p.rhs.asInstanceOf[CSizeColl[Item]]
    else
      unrefDelegate[CSizeColl[Item]](p)
  }

  implicit class ExtendedCSizeColl[Item](p: Ref[CSizeColl[Item]]) {
    def toData: Ref[CSizeCollData[Item]] = {
      implicit val eItem = p.sizes.eA.typeArgs("Val")._1.asElem[Item]
      isoCSizeColl(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeColl[Item](implicit eItem: Elem[Item]): Iso[CSizeCollData[Item], CSizeColl[Item]] =
    reifyObject(new CSizeCollIso[Item]()(eItem))

  def mkCSizeColl[Item]
    (sizes: Ref[Coll[Size[Item]]]): Ref[CSizeColl[Item]] = {
    new CSizeCollCtor[Item](sizes)
  }
  def unmkCSizeColl[Item](p: Ref[SizeColl[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeCollElem[Item] @unchecked =>
      Some((asRep[CSizeColl[Item]](p).sizes))
    case _ =>
      None
  }
} // of object CSizeColl
  registerEntityObject("CSizeColl", CSizeColl)

object CSizeFunc extends EntityObject("CSizeFunc") {
  case class CSizeFuncCtor[Env, Arg, Res]
      (override val sizeEnv: Ref[Size[Env]], override val sizeFunc: Ref[Long], override val tArg: Ref[WRType[Arg]], override val tRes: Ref[WRType[Res]])
    extends CSizeFunc[Env, Arg, Res](sizeEnv, sizeFunc, tArg, tRes) with Def[CSizeFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = sizeEnv.eVal;
implicit lazy val eArg = tArg.eA;
implicit lazy val eRes = tRes.eA
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val resultType = element[CSizeFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CSizeFuncCtor[Env, Arg, Res](t(sizeEnv), t(sizeFunc), t(tArg), t(tRes))
    private val thisClass = classOf[SizeFunc[_, _, _]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeFuncElem[Env, Arg, Res](val iso: Iso[CSizeFuncData[Env, Arg, Res], CSizeFunc[Env, Arg, Res]])(implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends SizeFuncElem[Env, Arg, Res, CSizeFunc[Env, Arg, Res]]
    with ConcreteElem[CSizeFuncData[Env, Arg, Res], CSizeFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeFuncElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }

  // state representation type
  type CSizeFuncData[Env, Arg, Res] = (Size[Env], (Long, (WRType[Arg], WRType[Res])))

  // 3) Iso for concrete class
  class CSizeFuncIso[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res])
    extends EntityIso[CSizeFuncData[Env, Arg, Res], CSizeFunc[Env, Arg, Res]] with Def[CSizeFuncIso[Env, Arg, Res]] {
    override def transform(t: Transformer) = new CSizeFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes)
    private lazy val _safeFrom = fun { p: Ref[CSizeFunc[Env, Arg, Res]] => (p.sizeEnv, p.sizeFunc, p.tArg, p.tRes) }
    override def from(p: Ref[CSizeFunc[Env, Arg, Res]]) =
      tryConvert[CSizeFunc[Env, Arg, Res], (Size[Env], (Long, (WRType[Arg], WRType[Res])))](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Size[Env], (Long, (WRType[Arg], WRType[Res])))]) = {
      val Pair(sizeEnv, Pair(sizeFunc, Pair(tArg, tRes))) = p
      RCSizeFunc(sizeEnv, sizeFunc, tArg, tRes)
    }
    lazy val eFrom = pairElement(element[Size[Env]], pairElement(element[Long], pairElement(element[WRType[Arg]], element[WRType[Res]])))
    lazy val eTo = new CSizeFuncElem[Env, Arg, Res](self)
    lazy val resultType = new CSizeFuncIsoElem[Env, Arg, Res](eEnv, eArg, eRes)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eEnv
      case 1 => eArg
      case 2 => eRes
    }
  }
  case class CSizeFuncIsoElem[Env, Arg, Res](eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]) extends Elem[CSizeFuncIso[Env, Arg, Res]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CSizeFuncCompanionCtor extends CompanionDef[CSizeFuncCompanionCtor] with CSizeFuncCompanion {
    def resultType = CSizeFuncCompanionElem
    override def toString = "CSizeFuncCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Ref[CSizeFuncData[Env, Arg, Res]]): Ref[CSizeFunc[Env, Arg, Res]] = {
      implicit val eEnv = p._1.eVal;
implicit val eArg = p._3.eA;
implicit val eRes = p._4.eA
      isoCSizeFunc[Env, Arg, Res].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](sizeEnv: Ref[Size[Env]], sizeFunc: Ref[Long], tArg: Ref[WRType[Arg]], tRes: Ref[WRType[Res]]): Ref[CSizeFunc[Env, Arg, Res]] =
      mkCSizeFunc(sizeEnv, sizeFunc, tArg, tRes)

    def unapply[Env, Arg, Res](p: Ref[SizeFunc[Env, Arg, Res]]) = unmkCSizeFunc(p)
  }
  lazy val CSizeFuncRep: Ref[CSizeFuncCompanionCtor] = new CSizeFuncCompanionCtor
  lazy val RCSizeFunc: CSizeFuncCompanionCtor = proxyCSizeFuncCompanion(CSizeFuncRep)
  implicit def proxyCSizeFuncCompanion(p: Ref[CSizeFuncCompanionCtor]): CSizeFuncCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeFuncCompanionCtor])
      p.rhs.asInstanceOf[CSizeFuncCompanionCtor]
    else
      unrefDelegate[CSizeFuncCompanionCtor](p)
  }

  implicit case object CSizeFuncCompanionElem extends CompanionElem[CSizeFuncCompanionCtor]

  implicit def proxyCSizeFunc[Env, Arg, Res](p: Ref[CSizeFunc[Env, Arg, Res]]): CSizeFunc[Env, Arg, Res] = {
    if (p.rhs.isInstanceOf[CSizeFunc[Env, Arg, Res]@unchecked])
      p.rhs.asInstanceOf[CSizeFunc[Env, Arg, Res]]
    else
      unrefDelegate[CSizeFunc[Env, Arg, Res]](p)
  }

  implicit class ExtendedCSizeFunc[Env, Arg, Res](p: Ref[CSizeFunc[Env, Arg, Res]]) {
    def toData: Ref[CSizeFuncData[Env, Arg, Res]] = {
      implicit val eEnv = p.sizeEnv.eVal;
implicit val eArg = p.tArg.eA;
implicit val eRes = p.tRes.eA
      isoCSizeFunc(eEnv, eArg, eRes).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeFunc[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Iso[CSizeFuncData[Env, Arg, Res], CSizeFunc[Env, Arg, Res]] =
    reifyObject(new CSizeFuncIso[Env, Arg, Res]()(eEnv, eArg, eRes))

  def mkCSizeFunc[Env, Arg, Res]
    (sizeEnv: Ref[Size[Env]], sizeFunc: Ref[Long], tArg: Ref[WRType[Arg]], tRes: Ref[WRType[Res]]): Ref[CSizeFunc[Env, Arg, Res]] = {
    new CSizeFuncCtor[Env, Arg, Res](sizeEnv, sizeFunc, tArg, tRes)
  }
  def unmkCSizeFunc[Env, Arg, Res](p: Ref[SizeFunc[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeFuncElem[Env, Arg, Res] @unchecked =>
      Some((asRep[CSizeFunc[Env, Arg, Res]](p).sizeEnv, asRep[CSizeFunc[Env, Arg, Res]](p).sizeFunc, asRep[CSizeFunc[Env, Arg, Res]](p).tArg, asRep[CSizeFunc[Env, Arg, Res]](p).tRes))
    case _ =>
      None
  }
} // of object CSizeFunc
  registerEntityObject("CSizeFunc", CSizeFunc)

object CSizeOption extends EntityObject("CSizeOption") {
  case class CSizeOptionCtor[Item]
      (override val sizeOpt: Ref[WOption[Size[Item]]])
    extends CSizeOption[Item](sizeOpt) with Def[CSizeOption[Item]] {
    implicit lazy val eItem = sizeOpt.eA.typeArgs("Val")._1.asElem[Item]
    override lazy val eT: Elem[Item] = eItem
override lazy val eVal: Elem[WOption[Item]] = implicitly[Elem[WOption[Item]]]
    lazy val resultType = element[CSizeOption[Item]]
    override def transform(t: Transformer) = CSizeOptionCtor[Item](t(sizeOpt))
    private val thisClass = classOf[SizeOption[_]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeOptionElem[Item](val iso: Iso[CSizeOptionData[Item], CSizeOption[Item]])(implicit val eItem: Elem[Item])
    extends SizeOptionElem[Item, CSizeOption[Item]]
    with ConcreteElem[CSizeOptionData[Item], CSizeOption[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeOptionElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  // state representation type
  type CSizeOptionData[Item] = WOption[Size[Item]]

  // 3) Iso for concrete class
  class CSizeOptionIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CSizeOptionData[Item], CSizeOption[Item]] with Def[CSizeOptionIso[Item]] {
    override def transform(t: Transformer) = new CSizeOptionIso[Item]()(eItem)
    private lazy val _safeFrom = fun { p: Ref[CSizeOption[Item]] => p.sizeOpt }
    override def from(p: Ref[CSizeOption[Item]]) =
      tryConvert[CSizeOption[Item], WOption[Size[Item]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[WOption[Size[Item]]]) = {
      val sizeOpt = p
      RCSizeOption(sizeOpt)
    }
    lazy val eFrom = element[WOption[Size[Item]]]
    lazy val eTo = new CSizeOptionElem[Item](self)
    lazy val resultType = new CSizeOptionIsoElem[Item](eItem)
    def productArity = 1
    def productElement(n: Int) = eItem
  }
  case class CSizeOptionIsoElem[Item](eItem: Elem[Item]) extends Elem[CSizeOptionIso[Item]] {
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CSizeOptionCompanionCtor extends CompanionDef[CSizeOptionCompanionCtor] with CSizeOptionCompanion {
    def resultType = CSizeOptionCompanionElem
    override def toString = "CSizeOptionCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](sizeOpt: Ref[WOption[Size[Item]]]): Ref[CSizeOption[Item]] =
      mkCSizeOption(sizeOpt)

    def unapply[Item](p: Ref[SizeOption[Item]]) = unmkCSizeOption(p)
  }
  lazy val CSizeOptionRep: Ref[CSizeOptionCompanionCtor] = new CSizeOptionCompanionCtor
  lazy val RCSizeOption: CSizeOptionCompanionCtor = proxyCSizeOptionCompanion(CSizeOptionRep)
  implicit def proxyCSizeOptionCompanion(p: Ref[CSizeOptionCompanionCtor]): CSizeOptionCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeOptionCompanionCtor])
      p.rhs.asInstanceOf[CSizeOptionCompanionCtor]
    else
      unrefDelegate[CSizeOptionCompanionCtor](p)
  }

  implicit case object CSizeOptionCompanionElem extends CompanionElem[CSizeOptionCompanionCtor]

  implicit def proxyCSizeOption[Item](p: Ref[CSizeOption[Item]]): CSizeOption[Item] = {
    if (p.rhs.isInstanceOf[CSizeOption[Item]@unchecked])
      p.rhs.asInstanceOf[CSizeOption[Item]]
    else
      unrefDelegate[CSizeOption[Item]](p)
  }

  implicit class ExtendedCSizeOption[Item](p: Ref[CSizeOption[Item]]) {
    def toData: Ref[CSizeOptionData[Item]] = {
      implicit val eItem = p.sizeOpt.eA.typeArgs("Val")._1.asElem[Item]
      isoCSizeOption(eItem).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeOption[Item](implicit eItem: Elem[Item]): Iso[CSizeOptionData[Item], CSizeOption[Item]] =
    reifyObject(new CSizeOptionIso[Item]()(eItem))

  def mkCSizeOption[Item]
    (sizeOpt: Ref[WOption[Size[Item]]]): Ref[CSizeOption[Item]] = {
    new CSizeOptionCtor[Item](sizeOpt)
  }
  def unmkCSizeOption[Item](p: Ref[SizeOption[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeOptionElem[Item] @unchecked =>
      Some((asRep[CSizeOption[Item]](p).sizeOpt))
    case _ =>
      None
  }
} // of object CSizeOption
  registerEntityObject("CSizeOption", CSizeOption)

  registerModule(ConcreteSizesModule)

  // manual fix
  override protected def onReset(): Unit = {
    super.onReset()
    CSizePrim._isoCSizePrimMemo.reset()
  }
}

object ConcreteSizesModule extends scalan.ModuleInfo("special.collection", "ConcreteSizes")
}

trait ConcreteSizesModule extends special.collection.impl.ConcreteSizesDefs {self: Library =>}
