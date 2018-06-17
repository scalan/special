package scalan.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ConcreteCostsDefs extends scalan.Scalan with ConcreteCosts {
  self: Library =>

  case class CostedPrimCtor[Val]
      (override val value: Rep[Val], override val cost: Rep[Long])
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
    override def convertCosted(x: Rep[Costed[Val]]) = CostedPrim(x.value, x.cost)
    override def getDefaultRep = CostedPrim(element[Val].defaultRepValue, 0l)
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[CostedPrim[Val]]
    }
  }

  // state representation type
  type CostedPrimData[Val] = (Val, Long)

  // 3) Iso for concrete class
  class CostedPrimIso[Val](implicit eVal: Elem[Val])
    extends EntityIso[CostedPrimData[Val], CostedPrim[Val]] with Def[CostedPrimIso[Val]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPrim[Val]] => (p.value, p.cost) }
    override def from(p: Rep[CostedPrim[Val]]) =
      tryConvert[CostedPrim[Val], (Val, Long)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Val, Long)]) = {
      val Pair(value, cost) = p
      CostedPrim(value, cost)
    }
    lazy val eFrom = pairElement(element[Val], element[Long])
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
    def apply[Val](value: Rep[Val], cost: Rep[Long]): Rep[CostedPrim[Val]] =
      mkCostedPrim(value, cost)

    def unapply[Val](p: Rep[Costed[Val]]) = unmkCostedPrim(p)
  }
  lazy val CostedPrimRep: Rep[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor
  lazy val CostedPrim: CostedPrimCompanionCtor = proxyCostedPrimCompanion(CostedPrimRep)
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

  case class CostedPairCtor[L, R]
      (override val l: Rep[L], override val r: Rep[R], override val cost: Rep[Long])
    extends CostedPair[L, R](l, r, cost) with Def[CostedPair[L, R]] {
    implicit lazy val eL = l.elem;
implicit lazy val eR = r.elem
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
    override def getDefaultRep = CostedPair(element[L].defaultRepValue, element[R].defaultRepValue, 0l)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[CostedPair[L, R]]
    }
  }

  // state representation type
  type CostedPairData[L, R] = (L, (R, Long))

  // 3) Iso for concrete class
  class CostedPairIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[CostedPairData[L, R], CostedPair[L, R]] with Def[CostedPairIso[L, R]] {
    private lazy val _safeFrom = fun { p: Rep[CostedPair[L, R]] => (p.l, p.r, p.cost) }
    override def from(p: Rep[CostedPair[L, R]]) =
      tryConvert[CostedPair[L, R], (L, (R, Long))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(L, (R, Long))]) = {
      val Pair(l, Pair(r, cost)) = p
      CostedPair(l, r, cost)
    }
    lazy val eFrom = pairElement(element[L], pairElement(element[R], element[Long]))
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
      implicit val eL = p._1.elem;
implicit val eR = p._2.elem
      isoCostedPair[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Rep[L], r: Rep[R], cost: Rep[Long]): Rep[CostedPair[L, R]] =
      mkCostedPair(l, r, cost)

    def unapply[L, R](p: Rep[Costed[(L, R)]]) = unmkCostedPair(p)
  }
  lazy val CostedPairRep: Rep[CostedPairCompanionCtor] = new CostedPairCompanionCtor
  lazy val CostedPair: CostedPairCompanionCtor = proxyCostedPairCompanion(CostedPairRep)
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
      implicit val eL = p.l.elem;
implicit val eR = p.r.elem
      isoCostedPair(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCostedPair[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[CostedPairData[L, R], CostedPair[L, R]] =
    reifyObject(new CostedPairIso[L, R]()(eL, eR))

  case class CostedArrayCtor[Item]
      (override val values: Rep[Col[Item]], override val costs: Rep[Col[Long]])
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
    override def getDefaultRep = CostedArray(element[Col[Item]].defaultRepValue, element[Col[Long]].defaultRepValue)
    override lazy val tag = {
      implicit val tagItem = eItem.tag
      weakTypeTag[CostedArray[Item]]
    }
  }

  // state representation type
  type CostedArrayData[Item] = (Col[Item], Col[Long])

  // 3) Iso for concrete class
  class CostedArrayIso[Item](implicit eItem: Elem[Item])
    extends EntityIso[CostedArrayData[Item], CostedArray[Item]] with Def[CostedArrayIso[Item]] {
    private lazy val _safeFrom = fun { p: Rep[CostedArray[Item]] => (p.values, p.costs) }
    override def from(p: Rep[CostedArray[Item]]) =
      tryConvert[CostedArray[Item], (Col[Item], Col[Long])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[Item], Col[Long])]) = {
      val Pair(values, costs) = p
      CostedArray(values, costs)
    }
    lazy val eFrom = pairElement(element[Col[Item]], element[Col[Long]])
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
    def apply[Item](values: Rep[Col[Item]], costs: Rep[Col[Long]]): Rep[CostedArray[Item]] =
      mkCostedArray(values, costs)

    def unapply[Item](p: Rep[Costed[WArray[Item]]]) = unmkCostedArray(p)
  }
  lazy val CostedArrayRep: Rep[CostedArrayCompanionCtor] = new CostedArrayCompanionCtor
  lazy val CostedArray: CostedArrayCompanionCtor = proxyCostedArrayCompanion(CostedArrayRep)
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
    override def getDefaultRep = CostedPairArray(element[Costed[WArray[L]]].defaultRepValue, element[Costed[WArray[R]]].defaultRepValue)
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
      CostedPairArray(ls, rs)
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
  lazy val CostedPairArray: CostedPairArrayCompanionCtor = proxyCostedPairArrayCompanion(CostedPairArrayRep)
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
    override def getDefaultRep = CostedNestedArray(element[Col[Costed[WArray[Item]]]].defaultRepValue)
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
      CostedNestedArray(rows)
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
  lazy val CostedNestedArray: CostedNestedArrayCompanionCtor = proxyCostedNestedArrayCompanion(CostedNestedArrayRep)
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
    override def convertCostedBuilder(x: Rep[CostedBuilder]) = ConcreteCostedBuilder()
    override def getDefaultRep = ConcreteCostedBuilder()
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
      ConcreteCostedBuilder()
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
  lazy val ConcreteCostedBuilder: ConcreteCostedBuilderCompanionCtor = proxyConcreteCostedBuilderCompanion(ConcreteCostedBuilderRep)
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

  registerModule(ConcreteCostsModule)

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

  def mkCostedPrim[Val]
    (value: Rep[Val], cost: Rep[Long]): Rep[CostedPrim[Val]] = {
    new CostedPrimCtor[Val](value, cost)
  }
  def unmkCostedPrim[Val](p: Rep[Costed[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPrimElem[Val] @unchecked =>
      Some((p.asRep[CostedPrim[Val]].value, p.asRep[CostedPrim[Val]].cost))
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
  }

  object CostedPairCompanionMethods {
  }

  def mkCostedPair[L, R]
    (l: Rep[L], r: Rep[R], cost: Rep[Long]): Rep[CostedPair[L, R]] = {
    new CostedPairCtor[L, R](l, r, cost)
  }
  def unmkCostedPair[L, R](p: Rep[Costed[(L, R)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedPairElem[L, R] @unchecked =>
      Some((p.asRep[CostedPair[L, R]].l, p.asRep[CostedPair[L, R]].r, p.asRep[CostedPair[L, R]].cost))
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

  def mkCostedArray[Item]
    (values: Rep[Col[Item]], costs: Rep[Col[Long]]): Rep[CostedArray[Item]] = {
    new CostedArrayCtor[Item](values, costs)
  }
  def unmkCostedArray[Item](p: Rep[Costed[WArray[Item]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CostedArrayElem[Item] @unchecked =>
      Some((p.asRep[CostedArray[Item]].values, p.asRep[CostedArray[Item]].costs))
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
}

object ConcreteCostsModule extends scalan.ModuleInfo("scalan.collection", "ConcreteCosts")
}

trait ConcreteCostsModule extends scalan.collection.impl.ConcreteCostsDefs {self: Library =>}
