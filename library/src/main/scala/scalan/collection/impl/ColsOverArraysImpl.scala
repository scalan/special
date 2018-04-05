package scalan.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ColsOverArraysDefs extends scalan.Scalan with ColsOverArrays {
  self: Library =>

  case class ColOverArrayCtor[A]
      (override val arr: Rep[WArray[A]])
    extends ColOverArray[A](arr) with Def[ColOverArray[A]] {
    implicit lazy val eA = arr.eT

    lazy val selfType = element[ColOverArray[A]]
  }
  // elem for concrete class
  class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A])
    extends ColElem[A, ColOverArray[A]]
    with ConcreteElem[ColOverArrayData[A], ColOverArray[A]] {
    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertCol(x: Rep[Col[A]]) = ColOverArray(x.arr)
    override def getDefaultRep = ColOverArray(element[WArray[A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ColOverArray[A]]
    }
  }

  // state representation type
  type ColOverArrayData[A] = WArray[A]

  // 3) Iso for concrete class
  class ColOverArrayIso[A](implicit eA: Elem[A])
    extends EntityIso[ColOverArrayData[A], ColOverArray[A]] with Def[ColOverArrayIso[A]] {
    override def from(p: Rep[ColOverArray[A]]) =
      p.arr
    override def to(p: Rep[WArray[A]]) = {
      val arr = p
      ColOverArray(arr)
    }
    lazy val eFrom = element[WArray[A]]
    lazy val eTo = new ColOverArrayElem[A](self)
    lazy val selfType = new ColOverArrayIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class ColOverArrayIsoElem[A](eA: Elem[A]) extends Elem[ColOverArrayIso[A]] {
    def getDefaultRep = reifyObject(new ColOverArrayIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ColOverArrayIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ColOverArrayCompanionCtor extends CompanionDef[ColOverArrayCompanionCtor] with ColOverArrayCompanion {
    def selfType = ColOverArrayCompanionElem
    override def toString = "ColOverArrayCompanion"

    @scalan.OverloadId("fromFields")
    def apply[A](arr: Rep[WArray[A]]): Rep[ColOverArray[A]] =
      mkColOverArray(arr)

    def unapply[A](p: Rep[Col[A]]) = unmkColOverArray(p)
  }
  lazy val ColOverArrayRep: Rep[ColOverArrayCompanionCtor] = new ColOverArrayCompanionCtor
  lazy val ColOverArray: ColOverArrayCompanionCtor = proxyColOverArrayCompanion(ColOverArrayRep)
  implicit def proxyColOverArrayCompanion(p: Rep[ColOverArrayCompanionCtor]): ColOverArrayCompanionCtor = {
    proxyOps[ColOverArrayCompanionCtor](p)
  }

  implicit case object ColOverArrayCompanionElem extends CompanionElem[ColOverArrayCompanionCtor] {
    lazy val tag = weakTypeTag[ColOverArrayCompanionCtor]
    protected def getDefaultRep = ColOverArrayRep
  }

  implicit def proxyColOverArray[A](p: Rep[ColOverArray[A]]): ColOverArray[A] =
    proxyOps[ColOverArray[A]](p)

  implicit class ExtendedColOverArray[A](p: Rep[ColOverArray[A]]) {
    def toData: Rep[ColOverArrayData[A]] = {
      implicit val eA = p.arr.eT
      isoColOverArray(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoColOverArray[A](implicit eA: Elem[A]): Iso[ColOverArrayData[A], ColOverArray[A]] =
    reifyObject(new ColOverArrayIso[A]()(eA))

  case class PairOfColsCtor[L, R]
      (override val ls: Rep[Col[L]], override val rs: Rep[Col[R]])
    extends PairOfCols[L, R](ls, rs) with Def[PairOfCols[L, R]] {
    implicit lazy val eL = ls.eA;
implicit lazy val eR = rs.eA
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[PairOfCols[L, R]]
  }
  // elem for concrete class
  class PairOfColsElem[L, R](val iso: Iso[PairOfColsData[L, R], PairOfCols[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends PairColElem[L, R, PairOfCols[L, R]]
    with ConcreteElem[PairOfColsData[L, R], PairOfCols[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(pairColElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertPairCol(x: Rep[PairCol[L, R]]) = PairOfCols(x.ls, x.rs)
    override def getDefaultRep = PairOfCols(element[Col[L]].defaultRepValue, element[Col[R]].defaultRepValue)
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairOfCols[L, R]]
    }
  }

  // state representation type
  type PairOfColsData[L, R] = (Col[L], Col[R])

  // 3) Iso for concrete class
  class PairOfColsIso[L, R](implicit eL: Elem[L], eR: Elem[R])
    extends EntityIso[PairOfColsData[L, R], PairOfCols[L, R]] with Def[PairOfColsIso[L, R]] {
    override def from(p: Rep[PairOfCols[L, R]]) =
      (p.ls, p.rs)
    override def to(p: Rep[(Col[L], Col[R])]) = {
      val Pair(ls, rs) = p
      PairOfCols(ls, rs)
    }
    lazy val eFrom = pairElement(element[Col[L]], element[Col[R]])
    lazy val eTo = new PairOfColsElem[L, R](self)
    lazy val selfType = new PairOfColsIsoElem[L, R](eL, eR)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eL
      case 1 => eR
    }
  }
  case class PairOfColsIsoElem[L, R](eL: Elem[L], eR: Elem[R]) extends Elem[PairOfColsIso[L, R]] {
    def getDefaultRep = reifyObject(new PairOfColsIso[L, R]()(eL, eR))
    lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairOfColsIso[L, R]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class PairOfColsCompanionCtor extends CompanionDef[PairOfColsCompanionCtor] with PairOfColsCompanion {
    def selfType = PairOfColsCompanionElem
    override def toString = "PairOfColsCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Rep[PairOfColsData[L, R]]): Rep[PairOfCols[L, R]] = {
      implicit val eL = p._1.eA;
implicit val eR = p._2.eA
      isoPairOfCols[L, R].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](ls: Rep[Col[L]], rs: Rep[Col[R]]): Rep[PairOfCols[L, R]] =
      mkPairOfCols(ls, rs)

    def unapply[L, R](p: Rep[PairCol[L, R]]) = unmkPairOfCols(p)
  }
  lazy val PairOfColsRep: Rep[PairOfColsCompanionCtor] = new PairOfColsCompanionCtor
  lazy val PairOfCols: PairOfColsCompanionCtor = proxyPairOfColsCompanion(PairOfColsRep)
  implicit def proxyPairOfColsCompanion(p: Rep[PairOfColsCompanionCtor]): PairOfColsCompanionCtor = {
    proxyOps[PairOfColsCompanionCtor](p)
  }

  implicit case object PairOfColsCompanionElem extends CompanionElem[PairOfColsCompanionCtor] {
    lazy val tag = weakTypeTag[PairOfColsCompanionCtor]
    protected def getDefaultRep = PairOfColsRep
  }

  implicit def proxyPairOfCols[L, R](p: Rep[PairOfCols[L, R]]): PairOfCols[L, R] =
    proxyOps[PairOfCols[L, R]](p)

  implicit class ExtendedPairOfCols[L, R](p: Rep[PairOfCols[L, R]]) {
    def toData: Rep[PairOfColsData[L, R]] = {
      implicit val eL = p.ls.eA;
implicit val eR = p.rs.eA
      isoPairOfCols(eL, eR).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoPairOfCols[L, R](implicit eL: Elem[L], eR: Elem[R]): Iso[PairOfColsData[L, R], PairOfCols[L, R]] =
    reifyObject(new PairOfColsIso[L, R]()(eL, eR))

  case class ColOverArrayBuilderCtor
      ()
    extends ColOverArrayBuilder() with Def[ColOverArrayBuilder] {
    lazy val selfType = element[ColOverArrayBuilder]
  }
  // elem for concrete class
  class ColOverArrayBuilderElem(val iso: Iso[ColOverArrayBuilderData, ColOverArrayBuilder])
    extends ColBuilderElem[ColOverArrayBuilder]
    with ConcreteElem[ColOverArrayBuilderData, ColOverArrayBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(colBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertColBuilder(x: Rep[ColBuilder]) = ColOverArrayBuilder()
    override def getDefaultRep = ColOverArrayBuilder()
    override lazy val tag = {
      weakTypeTag[ColOverArrayBuilder]
    }
  }

  // state representation type
  type ColOverArrayBuilderData = Unit

  // 3) Iso for concrete class
  class ColOverArrayBuilderIso
    extends EntityIso[ColOverArrayBuilderData, ColOverArrayBuilder] with Def[ColOverArrayBuilderIso] {
    override def from(p: Rep[ColOverArrayBuilder]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      ColOverArrayBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ColOverArrayBuilderElem(self)
    lazy val selfType = new ColOverArrayBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ColOverArrayBuilderIsoElem() extends Elem[ColOverArrayBuilderIso] {
    def getDefaultRep = reifyObject(new ColOverArrayBuilderIso())
    lazy val tag = {
      weakTypeTag[ColOverArrayBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ColOverArrayBuilderCompanionCtor extends CompanionDef[ColOverArrayBuilderCompanionCtor] with ColOverArrayBuilderCompanion {
    def selfType = ColOverArrayBuilderCompanionElem
    override def toString = "ColOverArrayBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ColOverArrayBuilderData]): Rep[ColOverArrayBuilder] = {
      isoColOverArrayBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ColOverArrayBuilder] =
      mkColOverArrayBuilder()

    def unapply(p: Rep[ColBuilder]) = unmkColOverArrayBuilder(p)
  }
  lazy val ColOverArrayBuilderRep: Rep[ColOverArrayBuilderCompanionCtor] = new ColOverArrayBuilderCompanionCtor
  lazy val ColOverArrayBuilder: ColOverArrayBuilderCompanionCtor = proxyColOverArrayBuilderCompanion(ColOverArrayBuilderRep)
  implicit def proxyColOverArrayBuilderCompanion(p: Rep[ColOverArrayBuilderCompanionCtor]): ColOverArrayBuilderCompanionCtor = {
    proxyOps[ColOverArrayBuilderCompanionCtor](p)
  }

  implicit case object ColOverArrayBuilderCompanionElem extends CompanionElem[ColOverArrayBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[ColOverArrayBuilderCompanionCtor]
    protected def getDefaultRep = ColOverArrayBuilderRep
  }

  implicit def proxyColOverArrayBuilder(p: Rep[ColOverArrayBuilder]): ColOverArrayBuilder =
    proxyOps[ColOverArrayBuilder](p)

  implicit class ExtendedColOverArrayBuilder(p: Rep[ColOverArrayBuilder]) {
    def toData: Rep[ColOverArrayBuilderData] = {
      isoColOverArrayBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoColOverArrayBuilder: Iso[ColOverArrayBuilderData, ColOverArrayBuilder] =
    reifyObject(new ColOverArrayBuilderIso())

  case class ArrayFunctorCtor
      ()
    extends ArrayFunctor() with Def[ArrayFunctor] {
    override lazy val cF: Cont[WArray] = implicitly[Cont[WArray]]
    lazy val selfType = element[ArrayFunctor]
  }
  // elem for concrete class
  class ArrayFunctorElem(val iso: Iso[ArrayFunctorData, ArrayFunctor])
    extends FunctorElem[WArray, ArrayFunctor]
    with ConcreteElem[ArrayFunctorData, ArrayFunctor] {
    override lazy val parent: Option[Elem[_]] = Some(functorElement(container[WArray]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertFunctor(x: Rep[Functor[WArray]]) = ArrayFunctor()
    override def getDefaultRep = ArrayFunctor()
    override lazy val tag = {
      weakTypeTag[ArrayFunctor]
    }
  }

  // state representation type
  type ArrayFunctorData = Unit

  // 3) Iso for concrete class
  class ArrayFunctorIso
    extends EntityIso[ArrayFunctorData, ArrayFunctor] with Def[ArrayFunctorIso] {
    override def from(p: Rep[ArrayFunctor]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      ArrayFunctor()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ArrayFunctorElem(self)
    lazy val selfType = new ArrayFunctorIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ArrayFunctorIsoElem() extends Elem[ArrayFunctorIso] {
    def getDefaultRep = reifyObject(new ArrayFunctorIso())
    lazy val tag = {
      weakTypeTag[ArrayFunctorIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ArrayFunctorCompanionCtor extends CompanionDef[ArrayFunctorCompanionCtor] with ArrayFunctorCompanion {
    def selfType = ArrayFunctorCompanionElem
    override def toString = "ArrayFunctorCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ArrayFunctorData]): Rep[ArrayFunctor] = {
      isoArrayFunctor.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ArrayFunctor] =
      mkArrayFunctor()

    def unapply(p: Rep[Functor[WArray]]) = unmkArrayFunctor(p)
  }
  lazy val ArrayFunctorRep: Rep[ArrayFunctorCompanionCtor] = new ArrayFunctorCompanionCtor
  lazy val ArrayFunctor: ArrayFunctorCompanionCtor = proxyArrayFunctorCompanion(ArrayFunctorRep)
  implicit def proxyArrayFunctorCompanion(p: Rep[ArrayFunctorCompanionCtor]): ArrayFunctorCompanionCtor = {
    proxyOps[ArrayFunctorCompanionCtor](p)
  }

  implicit case object ArrayFunctorCompanionElem extends CompanionElem[ArrayFunctorCompanionCtor] {
    lazy val tag = weakTypeTag[ArrayFunctorCompanionCtor]
    protected def getDefaultRep = ArrayFunctorRep
  }

  implicit def proxyArrayFunctor(p: Rep[ArrayFunctor]): ArrayFunctor =
    proxyOps[ArrayFunctor](p)

  implicit class ExtendedArrayFunctor(p: Rep[ArrayFunctor]) {
    def toData: Rep[ArrayFunctorData] = {
      isoArrayFunctor.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoArrayFunctor: Iso[ArrayFunctorData, ArrayFunctor] =
    reifyObject(new ArrayFunctorIso())

  registerModule(ColsOverArraysModule)

  object ColOverArrayMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[ColOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[ColOverArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ColOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[ColOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[ColOverArray[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[ColOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Option[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "foreach" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object exists {
      def unapply(d: Def[_]): Option[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "exists" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ColOverArrayCompanionMethods {
  }

  def mkColOverArray[A]
    (arr: Rep[WArray[A]]): Rep[ColOverArray[A]] = {
    new ColOverArrayCtor[A](arr)
  }
  def unmkColOverArray[A](p: Rep[Col[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ColOverArrayElem[A] @unchecked =>
      Some((p.asRep[ColOverArray[A]].arr))
    case _ =>
      None
  }

  object PairOfColsMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object arr {
      def unapply(d: Def[_]): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "foreach" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object exists {
      def unapply(d: Def[_]): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "exists" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairOfColsCompanionMethods {
  }

  def mkPairOfCols[L, R]
    (ls: Rep[Col[L]], rs: Rep[Col[R]]): Rep[PairOfCols[L, R]] = {
    new PairOfColsCtor[L, R](ls, rs)
  }
  def unmkPairOfCols[L, R](p: Rep[PairCol[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairOfColsElem[L, R] @unchecked =>
      Some((p.asRep[PairOfCols[L, R]].ls, p.asRep[PairOfCols[L, R]].rs))
    case _ =>
      None
  }

  object ColOverArrayBuilderMethods {
    object apply_apply {
      def unapply(d: Def[_]): Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(as, bs, _*), _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "apply" } =>
          Some((receiver, as, bs)).asInstanceOf[Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `apply`: Method has repeated argument items

    object fromArray {
      def unapply(d: Def[_]): Option[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "fromArray" =>
          Some((receiver, arr)).asInstanceOf[Option[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(n, v, _*), _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "replicate" =>
          Some((receiver, n, v)).asInstanceOf[Option[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dot {
      def unapply(d: Def[_]): Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, ys, _*), _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "dot" =>
          Some((receiver, xs, ys)).asInstanceOf[Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ColOverArrayBuilderCompanionMethods {
  }

  def mkColOverArrayBuilder
    (): Rep[ColOverArrayBuilder] = {
    new ColOverArrayBuilderCtor()
  }
  def unmkColOverArrayBuilder(p: Rep[ColBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ColOverArrayBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

  object ArrayFunctorMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[ArrayFunctor], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(fa, f, _*), _) if receiver.elem.isInstanceOf[ArrayFunctorElem] && method.getName == "map" =>
          Some((receiver, fa, f)).asInstanceOf[Option[(Rep[ArrayFunctor], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayFunctor], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ArrayFunctorCompanionMethods {
  }

  def mkArrayFunctor
    (): Rep[ArrayFunctor] = {
    new ArrayFunctorCtor()
  }
  def unmkArrayFunctor(p: Rep[Functor[WArray]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayFunctorElem @unchecked =>
      Some(())
    case _ =>
      None
  }
}

object ColsOverArraysModule extends scalan.ModuleInfo("scalan.collection", "ColsOverArrays")
}

trait ColsOverArraysModule extends scalan.collection.impl.ColsOverArraysDefs {self: Library =>}
