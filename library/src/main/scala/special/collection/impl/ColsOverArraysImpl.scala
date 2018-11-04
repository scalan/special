package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ColsOverArraysDefs extends scalan.Scalan with ColsOverArrays {
  self: Library =>
import IsoUR._
import Converter._
import CReplCol._
import Col._
import ColBuilder._
import ColOverArray._
import ColOverArrayBuilder._
import PairCol._
import PairOfCols._
import ReplCol._
import WArray._

object ColOverArray extends EntityObject("ColOverArray") {
  case class ColOverArrayCtor[A]
      (override val arr: Rep[WArray[A]])
    extends ColOverArray[A](arr) with Def[ColOverArray[A]] {
    implicit lazy val eA = arr.eT

    lazy val selfType = element[ColOverArray[A]]
    private val thisClass = classOf[ColOverArray[A]]

    override def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, element[A]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, element[B]))
    }

    override def append(other: Rep[Col[A]]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(self,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, element[Col[A]]))
    }
  }
  // elem for concrete class
  class ColOverArrayElem[A](val iso: Iso[ColOverArrayData[A], ColOverArray[A]])(implicit override val eA: Elem[A])
    extends ColElem[A, ColOverArray[A]]
    with ConcreteElem1[A, ColOverArrayData[A], ColOverArray[A], Col] {
    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertCol(x: Rep[Col[A]]) = RColOverArray(x.arr)
    override def getDefaultRep = RColOverArray(element[WArray[A]].defaultRepValue)
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
    private lazy val _safeFrom = fun { p: Rep[ColOverArray[A]] => p.arr }
    override def from(p: Rep[ColOverArray[A]]) =
      tryConvert[ColOverArray[A], WArray[A]](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WArray[A]]) = {
      val arr = p
      RColOverArray(arr)
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
  lazy val RColOverArray: ColOverArrayCompanionCtor = proxyColOverArrayCompanion(ColOverArrayRep)
  implicit def proxyColOverArrayCompanion(p: Rep[ColOverArrayCompanionCtor]): ColOverArrayCompanionCtor = {
    if (p.rhs.isInstanceOf[ColOverArrayCompanionCtor])
      p.rhs.asInstanceOf[ColOverArrayCompanionCtor]
    else
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

  def mkColOverArray[A]
    (arr: Rep[WArray[A]]): Rep[ColOverArray[A]] = {
    new ColOverArrayCtor[A](arr)
  }
  def unmkColOverArray[A](p: Rep[Col[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ColOverArrayElem[A] @unchecked =>
      Some((asRep[ColOverArray[A]](p).arr))
    case _ =>
      None
  }

    object ColOverArrayMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[ColOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ColOverArray[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ColOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[ColOverArray[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ColOverArray[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ColOverArray[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArray[A]], Rep[Col[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayElem[_]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArray[A]], Rep[Col[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArray[A]], Rep[Col[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ColOverArrayCompanionMethods {
  }
} // of object ColOverArray
  registerEntityObject("ColOverArray", ColOverArray)

object ColOverArrayBuilder extends EntityObject("ColOverArrayBuilder") {
  case class ColOverArrayBuilderCtor
      ()
    extends ColOverArrayBuilder() with Def[ColOverArrayBuilder] {
    lazy val selfType = element[ColOverArrayBuilder]
    private val thisClass = classOf[ColBuilder] // manual fix

    override def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Col[T]] = {
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("fromItems", classOf[Sym], classOf[Elem[_]]),
        List(items, cT),
        true, element[Col[T]]))
    }

    override def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = {
      implicit val eT = arr.eT
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, element[Col[T]]))
    }

    override def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = {
      implicit val eT = v.elem
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, element[Col[T]]))
    }

    override def xor(left: Rep[Col[Byte]], right: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, element[Col[Byte]]))
    }
  }
  // elem for concrete class
  class ColOverArrayBuilderElem(val iso: Iso[ColOverArrayBuilderData, ColOverArrayBuilder])
    extends ColBuilderElem[ColOverArrayBuilder]
    with ConcreteElem[ColOverArrayBuilderData, ColOverArrayBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(colBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertColBuilder(x: Rep[ColBuilder]) = RColOverArrayBuilder()
    override def getDefaultRep = RColOverArrayBuilder()
    override lazy val tag = {
      weakTypeTag[ColOverArrayBuilder]
    }
  }

  // state representation type
  type ColOverArrayBuilderData = Unit

  // 3) Iso for concrete class
  class ColOverArrayBuilderIso
    extends EntityIso[ColOverArrayBuilderData, ColOverArrayBuilder] with Def[ColOverArrayBuilderIso] {
    private lazy val _safeFrom = fun { p: Rep[ColOverArrayBuilder] => () }
    override def from(p: Rep[ColOverArrayBuilder]) =
      tryConvert[ColOverArrayBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RColOverArrayBuilder()
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
  lazy val RColOverArrayBuilder: ColOverArrayBuilderCompanionCtor = proxyColOverArrayBuilderCompanion(ColOverArrayBuilderRep)
  implicit def proxyColOverArrayBuilderCompanion(p: Rep[ColOverArrayBuilderCompanionCtor]): ColOverArrayBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[ColOverArrayBuilderCompanionCtor])
      p.rhs.asInstanceOf[ColOverArrayBuilderCompanionCtor]
    else
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

    object ColOverArrayBuilderMethods {
    object pairCol {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "pairCol" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArrayBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArrayBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "fromArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArrayBuilder], Rep[WArray[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "replicate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArrayBuilder], Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[ColOverArrayBuilder], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColOverArrayBuilderElem] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColOverArrayBuilder], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColOverArrayBuilder], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ColOverArrayBuilderCompanionMethods {
  }
} // of object ColOverArrayBuilder
  registerEntityObject("ColOverArrayBuilder", ColOverArrayBuilder)

object PairOfCols extends EntityObject("PairOfCols") {
  case class PairOfColsCtor[L, R]
      (override val ls: Rep[Col[L]], override val rs: Rep[Col[R]])
    extends PairOfCols[L, R](ls, rs) with Def[PairOfCols[L, R]] {
    implicit lazy val eL = ls.eA;
implicit lazy val eR = rs.eA
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val selfType = element[PairOfCols[L, R]]
    private val thisClass = classOf[PairOfCols[L, R]]

    override def getOrElse(i: Rep[Int], default: Rep[(L, R)]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, element[(L, R)]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, (L, R))) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, element[B]))
    }

    override def sum(m: Rep[Monoid[(L, R)]]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(self,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, element[(L, R)]))
    }
  }
  // elem for concrete class
  class PairOfColsElem[L, R](val iso: Iso[PairOfColsData[L, R], PairOfCols[L, R]])(implicit override val eL: Elem[L], override val eR: Elem[R])
    extends PairColElem[L, R, PairOfCols[L, R]]
    with ConcreteElem[PairOfColsData[L, R], PairOfCols[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(pairColElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override def convertPairCol(x: Rep[PairCol[L, R]]) = RPairOfCols(x.ls, x.rs)
    override def getDefaultRep = RPairOfCols(element[Col[L]].defaultRepValue, element[Col[R]].defaultRepValue)
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
    private lazy val _safeFrom = fun { p: Rep[PairOfCols[L, R]] => (p.ls, p.rs) }
    override def from(p: Rep[PairOfCols[L, R]]) =
      tryConvert[PairOfCols[L, R], (Col[L], Col[R])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Col[L], Col[R])]) = {
      val Pair(ls, rs) = p
      RPairOfCols(ls, rs)
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
  lazy val RPairOfCols: PairOfColsCompanionCtor = proxyPairOfColsCompanion(PairOfColsRep)
  implicit def proxyPairOfColsCompanion(p: Rep[PairOfColsCompanionCtor]): PairOfColsCompanionCtor = {
    if (p.rhs.isInstanceOf[PairOfColsCompanionCtor])
      p.rhs.asInstanceOf[PairOfColsCompanionCtor]
    else
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

  def mkPairOfCols[L, R]
    (ls: Rep[Col[L]], rs: Rep[Col[R]]): Rep[PairOfCols[L, R]] = {
    new PairOfColsCtor[L, R](ls, rs)
  }
  def unmkPairOfCols[L, R](p: Rep[PairCol[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairOfColsElem[L, R] @unchecked =>
      Some((asRep[PairOfCols[L, R]](p).ls, asRep[PairOfCols[L, R]](p).rs))
    case _ =>
      None
  }

    object PairOfColsMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairOfCols[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[(L, R)]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => V]) forSome {type L; type R; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Unit]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[((L, R)) => Boolean]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[B], Rep[((B, (L, R))) => B]) forSome {type L; type R; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Int], Rep[Int]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Col[(L, R)]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Col[(L, R)]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Col[(L, R)]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Monoid[(L, R)]]) forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[PairOfCols[L, R]], Rep[Col[B]]) forSome {type L; type R; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[PairOfColsElem[_, _]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[PairOfCols[L, R]], Rep[Col[B]]) forSome {type L; type R; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[PairOfCols[L, R]], Rep[Col[B]]) forSome {type L; type R; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PairOfColsCompanionMethods {
  }
} // of object PairOfCols
  registerEntityObject("PairOfCols", PairOfCols)

object CReplCol extends EntityObject("CReplCol") {
  case class CReplColCtor[A]
      (override val value: Rep[A], override val length: Rep[Int])
    extends CReplCol[A](value, length) with Def[CReplCol[A]] {
    implicit lazy val eA = value.elem

    lazy val selfType = element[CReplCol[A]]
    private val thisClass = classOf[ReplCol[A]] // manual fix

    override def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, element[A]))
    }

    override def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(self,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, element[Unit]))
    }

    override def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, element[B]))
    }

    override def append(other: Rep[Col[A]]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(self,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, element[Col[A]]))
    }
  }
  // elem for concrete class
  class CReplColElem[A](val iso: Iso[CReplColData[A], CReplCol[A]])(implicit override val eA: Elem[A])
    extends ReplColElem[A, CReplCol[A]]
    with ConcreteElem[CReplColData[A], CReplCol[A]] {
    override lazy val parent: Option[Elem[_]] = Some(replColElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override def convertReplCol(x: Rep[ReplCol[A]]) = RCReplCol(x.value, x.length)
    override def getDefaultRep = RCReplCol(element[A].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CReplCol[A]]
    }
  }

  // state representation type
  type CReplColData[A] = (A, Int)

  // 3) Iso for concrete class
  class CReplColIso[A](implicit eA: Elem[A])
    extends EntityIso[CReplColData[A], CReplCol[A]] with Def[CReplColIso[A]] {
    private lazy val _safeFrom = fun { p: Rep[CReplCol[A]] => (p.value, p.length) }
    override def from(p: Rep[CReplCol[A]]) =
      tryConvert[CReplCol[A], (A, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(A, Int)]) = {
      val Pair(value, length) = p
      RCReplCol(value, length)
    }
    lazy val eFrom = pairElement(element[A], element[Int])
    lazy val eTo = new CReplColElem[A](self)
    lazy val selfType = new CReplColIsoElem[A](eA)
    def productArity = 1
    def productElement(n: Int) = eA
  }
  case class CReplColIsoElem[A](eA: Elem[A]) extends Elem[CReplColIso[A]] {
    def getDefaultRep = reifyObject(new CReplColIso[A]()(eA))
    lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CReplColIso[A]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class CReplColCompanionCtor extends CompanionDef[CReplColCompanionCtor] with CReplColCompanion {
    def selfType = CReplColCompanionElem
    override def toString = "CReplColCompanion"
    @scalan.OverloadId("fromData")
    def apply[A](p: Rep[CReplColData[A]]): Rep[CReplCol[A]] = {
      implicit val eA = p._1.elem
      isoCReplCol[A].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A](value: Rep[A], length: Rep[Int]): Rep[CReplCol[A]] =
      mkCReplCol(value, length)

    def unapply[A](p: Rep[ReplCol[A]]) = unmkCReplCol(p)
  }
  lazy val CReplColRep: Rep[CReplColCompanionCtor] = new CReplColCompanionCtor
  lazy val RCReplCol: CReplColCompanionCtor = proxyCReplColCompanion(CReplColRep)
  implicit def proxyCReplColCompanion(p: Rep[CReplColCompanionCtor]): CReplColCompanionCtor = {
    if (p.rhs.isInstanceOf[CReplColCompanionCtor])
      p.rhs.asInstanceOf[CReplColCompanionCtor]
    else
      proxyOps[CReplColCompanionCtor](p)
  }

  implicit case object CReplColCompanionElem extends CompanionElem[CReplColCompanionCtor] {
    lazy val tag = weakTypeTag[CReplColCompanionCtor]
    protected def getDefaultRep = CReplColRep
  }

  implicit def proxyCReplCol[A](p: Rep[CReplCol[A]]): CReplCol[A] =
    proxyOps[CReplCol[A]](p)

  implicit class ExtendedCReplCol[A](p: Rep[CReplCol[A]]) {
    def toData: Rep[CReplColData[A]] = {
      implicit val eA = p.value.elem
      isoCReplCol(eA).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCReplCol[A](implicit eA: Elem[A]): Iso[CReplColData[A], CReplCol[A]] =
    reifyObject(new CReplColIso[A]()(eA))

  def mkCReplCol[A]
    (value: Rep[A], length: Rep[Int]): Rep[CReplCol[A]] = {
    new CReplColCtor[A](value, length)
  }
  def unmkCReplCol[A](p: Rep[ReplCol[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CReplColElem[A] @unchecked =>
      Some((asRep[CReplCol[A]](p).value, asRep[CReplCol[A]](p).length))
    case _ =>
      None
  }

    object CReplColMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[CReplCol[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CReplCol[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CReplCol[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[CReplCol[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CReplCol[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CReplCol[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Col[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Col[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Col[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[CReplCol[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CReplColElem[_]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[CReplCol[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CReplCol[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CReplColCompanionMethods {
  }
} // of object CReplCol
  registerEntityObject("CReplCol", CReplCol)

  registerModule(ColsOverArraysModule)
}

object ColsOverArraysModule extends scalan.ModuleInfo("special.collection", "ColsOverArrays")
}

trait ColsOverArraysModule extends special.collection.impl.ColsOverArraysDefs {self: Library =>}
