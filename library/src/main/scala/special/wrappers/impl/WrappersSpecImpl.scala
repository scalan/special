package special.wrappers

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: Library =>
import IsoUR._
import Converter._
import WArray._
import WOption._
import WRType._
import WSpecialPredef._
import WrapSpecBase._
import ArrayWrapSpec._
import OptionWrapSpec._
import RTypeWrapSpec._
import SpecialPredefWrapSpec._

object WrapSpecBase extends EntityObject("WrapSpecBase") {
  // entityAdapter for WrapSpecBase trait
  case class WrapSpecBaseAdapter(source: Rep[WrapSpecBase])
      extends WrapSpecBase with Def[WrapSpecBase] {
    val selfType: Elem[WrapSpecBase] = element[WrapSpecBase]
    override def transform(t: Transformer) = WrapSpecBaseAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWrapSpecBase(p: Rep[WrapSpecBase]): WrapSpecBase = {
    if (p.rhs.isInstanceOf[WrapSpecBase@unchecked]) p.rhs.asInstanceOf[WrapSpecBase]
    else
      WrapSpecBaseAdapter(p)
  }

  // familyElem
  class WrapSpecBaseElem[To <: WrapSpecBase]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WrapSpecBase].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WrapSpecBase] => convertWrapSpecBase(x) }
      tryConvert(element[WrapSpecBase], this, x, conv)
    }

    def convertWrapSpecBase(x: Rep[WrapSpecBase]): Rep[To] = {
      x.elem match {
        case _: WrapSpecBaseElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have WrapSpecBaseElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val wrapSpecBaseElement: Elem[WrapSpecBase] =
    new WrapSpecBaseElem[WrapSpecBase]

  implicit case object WrapSpecBaseCompanionElem extends CompanionElem[WrapSpecBaseCompanionCtor] {
    lazy val tag = weakTypeTag[WrapSpecBaseCompanionCtor]
    protected def getDefaultRep = RWrapSpecBase
  }

  abstract class WrapSpecBaseCompanionCtor extends CompanionDef[WrapSpecBaseCompanionCtor] with WrapSpecBaseCompanion {
    def selfType = WrapSpecBaseCompanionElem
    override def toString = "WrapSpecBase"
  }
  implicit def proxyWrapSpecBaseCompanionCtor(p: Rep[WrapSpecBaseCompanionCtor]): WrapSpecBaseCompanionCtor =
    proxyOps[WrapSpecBaseCompanionCtor](p)

  lazy val RWrapSpecBase: Rep[WrapSpecBaseCompanionCtor] = new WrapSpecBaseCompanionCtor {
    private val thisClass = classOf[WrapSpecBaseCompanion]
  }

  object WrapSpecBaseMethods {
  }

  object WrapSpecBaseCompanionMethods {
  }
} // of object WrapSpecBase
  registerEntityObject("WrapSpecBase", WrapSpecBase)

object ArrayWrapSpec extends EntityObject("ArrayWrapSpec") {
  // entityAdapter for ArrayWrapSpec trait
  case class ArrayWrapSpecAdapter(source: Rep[ArrayWrapSpec])
      extends ArrayWrapSpec with Def[ArrayWrapSpec] {
    val selfType: Elem[ArrayWrapSpec] = element[ArrayWrapSpec]
    override def transform(t: Transformer) = ArrayWrapSpecAdapter(t(source))
    private val thisClass = classOf[ArrayWrapSpec]

    override def foldLeft[A, B](xs: Rep[WArray[A]], zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eA = xs.eT
implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("foldLeft", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, zero, op),
        true, true, element[B]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyArrayWrapSpec(p: Rep[ArrayWrapSpec]): ArrayWrapSpec = {
    if (p.rhs.isInstanceOf[ArrayWrapSpec@unchecked]) p.rhs.asInstanceOf[ArrayWrapSpec]
    else
      ArrayWrapSpecAdapter(p)
  }

  // familyElem
  class ArrayWrapSpecElem[To <: ArrayWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[ArrayWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ArrayWrapSpec] => convertArrayWrapSpec(x) }
      tryConvert(element[ArrayWrapSpec], this, x, conv)
    }

    def convertArrayWrapSpec(x: Rep[ArrayWrapSpec]): Rep[To] = {
      x.elem match {
        case _: ArrayWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have ArrayWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val arrayWrapSpecElement: Elem[ArrayWrapSpec] =
    new ArrayWrapSpecElem[ArrayWrapSpec]

  implicit case object ArrayWrapSpecCompanionElem extends CompanionElem[ArrayWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[ArrayWrapSpecCompanionCtor]
    protected def getDefaultRep = RArrayWrapSpec
  }

  abstract class ArrayWrapSpecCompanionCtor extends CompanionDef[ArrayWrapSpecCompanionCtor] with ArrayWrapSpecCompanion {
    def selfType = ArrayWrapSpecCompanionElem
    override def toString = "ArrayWrapSpec"
  }
  implicit def proxyArrayWrapSpecCompanionCtor(p: Rep[ArrayWrapSpecCompanionCtor]): ArrayWrapSpecCompanionCtor =
    proxyOps[ArrayWrapSpecCompanionCtor](p)

  lazy val RArrayWrapSpec: Rep[ArrayWrapSpecCompanionCtor] = new ArrayWrapSpecCompanionCtor {
    private val thisClass = classOf[ArrayWrapSpecCompanion]
  }

  object ArrayWrapSpecMethods {
    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "zip" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "length" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fill {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "fill" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldLeft {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "foldLeft" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "forall" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "exists" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "foreach" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem[_]] && method.getName == "apply" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ArrayWrapSpecCompanionMethods {
  }
} // of object ArrayWrapSpec
  registerEntityObject("ArrayWrapSpec", ArrayWrapSpec)

object OptionWrapSpec extends EntityObject("OptionWrapSpec") {
  // entityAdapter for OptionWrapSpec trait
  case class OptionWrapSpecAdapter(source: Rep[OptionWrapSpec])
      extends OptionWrapSpec with Def[OptionWrapSpec] {
    val selfType: Elem[OptionWrapSpec] = element[OptionWrapSpec]
    override def transform(t: Transformer) = OptionWrapSpecAdapter(t(source))
    private val thisClass = classOf[OptionWrapSpec]

    override def getOrElse[A](xs: Rep[WOption[A]], default: Rep[Thunk[A]]): Rep[A] = {
      implicit val eA = xs.eA
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(xs, default),
        true, true, element[A]))
    }

    override def fold[A, B](xs: Rep[WOption[A]], ifEmpty: Rep[Thunk[B]], f: Rep[A => B]): Rep[B] = {
      implicit val eA = xs.eA
implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, ifEmpty, f),
        true, true, element[B]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyOptionWrapSpec(p: Rep[OptionWrapSpec]): OptionWrapSpec = {
    if (p.rhs.isInstanceOf[OptionWrapSpec@unchecked]) p.rhs.asInstanceOf[OptionWrapSpec]
    else
      OptionWrapSpecAdapter(p)
  }

  // familyElem
  class OptionWrapSpecElem[To <: OptionWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[OptionWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[OptionWrapSpec] => convertOptionWrapSpec(x) }
      tryConvert(element[OptionWrapSpec], this, x, conv)
    }

    def convertOptionWrapSpec(x: Rep[OptionWrapSpec]): Rep[To] = {
      x.elem match {
        case _: OptionWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have OptionWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val optionWrapSpecElement: Elem[OptionWrapSpec] =
    new OptionWrapSpecElem[OptionWrapSpec]

  implicit case object OptionWrapSpecCompanionElem extends CompanionElem[OptionWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[OptionWrapSpecCompanionCtor]
    protected def getDefaultRep = ROptionWrapSpec
  }

  abstract class OptionWrapSpecCompanionCtor extends CompanionDef[OptionWrapSpecCompanionCtor] with OptionWrapSpecCompanion {
    def selfType = OptionWrapSpecCompanionElem
    override def toString = "OptionWrapSpec"
  }
  implicit def proxyOptionWrapSpecCompanionCtor(p: Rep[OptionWrapSpecCompanionCtor]): OptionWrapSpecCompanionCtor =
    proxyOps[OptionWrapSpecCompanionCtor](p)

  lazy val ROptionWrapSpec: Rep[OptionWrapSpecCompanionCtor] = new OptionWrapSpecCompanionCtor {
    private val thisClass = classOf[OptionWrapSpecCompanion]
  }

  object OptionWrapSpecMethods {
    object get {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "get" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "map" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "flatMap" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "filter" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isDefined {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "isDefined" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "isEmpty" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem[_]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object OptionWrapSpecCompanionMethods {
  }
} // of object OptionWrapSpec
  registerEntityObject("OptionWrapSpec", OptionWrapSpec)

object SpecialPredefWrapSpec extends EntityObject("SpecialPredefWrapSpec") {
  // entityAdapter for SpecialPredefWrapSpec trait
  case class SpecialPredefWrapSpecAdapter(source: Rep[SpecialPredefWrapSpec])
      extends SpecialPredefWrapSpec with Def[SpecialPredefWrapSpec] {
    val selfType: Elem[SpecialPredefWrapSpec] = element[SpecialPredefWrapSpec]
    override def transform(t: Transformer) = SpecialPredefWrapSpecAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxySpecialPredefWrapSpec(p: Rep[SpecialPredefWrapSpec]): SpecialPredefWrapSpec = {
    if (p.rhs.isInstanceOf[SpecialPredefWrapSpec@unchecked]) p.rhs.asInstanceOf[SpecialPredefWrapSpec]
    else
      SpecialPredefWrapSpecAdapter(p)
  }

  // familyElem
  class SpecialPredefWrapSpecElem[To <: SpecialPredefWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[SpecialPredefWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[SpecialPredefWrapSpec] => convertSpecialPredefWrapSpec(x) }
      tryConvert(element[SpecialPredefWrapSpec], this, x, conv)
    }

    def convertSpecialPredefWrapSpec(x: Rep[SpecialPredefWrapSpec]): Rep[To] = {
      x.elem match {
        case _: SpecialPredefWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SpecialPredefWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val specialPredefWrapSpecElement: Elem[SpecialPredefWrapSpec] =
    new SpecialPredefWrapSpecElem[SpecialPredefWrapSpec]

  implicit case object SpecialPredefWrapSpecCompanionElem extends CompanionElem[SpecialPredefWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[SpecialPredefWrapSpecCompanionCtor]
    protected def getDefaultRep = RSpecialPredefWrapSpec
  }

  abstract class SpecialPredefWrapSpecCompanionCtor extends CompanionDef[SpecialPredefWrapSpecCompanionCtor] with SpecialPredefWrapSpecCompanion {
    def selfType = SpecialPredefWrapSpecCompanionElem
    override def toString = "SpecialPredefWrapSpec"
  }
  implicit def proxySpecialPredefWrapSpecCompanionCtor(p: Rep[SpecialPredefWrapSpecCompanionCtor]): SpecialPredefWrapSpecCompanionCtor =
    proxyOps[SpecialPredefWrapSpecCompanionCtor](p)

  lazy val RSpecialPredefWrapSpec: Rep[SpecialPredefWrapSpecCompanionCtor] = new SpecialPredefWrapSpecCompanionCtor {
    private val thisClass = classOf[SpecialPredefWrapSpecCompanion]
  }

  object SpecialPredefWrapSpecMethods {
    object loopUntil {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem[_]] && method.getName == "loopUntil" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cast {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem[_]] && method.getName == "cast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object some {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem[_]] && method.getName == "some" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object none {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem[_]] && method.getName == "none" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object optionGetOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem[_]] && method.getName == "optionGetOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object SpecialPredefWrapSpecCompanionMethods {
  }
} // of object SpecialPredefWrapSpec
  registerEntityObject("SpecialPredefWrapSpec", SpecialPredefWrapSpec)

object RTypeWrapSpec extends EntityObject("RTypeWrapSpec") {
  // entityAdapter for RTypeWrapSpec trait
  case class RTypeWrapSpecAdapter(source: Rep[RTypeWrapSpec])
      extends RTypeWrapSpec with Def[RTypeWrapSpec] {
    val selfType: Elem[RTypeWrapSpec] = element[RTypeWrapSpec]
    override def transform(t: Transformer) = RTypeWrapSpecAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyRTypeWrapSpec(p: Rep[RTypeWrapSpec]): RTypeWrapSpec = {
    if (p.rhs.isInstanceOf[RTypeWrapSpec@unchecked]) p.rhs.asInstanceOf[RTypeWrapSpec]
    else
      RTypeWrapSpecAdapter(p)
  }

  // familyElem
  class RTypeWrapSpecElem[To <: RTypeWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[RTypeWrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[RTypeWrapSpec] => convertRTypeWrapSpec(x) }
      tryConvert(element[RTypeWrapSpec], this, x, conv)
    }

    def convertRTypeWrapSpec(x: Rep[RTypeWrapSpec]): Rep[To] = {
      x.elem match {
        case _: RTypeWrapSpecElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have RTypeWrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val rTypeWrapSpecElement: Elem[RTypeWrapSpec] =
    new RTypeWrapSpecElem[RTypeWrapSpec]

  implicit case object RTypeWrapSpecCompanionElem extends CompanionElem[RTypeWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[RTypeWrapSpecCompanionCtor]
    protected def getDefaultRep = RRTypeWrapSpec
  }

  abstract class RTypeWrapSpecCompanionCtor extends CompanionDef[RTypeWrapSpecCompanionCtor] with RTypeWrapSpecCompanion {
    def selfType = RTypeWrapSpecCompanionElem
    override def toString = "RTypeWrapSpec"
  }
  implicit def proxyRTypeWrapSpecCompanionCtor(p: Rep[RTypeWrapSpecCompanionCtor]): RTypeWrapSpecCompanionCtor =
    proxyOps[RTypeWrapSpecCompanionCtor](p)

  lazy val RRTypeWrapSpec: Rep[RTypeWrapSpecCompanionCtor] = new RTypeWrapSpecCompanionCtor {
    private val thisClass = classOf[RTypeWrapSpecCompanion]
  }

  object RTypeWrapSpecMethods {
    object name {
      def unapply(d: Def[_]): Nullable[(Rep[RTypeWrapSpec], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[RTypeWrapSpecElem[_]] && method.getName == "name" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[RTypeWrapSpec], Rep[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[RTypeWrapSpec], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object RTypeWrapSpecCompanionMethods {
  }
} // of object RTypeWrapSpec
  registerEntityObject("RTypeWrapSpec", RTypeWrapSpec)

  registerModule(WrappersSpecModule)
}

object WrappersSpecModule extends scalan.ModuleInfo("special.wrappers", "WrappersSpec")
}

trait WrappersSpecModule extends special.wrappers.impl.WrappersSpecDefs {self: Library =>}
