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
import WrapSpec._
import WArray._
import WOption._
import WEither._
import ArrayWrapSpec._
import OptionWrapSpec._
import EitherWrapSpec._
import SpecialPredefWrapSpec._

object WrapSpec extends EntityObject("WrapSpec") {
  // entityProxy: single proxy for each type family
  implicit def proxyWrapSpec(p: Rep[WrapSpec]): WrapSpec = {
    proxyOps[WrapSpec](p)(scala.reflect.classTag[WrapSpec])
  }

  // familyElem
  class WrapSpecElem[To <: WrapSpec]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WrapSpec].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WrapSpec] => convertWrapSpec(x) }
      tryConvert(element[WrapSpec], this, x, conv)
    }

    def convertWrapSpec(x: Rep[WrapSpec]): Rep[To] = {
      x.elem match {
        case _: WrapSpecElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have WrapSpecElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wrapSpecElement: Elem[WrapSpec] =
    cachedElem[WrapSpecElem[WrapSpec]]()

  implicit case object WrapSpecCompanionElem extends CompanionElem[WrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[WrapSpecCompanionCtor]
    protected def getDefaultRep = RWrapSpec
  }

  abstract class WrapSpecCompanionCtor extends CompanionDef[WrapSpecCompanionCtor] with WrapSpecCompanion {
    def selfType = WrapSpecCompanionElem
    override def toString = "WrapSpec"
  }
  implicit def proxyWrapSpecCompanionCtor(p: Rep[WrapSpecCompanionCtor]): WrapSpecCompanionCtor =
    proxyOps[WrapSpecCompanionCtor](p)

  lazy val RWrapSpec: Rep[WrapSpecCompanionCtor] = new WrapSpecCompanionCtor {
  }

  object WrapSpecMethods {
  }

  object WrapSpecCompanionMethods {
  }
} // of object WrapSpec
  registerEntityObject("WrapSpec", WrapSpec)

object ArrayWrapSpec extends EntityObject("ArrayWrapSpec") {
  case class ArrayWrapSpecCtor
      ()
    extends ArrayWrapSpec() with Def[ArrayWrapSpec] {
    lazy val selfType = element[ArrayWrapSpec]
  }
  // elem for concrete class
  class ArrayWrapSpecElem(val iso: Iso[ArrayWrapSpecData, ArrayWrapSpec])
    extends WrapSpecElem[ArrayWrapSpec]
    with ConcreteElem[ArrayWrapSpecData, ArrayWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = RArrayWrapSpec()
    override def getDefaultRep = RArrayWrapSpec()
    override lazy val tag = {
      weakTypeTag[ArrayWrapSpec]
    }
  }

  // state representation type
  type ArrayWrapSpecData = Unit

  // 3) Iso for concrete class
  class ArrayWrapSpecIso
    extends EntityIso[ArrayWrapSpecData, ArrayWrapSpec] with Def[ArrayWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[ArrayWrapSpec] => () }
    override def from(p: Rep[ArrayWrapSpec]) =
      tryConvert[ArrayWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RArrayWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new ArrayWrapSpecElem(self)
    lazy val selfType = new ArrayWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ArrayWrapSpecIsoElem() extends Elem[ArrayWrapSpecIso] {
    def getDefaultRep = reifyObject(new ArrayWrapSpecIso())
    lazy val tag = {
      weakTypeTag[ArrayWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ArrayWrapSpecCompanionCtor extends CompanionDef[ArrayWrapSpecCompanionCtor] with ArrayWrapSpecCompanion {
    def selfType = ArrayWrapSpecCompanionElem
    override def toString = "ArrayWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ArrayWrapSpecData]): Rep[ArrayWrapSpec] = {
      isoArrayWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[ArrayWrapSpec] =
      mkArrayWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkArrayWrapSpec(p)
  }
  lazy val ArrayWrapSpecRep: Rep[ArrayWrapSpecCompanionCtor] = new ArrayWrapSpecCompanionCtor
  lazy val RArrayWrapSpec: ArrayWrapSpecCompanionCtor = proxyArrayWrapSpecCompanion(ArrayWrapSpecRep)
  implicit def proxyArrayWrapSpecCompanion(p: Rep[ArrayWrapSpecCompanionCtor]): ArrayWrapSpecCompanionCtor = {
    proxyOps[ArrayWrapSpecCompanionCtor](p)
  }

  implicit case object ArrayWrapSpecCompanionElem extends CompanionElem[ArrayWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[ArrayWrapSpecCompanionCtor]
    protected def getDefaultRep = ArrayWrapSpecRep
  }

  implicit def proxyArrayWrapSpec(p: Rep[ArrayWrapSpec]): ArrayWrapSpec =
    proxyOps[ArrayWrapSpec](p)

  implicit class ExtendedArrayWrapSpec(p: Rep[ArrayWrapSpec]) {
    def toData: Rep[ArrayWrapSpecData] = {
      isoArrayWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoArrayWrapSpec: Iso[ArrayWrapSpecData, ArrayWrapSpec] =
    reifyObject(new ArrayWrapSpecIso())

  def mkArrayWrapSpec
    (): Rep[ArrayWrapSpec] = {
    new ArrayWrapSpecCtor()
  }
  def unmkArrayWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object ArrayWrapSpecMethods {
    object zip {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, ys, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "zip" =>
          Some((receiver, xs, ys)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, f, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "map" =>
          Some((receiver, xs, f)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "length" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fill {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(n, elem, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "fill" =>
          Some((receiver, n, elem)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object slice {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, from, until, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "slice" =>
          Some((receiver, xs, from, until)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foldLeft {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, zero, op, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "foldLeft" =>
          Some((receiver, xs, zero, op)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, p, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "filter" =>
          Some((receiver, xs, p)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object forall {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, p, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "forall" =>
          Some((receiver, xs, p)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object exists {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, p, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "exists" =>
          Some((receiver, xs, p)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, p, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "foreach" =>
          Some((receiver, xs, p)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, i, _*), _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "apply" =>
          Some((receiver, xs, i)).asInstanceOf[Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ArrayWrapSpecCompanionMethods {
  }
} // of object ArrayWrapSpec
  registerEntityObject("ArrayWrapSpec", ArrayWrapSpec)

object OptionWrapSpec extends EntityObject("OptionWrapSpec") {
  case class OptionWrapSpecCtor
      ()
    extends OptionWrapSpec() with Def[OptionWrapSpec] {
    lazy val selfType = element[OptionWrapSpec]
  }
  // elem for concrete class
  class OptionWrapSpecElem(val iso: Iso[OptionWrapSpecData, OptionWrapSpec])
    extends WrapSpecElem[OptionWrapSpec]
    with ConcreteElem[OptionWrapSpecData, OptionWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = ROptionWrapSpec()
    override def getDefaultRep = ROptionWrapSpec()
    override lazy val tag = {
      weakTypeTag[OptionWrapSpec]
    }
  }

  // state representation type
  type OptionWrapSpecData = Unit

  // 3) Iso for concrete class
  class OptionWrapSpecIso
    extends EntityIso[OptionWrapSpecData, OptionWrapSpec] with Def[OptionWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[OptionWrapSpec] => () }
    override def from(p: Rep[OptionWrapSpec]) =
      tryConvert[OptionWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      ROptionWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new OptionWrapSpecElem(self)
    lazy val selfType = new OptionWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class OptionWrapSpecIsoElem() extends Elem[OptionWrapSpecIso] {
    def getDefaultRep = reifyObject(new OptionWrapSpecIso())
    lazy val tag = {
      weakTypeTag[OptionWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class OptionWrapSpecCompanionCtor extends CompanionDef[OptionWrapSpecCompanionCtor] with OptionWrapSpecCompanion {
    def selfType = OptionWrapSpecCompanionElem
    override def toString = "OptionWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[OptionWrapSpecData]): Rep[OptionWrapSpec] = {
      isoOptionWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[OptionWrapSpec] =
      mkOptionWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkOptionWrapSpec(p)
  }
  lazy val OptionWrapSpecRep: Rep[OptionWrapSpecCompanionCtor] = new OptionWrapSpecCompanionCtor
  lazy val ROptionWrapSpec: OptionWrapSpecCompanionCtor = proxyOptionWrapSpecCompanion(OptionWrapSpecRep)
  implicit def proxyOptionWrapSpecCompanion(p: Rep[OptionWrapSpecCompanionCtor]): OptionWrapSpecCompanionCtor = {
    proxyOps[OptionWrapSpecCompanionCtor](p)
  }

  implicit case object OptionWrapSpecCompanionElem extends CompanionElem[OptionWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[OptionWrapSpecCompanionCtor]
    protected def getDefaultRep = OptionWrapSpecRep
  }

  implicit def proxyOptionWrapSpec(p: Rep[OptionWrapSpec]): OptionWrapSpec =
    proxyOps[OptionWrapSpec](p)

  implicit class ExtendedOptionWrapSpec(p: Rep[OptionWrapSpec]) {
    def toData: Rep[OptionWrapSpecData] = {
      isoOptionWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoOptionWrapSpec: Iso[OptionWrapSpecData, OptionWrapSpec] =
    reifyObject(new OptionWrapSpecIso())

  def mkOptionWrapSpec
    (): Rep[OptionWrapSpec] = {
    new OptionWrapSpecCtor()
  }
  def unmkOptionWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: OptionWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object OptionWrapSpecMethods {
    object get {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "get" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, default, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "getOrElse" =>
          Some((receiver, xs, default)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, f, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "map" =>
          Some((receiver, xs, f)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, f, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "flatMap" =>
          Some((receiver, xs, f)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => WOption[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, f, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "filter" =>
          Some((receiver, xs, f)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isDefined {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "isDefined" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(xs, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "isEmpty" =>
          Some((receiver, xs)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fold {
      def unapply(d: Def[_]): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(xs, ifEmpty, f, _*), _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "fold" =>
          Some((receiver, xs, ifEmpty, f)).asInstanceOf[Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[OptionWrapSpec], Rep[WOption[A]], Rep[Thunk[B]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object OptionWrapSpecCompanionMethods {
  }
} // of object OptionWrapSpec
  registerEntityObject("OptionWrapSpec", OptionWrapSpec)

object EitherWrapSpec extends EntityObject("EitherWrapSpec") {
  case class EitherWrapSpecCtor
      ()
    extends EitherWrapSpec() with Def[EitherWrapSpec] {
    lazy val selfType = element[EitherWrapSpec]
  }
  // elem for concrete class
  class EitherWrapSpecElem(val iso: Iso[EitherWrapSpecData, EitherWrapSpec])
    extends WrapSpecElem[EitherWrapSpec]
    with ConcreteElem[EitherWrapSpecData, EitherWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = REitherWrapSpec()
    override def getDefaultRep = REitherWrapSpec()
    override lazy val tag = {
      weakTypeTag[EitherWrapSpec]
    }
  }

  // state representation type
  type EitherWrapSpecData = Unit

  // 3) Iso for concrete class
  class EitherWrapSpecIso
    extends EntityIso[EitherWrapSpecData, EitherWrapSpec] with Def[EitherWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[EitherWrapSpec] => () }
    override def from(p: Rep[EitherWrapSpec]) =
      tryConvert[EitherWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      REitherWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new EitherWrapSpecElem(self)
    lazy val selfType = new EitherWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class EitherWrapSpecIsoElem() extends Elem[EitherWrapSpecIso] {
    def getDefaultRep = reifyObject(new EitherWrapSpecIso())
    lazy val tag = {
      weakTypeTag[EitherWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class EitherWrapSpecCompanionCtor extends CompanionDef[EitherWrapSpecCompanionCtor] with EitherWrapSpecCompanion {
    def selfType = EitherWrapSpecCompanionElem
    override def toString = "EitherWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[EitherWrapSpecData]): Rep[EitherWrapSpec] = {
      isoEitherWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[EitherWrapSpec] =
      mkEitherWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkEitherWrapSpec(p)
  }
  lazy val EitherWrapSpecRep: Rep[EitherWrapSpecCompanionCtor] = new EitherWrapSpecCompanionCtor
  lazy val REitherWrapSpec: EitherWrapSpecCompanionCtor = proxyEitherWrapSpecCompanion(EitherWrapSpecRep)
  implicit def proxyEitherWrapSpecCompanion(p: Rep[EitherWrapSpecCompanionCtor]): EitherWrapSpecCompanionCtor = {
    proxyOps[EitherWrapSpecCompanionCtor](p)
  }

  implicit case object EitherWrapSpecCompanionElem extends CompanionElem[EitherWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[EitherWrapSpecCompanionCtor]
    protected def getDefaultRep = EitherWrapSpecRep
  }

  implicit def proxyEitherWrapSpec(p: Rep[EitherWrapSpec]): EitherWrapSpec =
    proxyOps[EitherWrapSpec](p)

  implicit class ExtendedEitherWrapSpec(p: Rep[EitherWrapSpec]) {
    def toData: Rep[EitherWrapSpecData] = {
      isoEitherWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoEitherWrapSpec: Iso[EitherWrapSpecData, EitherWrapSpec] =
    reifyObject(new EitherWrapSpecIso())

  def mkEitherWrapSpec
    (): Rep[EitherWrapSpec] = {
    new EitherWrapSpecCtor()
  }
  def unmkEitherWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: EitherWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object EitherWrapSpecMethods {
    object fold {
      def unapply(d: Def[_]): Option[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(xs, fa, fb, _*), _) if receiver.elem.isInstanceOf[EitherWrapSpecElem] && method.getName == "fold" =>
          Some((receiver, xs, fa, fb)).asInstanceOf[Option[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cond {
      def unapply(d: Def[_]): Option[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(c, a, b, _*), _) if receiver.elem.isInstanceOf[EitherWrapSpecElem] && method.getName == "cond" =>
          Some((receiver, c, a, b)).asInstanceOf[Option[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object EitherWrapSpecCompanionMethods {
  }
} // of object EitherWrapSpec
  registerEntityObject("EitherWrapSpec", EitherWrapSpec)

object SpecialPredefWrapSpec extends EntityObject("SpecialPredefWrapSpec") {
  case class SpecialPredefWrapSpecCtor
      ()
    extends SpecialPredefWrapSpec() with Def[SpecialPredefWrapSpec] {
    lazy val selfType = element[SpecialPredefWrapSpec]
  }
  // elem for concrete class
  class SpecialPredefWrapSpecElem(val iso: Iso[SpecialPredefWrapSpecData, SpecialPredefWrapSpec])
    extends WrapSpecElem[SpecialPredefWrapSpec]
    with ConcreteElem[SpecialPredefWrapSpecData, SpecialPredefWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpec(x: Rep[WrapSpec]) = RSpecialPredefWrapSpec()
    override def getDefaultRep = RSpecialPredefWrapSpec()
    override lazy val tag = {
      weakTypeTag[SpecialPredefWrapSpec]
    }
  }

  // state representation type
  type SpecialPredefWrapSpecData = Unit

  // 3) Iso for concrete class
  class SpecialPredefWrapSpecIso
    extends EntityIso[SpecialPredefWrapSpecData, SpecialPredefWrapSpec] with Def[SpecialPredefWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[SpecialPredefWrapSpec] => () }
    override def from(p: Rep[SpecialPredefWrapSpec]) =
      tryConvert[SpecialPredefWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RSpecialPredefWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new SpecialPredefWrapSpecElem(self)
    lazy val selfType = new SpecialPredefWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SpecialPredefWrapSpecIsoElem() extends Elem[SpecialPredefWrapSpecIso] {
    def getDefaultRep = reifyObject(new SpecialPredefWrapSpecIso())
    lazy val tag = {
      weakTypeTag[SpecialPredefWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class SpecialPredefWrapSpecCompanionCtor extends CompanionDef[SpecialPredefWrapSpecCompanionCtor] with SpecialPredefWrapSpecCompanion {
    def selfType = SpecialPredefWrapSpecCompanionElem
    override def toString = "SpecialPredefWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[SpecialPredefWrapSpecData]): Rep[SpecialPredefWrapSpec] = {
      isoSpecialPredefWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[SpecialPredefWrapSpec] =
      mkSpecialPredefWrapSpec()

    def unapply(p: Rep[WrapSpec]) = unmkSpecialPredefWrapSpec(p)
  }
  lazy val SpecialPredefWrapSpecRep: Rep[SpecialPredefWrapSpecCompanionCtor] = new SpecialPredefWrapSpecCompanionCtor
  lazy val RSpecialPredefWrapSpec: SpecialPredefWrapSpecCompanionCtor = proxySpecialPredefWrapSpecCompanion(SpecialPredefWrapSpecRep)
  implicit def proxySpecialPredefWrapSpecCompanion(p: Rep[SpecialPredefWrapSpecCompanionCtor]): SpecialPredefWrapSpecCompanionCtor = {
    proxyOps[SpecialPredefWrapSpecCompanionCtor](p)
  }

  implicit case object SpecialPredefWrapSpecCompanionElem extends CompanionElem[SpecialPredefWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[SpecialPredefWrapSpecCompanionCtor]
    protected def getDefaultRep = SpecialPredefWrapSpecRep
  }

  implicit def proxySpecialPredefWrapSpec(p: Rep[SpecialPredefWrapSpec]): SpecialPredefWrapSpec =
    proxyOps[SpecialPredefWrapSpec](p)

  implicit class ExtendedSpecialPredefWrapSpec(p: Rep[SpecialPredefWrapSpec]) {
    def toData: Rep[SpecialPredefWrapSpecData] = {
      isoSpecialPredefWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSpecialPredefWrapSpec: Iso[SpecialPredefWrapSpecData, SpecialPredefWrapSpec] =
    reifyObject(new SpecialPredefWrapSpecIso())

  def mkSpecialPredefWrapSpec
    (): Rep[SpecialPredefWrapSpec] = {
    new SpecialPredefWrapSpecCtor()
  }
  def unmkSpecialPredefWrapSpec(p: Rep[WrapSpec]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SpecialPredefWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object SpecialPredefWrapSpecMethods {
    object loopUntil {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(s1, isMatch, step, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "loopUntil" =>
          Some((receiver, s1, isMatch, step)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cast {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(v, cA, emA, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "cast" =>
          Some((receiver, v, cA, emA)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A], Elem[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapSum {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(e, fa, fb, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "mapSum" =>
          Some((receiver, e, fa, fb)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object some {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "some" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object none {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Elem[A], Elem[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(cA, emA, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "none" =>
          Some((receiver, cA, emA)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Elem[A], Elem[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Elem[A], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object left {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(a, cB, emB, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "left" =>
          Some((receiver, a, cB, emB)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object right {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A], Elem[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(b, cA, emA, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "right" =>
          Some((receiver, b, cA, emA)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A], Elem[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A], Elem[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object optionGetOrElse {
      def unapply(d: Def[_]): Option[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(opt, default, _*), _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "optionGetOrElse" =>
          Some((receiver, opt, default)).asInstanceOf[Option[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SpecialPredefWrapSpecCompanionMethods {
  }
} // of object SpecialPredefWrapSpec
  registerEntityObject("SpecialPredefWrapSpec", SpecialPredefWrapSpec)

  registerModule(WrappersSpecModule)
}

object WrappersSpecModule extends scalan.ModuleInfo("special.wrappers", "WrappersSpec")
}

trait WrappersSpecModule extends special.wrappers.impl.WrappersSpecDefs {self: Library =>}
