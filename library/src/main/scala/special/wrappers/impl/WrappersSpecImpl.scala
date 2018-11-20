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
import WEither._
import WOption._
import WrapSpecBase._
import ArrayWrapSpec._
import EitherWrapSpec._
import OptionWrapSpec._
import RTypeWrapSpec._
import SpecialPredefWrapSpec._

object WrapSpecBase extends EntityObject("WrapSpecBase") {
  // entityAdapter for WrapSpecBase trait
  case class WrapSpecBaseAdapter(source: Rep[WrapSpecBase])
      extends WrapSpecBase with Def[WrapSpecBase] {
    val selfType: Elem[WrapSpecBase] = element[WrapSpecBase]
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

  implicit def wrapSpecBaseElement: Elem[WrapSpecBase] =
    cachedElem[WrapSpecBaseElem[WrapSpecBase]]()

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
  case class ArrayWrapSpecCtor
      ()
    extends ArrayWrapSpec() with Def[ArrayWrapSpec] {
    lazy val selfType = element[ArrayWrapSpec]
    private val thisClass = classOf[ArrayWrapSpec]

    override def foldLeft[A, B](xs: Rep[WArray[A]], zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eA = xs.eT
implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("foldLeft", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, zero, op),
        true, isAdapterCall = false, element[B]))
    }
  }
  // elem for concrete class
  class ArrayWrapSpecElem(val iso: Iso[ArrayWrapSpecData, ArrayWrapSpec])
    extends WrapSpecBaseElem[ArrayWrapSpec]
    with ConcreteElem[ArrayWrapSpecData, ArrayWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RArrayWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkArrayWrapSpec(p)
  }
  lazy val ArrayWrapSpecRep: Rep[ArrayWrapSpecCompanionCtor] = new ArrayWrapSpecCompanionCtor
  lazy val RArrayWrapSpec: ArrayWrapSpecCompanionCtor = proxyArrayWrapSpecCompanion(ArrayWrapSpecRep)
  implicit def proxyArrayWrapSpecCompanion(p: Rep[ArrayWrapSpecCompanionCtor]): ArrayWrapSpecCompanionCtor = {
    if (p.rhs.isInstanceOf[ArrayWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[ArrayWrapSpecCompanionCtor]
    else
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
  def unmkArrayWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object ArrayWrapSpecMethods {
    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[ArrayWrapSpec], Rep[WArray[A]], Rep[WArray[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "zip" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "map" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "length" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "fill" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "slice" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "foldLeft" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "filter" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "forall" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "exists" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "foreach" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ArrayWrapSpecElem] && method.getName == "apply" =>
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
  case class OptionWrapSpecCtor
      ()
    extends OptionWrapSpec() with Def[OptionWrapSpec] {
    lazy val selfType = element[OptionWrapSpec]
    private val thisClass = classOf[OptionWrapSpec]

    override def getOrElse[A](xs: Rep[WOption[A]], default: Rep[Thunk[A]]): Rep[A] = {
      implicit val eA = xs.eA
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(xs, default),
        true, isAdapterCall = false, element[A]))
    }

    override def fold[A, B](xs: Rep[WOption[A]], ifEmpty: Rep[Thunk[B]], f: Rep[A => B]): Rep[B] = {
      implicit val eA = xs.eA
implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(xs, ifEmpty, f),
        true, isAdapterCall = false, element[B]))
    }
  }
  // elem for concrete class
  class OptionWrapSpecElem(val iso: Iso[OptionWrapSpecData, OptionWrapSpec])
    extends WrapSpecBaseElem[OptionWrapSpec]
    with ConcreteElem[OptionWrapSpecData, OptionWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = ROptionWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkOptionWrapSpec(p)
  }
  lazy val OptionWrapSpecRep: Rep[OptionWrapSpecCompanionCtor] = new OptionWrapSpecCompanionCtor
  lazy val ROptionWrapSpec: OptionWrapSpecCompanionCtor = proxyOptionWrapSpecCompanion(OptionWrapSpecRep)
  implicit def proxyOptionWrapSpecCompanion(p: Rep[OptionWrapSpecCompanionCtor]): OptionWrapSpecCompanionCtor = {
    if (p.rhs.isInstanceOf[OptionWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[OptionWrapSpecCompanionCtor]
    else
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
  def unmkOptionWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: OptionWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object OptionWrapSpecMethods {
    object get {
      def unapply(d: Def[_]): Nullable[(Rep[OptionWrapSpec], Rep[WOption[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "get" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "getOrElse" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "map" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "flatMap" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "filter" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "isDefined" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "isEmpty" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[OptionWrapSpecElem] && method.getName == "fold" =>
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

object EitherWrapSpec extends EntityObject("EitherWrapSpec") {
  case class EitherWrapSpecCtor
      ()
    extends EitherWrapSpec() with Def[EitherWrapSpec] {
    lazy val selfType = element[EitherWrapSpec]
  }
  // elem for concrete class
  class EitherWrapSpecElem(val iso: Iso[EitherWrapSpecData, EitherWrapSpec])
    extends WrapSpecBaseElem[EitherWrapSpec]
    with ConcreteElem[EitherWrapSpecData, EitherWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = REitherWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkEitherWrapSpec(p)
  }
  lazy val EitherWrapSpecRep: Rep[EitherWrapSpecCompanionCtor] = new EitherWrapSpecCompanionCtor
  lazy val REitherWrapSpec: EitherWrapSpecCompanionCtor = proxyEitherWrapSpecCompanion(EitherWrapSpecRep)
  implicit def proxyEitherWrapSpecCompanion(p: Rep[EitherWrapSpecCompanionCtor]): EitherWrapSpecCompanionCtor = {
    if (p.rhs.isInstanceOf[EitherWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[EitherWrapSpecCompanionCtor]
    else
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
  def unmkEitherWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: EitherWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object EitherWrapSpecMethods {
    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[EitherWrapSpecElem] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[EitherWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cond {
      def unapply(d: Def[_]): Nullable[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[EitherWrapSpecElem] && method.getName == "cond" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[EitherWrapSpec], Rep[Boolean], Rep[Thunk[A]], Rep[Thunk[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
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
    extends WrapSpecBaseElem[SpecialPredefWrapSpec]
    with ConcreteElem[SpecialPredefWrapSpecData, SpecialPredefWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RSpecialPredefWrapSpec()
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

    def unapply(p: Rep[WrapSpecBase]) = unmkSpecialPredefWrapSpec(p)
  }
  lazy val SpecialPredefWrapSpecRep: Rep[SpecialPredefWrapSpecCompanionCtor] = new SpecialPredefWrapSpecCompanionCtor
  lazy val RSpecialPredefWrapSpec: SpecialPredefWrapSpecCompanionCtor = proxySpecialPredefWrapSpecCompanion(SpecialPredefWrapSpecRep)
  implicit def proxySpecialPredefWrapSpecCompanion(p: Rep[SpecialPredefWrapSpecCompanionCtor]): SpecialPredefWrapSpecCompanionCtor = {
    if (p.rhs.isInstanceOf[SpecialPredefWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[SpecialPredefWrapSpecCompanionCtor]
    else
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
  def unmkSpecialPredefWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SpecialPredefWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object SpecialPredefWrapSpecMethods {
    object loopUntil {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "loopUntil" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "cast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[Any], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mapSum {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "mapSum" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object some {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "some" =>
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
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "none" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Elem[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Elem[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object left {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "left" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object right {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "right" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[SpecialPredefWrapSpec], Rep[B], Elem[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object optionGetOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[SpecialPredefWrapSpec], Rep[WOption[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[SpecialPredefWrapSpecElem] && method.getName == "optionGetOrElse" =>
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
  case class RTypeWrapSpecCtor
      ()
    extends RTypeWrapSpec() with Def[RTypeWrapSpec] {
    lazy val selfType = element[RTypeWrapSpec]
  }
  // elem for concrete class
  class RTypeWrapSpecElem(val iso: Iso[RTypeWrapSpecData, RTypeWrapSpec])
    extends WrapSpecBaseElem[RTypeWrapSpec]
    with ConcreteElem[RTypeWrapSpecData, RTypeWrapSpec] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertWrapSpecBase(x: Rep[WrapSpecBase]) = RRTypeWrapSpec()
    override def getDefaultRep = RRTypeWrapSpec()
    override lazy val tag = {
      weakTypeTag[RTypeWrapSpec]
    }
  }

  // state representation type
  type RTypeWrapSpecData = Unit

  // 3) Iso for concrete class
  class RTypeWrapSpecIso
    extends EntityIso[RTypeWrapSpecData, RTypeWrapSpec] with Def[RTypeWrapSpecIso] {
    private lazy val _safeFrom = fun { p: Rep[RTypeWrapSpec] => () }
    override def from(p: Rep[RTypeWrapSpec]) =
      tryConvert[RTypeWrapSpec, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RRTypeWrapSpec()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new RTypeWrapSpecElem(self)
    lazy val selfType = new RTypeWrapSpecIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class RTypeWrapSpecIsoElem() extends Elem[RTypeWrapSpecIso] {
    def getDefaultRep = reifyObject(new RTypeWrapSpecIso())
    lazy val tag = {
      weakTypeTag[RTypeWrapSpecIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class RTypeWrapSpecCompanionCtor extends CompanionDef[RTypeWrapSpecCompanionCtor] with RTypeWrapSpecCompanion {
    def selfType = RTypeWrapSpecCompanionElem
    override def toString = "RTypeWrapSpecCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[RTypeWrapSpecData]): Rep[RTypeWrapSpec] = {
      isoRTypeWrapSpec.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[RTypeWrapSpec] =
      mkRTypeWrapSpec()

    def unapply(p: Rep[WrapSpecBase]) = unmkRTypeWrapSpec(p)
  }
  lazy val RTypeWrapSpecRep: Rep[RTypeWrapSpecCompanionCtor] = new RTypeWrapSpecCompanionCtor
  lazy val RRTypeWrapSpec: RTypeWrapSpecCompanionCtor = proxyRTypeWrapSpecCompanion(RTypeWrapSpecRep)
  implicit def proxyRTypeWrapSpecCompanion(p: Rep[RTypeWrapSpecCompanionCtor]): RTypeWrapSpecCompanionCtor = {
    if (p.rhs.isInstanceOf[RTypeWrapSpecCompanionCtor])
      p.rhs.asInstanceOf[RTypeWrapSpecCompanionCtor]
    else
      proxyOps[RTypeWrapSpecCompanionCtor](p)
  }

  implicit case object RTypeWrapSpecCompanionElem extends CompanionElem[RTypeWrapSpecCompanionCtor] {
    lazy val tag = weakTypeTag[RTypeWrapSpecCompanionCtor]
    protected def getDefaultRep = RTypeWrapSpecRep
  }

  implicit def proxyRTypeWrapSpec(p: Rep[RTypeWrapSpec]): RTypeWrapSpec =
    proxyOps[RTypeWrapSpec](p)

  implicit class ExtendedRTypeWrapSpec(p: Rep[RTypeWrapSpec]) {
    def toData: Rep[RTypeWrapSpecData] = {
      isoRTypeWrapSpec.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoRTypeWrapSpec: Iso[RTypeWrapSpecData, RTypeWrapSpec] =
    reifyObject(new RTypeWrapSpecIso())

  def mkRTypeWrapSpec
    (): Rep[RTypeWrapSpec] = {
    new RTypeWrapSpecCtor()
  }
  def unmkRTypeWrapSpec(p: Rep[WrapSpecBase]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: RTypeWrapSpecElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object RTypeWrapSpecMethods {
    object name {
      def unapply(d: Def[_]): Nullable[(Rep[RTypeWrapSpec], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[RTypeWrapSpecElem] && method.getName == "name" =>
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
