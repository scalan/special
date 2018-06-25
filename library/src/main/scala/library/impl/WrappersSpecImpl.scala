package library

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: Library =>

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
    protected def getDefaultRep = WrapSpec
  }

  abstract class WrapSpecCompanionCtor extends CompanionDef[WrapSpecCompanionCtor] with WrapSpecCompanion {
    def selfType = WrapSpecCompanionElem
    override def toString = "WrapSpec"
  }
  implicit def proxyWrapSpecCompanionCtor(p: Rep[WrapSpecCompanionCtor]): WrapSpecCompanionCtor =
    proxyOps[WrapSpecCompanionCtor](p)

  lazy val WrapSpec: Rep[WrapSpecCompanionCtor] = new WrapSpecCompanionCtor {
  }

  object WrapSpecMethods {
  }

  object WrapSpecCompanionMethods {
  }

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
    override def convertWrapSpec(x: Rep[WrapSpec]) = ArrayWrapSpec()
    override def getDefaultRep = ArrayWrapSpec()
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
      ArrayWrapSpec()
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
  lazy val ArrayWrapSpec: ArrayWrapSpecCompanionCtor = proxyArrayWrapSpecCompanion(ArrayWrapSpecRep)
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
    override def convertWrapSpec(x: Rep[WrapSpec]) = SpecialPredefWrapSpec()
    override def getDefaultRep = SpecialPredefWrapSpec()
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
      SpecialPredefWrapSpec()
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
  lazy val SpecialPredefWrapSpec: SpecialPredefWrapSpecCompanionCtor = proxySpecialPredefWrapSpecCompanion(SpecialPredefWrapSpecRep)
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

  registerModule(WrappersSpecModule)

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
  }

  object SpecialPredefWrapSpecCompanionMethods {
  }

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
}

object WrappersSpecModule extends scalan.ModuleInfo("library", "WrappersSpec")
}

trait WrappersSpecModule extends library.impl.WrappersSpecDefs {self: Library =>}
