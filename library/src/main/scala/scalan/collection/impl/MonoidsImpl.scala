package scalan.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait MonoidsDefs extends scalan.Scalan with Monoids {
  self: Library =>

  // entityProxy: single proxy for each type family
  implicit def proxyMonoid[T](p: Rep[Monoid[T]]): Monoid[T] = {
    proxyOps[Monoid[T]](p)(scala.reflect.classTag[Monoid[T]])
  }

  // familyElem
  class MonoidElem[T, To <: Monoid[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Monoid[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Monoid[T]] => convertMonoid(x) }
      tryConvert(element[Monoid[T]], this, x, conv)
    }

    def convertMonoid(x: Rep[Monoid[T]]): Rep[To] = {
      x.elem match {
        case _: MonoidElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have MonoidElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def monoidElement[T](implicit eT: Elem[T]): Elem[Monoid[T]] =
    cachedElem[MonoidElem[T, Monoid[T]]](eT)

  implicit case object MonoidCompanionElem extends CompanionElem[MonoidCompanionCtor] {
    lazy val tag = weakTypeTag[MonoidCompanionCtor]
    protected def getDefaultRep = Monoid
  }

  abstract class MonoidCompanionCtor extends CompanionDef[MonoidCompanionCtor] with MonoidCompanion {
    def selfType = MonoidCompanionElem
    override def toString = "Monoid"
  }
  implicit def proxyMonoidCompanionCtor(p: Rep[MonoidCompanionCtor]): MonoidCompanionCtor =
    proxyOps[MonoidCompanionCtor](p)

  lazy val Monoid: Rep[MonoidCompanionCtor] = new MonoidCompanionCtor {
  }

  object MonoidMethods {
    object zero {
      def unapply(d: Def[_]): Option[Rep[Monoid[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MonoidElem[_, _]] && method.getName == "zero" =>
          Some(receiver).asInstanceOf[Option[Rep[Monoid[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Monoid[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object plus {
      def unapply(d: Def[_]): Option[(Rep[Monoid[T]], Rep[T], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(x, y, _*), _) if receiver.elem.isInstanceOf[MonoidElem[_, _]] && method.getName == "plus" =>
          Some((receiver, x, y)).asInstanceOf[Option[(Rep[Monoid[T]], Rep[T], Rep[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Monoid[T]], Rep[T], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object power {
      def unapply(d: Def[_]): Option[(Rep[Monoid[T]], Rep[T], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(x, n, _*), _) if receiver.elem.isInstanceOf[MonoidElem[_, _]] && method.getName == "power" =>
          Some((receiver, x, n)).asInstanceOf[Option[(Rep[Monoid[T]], Rep[T], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Monoid[T]], Rep[T], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MonoidCompanionMethods {
  }

  // entityProxy: single proxy for each type family
  implicit def proxyMonoidBuilder(p: Rep[MonoidBuilder]): MonoidBuilder = {
    proxyOps[MonoidBuilder](p)(scala.reflect.classTag[MonoidBuilder])
  }

  // familyElem
  class MonoidBuilderElem[To <: MonoidBuilder]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[MonoidBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[MonoidBuilder] => convertMonoidBuilder(x) }
      tryConvert(element[MonoidBuilder], this, x, conv)
    }

    def convertMonoidBuilder(x: Rep[MonoidBuilder]): Rep[To] = {
      x.elem match {
        case _: MonoidBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have MonoidBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def monoidBuilderElement: Elem[MonoidBuilder] =
    cachedElem[MonoidBuilderElem[MonoidBuilder]]()

  implicit case object MonoidBuilderCompanionElem extends CompanionElem[MonoidBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[MonoidBuilderCompanionCtor]
    protected def getDefaultRep = MonoidBuilder
  }

  abstract class MonoidBuilderCompanionCtor extends CompanionDef[MonoidBuilderCompanionCtor] with MonoidBuilderCompanion {
    def selfType = MonoidBuilderCompanionElem
    override def toString = "MonoidBuilder"
  }
  implicit def proxyMonoidBuilderCompanionCtor(p: Rep[MonoidBuilderCompanionCtor]): MonoidBuilderCompanionCtor =
    proxyOps[MonoidBuilderCompanionCtor](p)

  lazy val MonoidBuilder: Rep[MonoidBuilderCompanionCtor] = new MonoidBuilderCompanionCtor {
  }

  object MonoidBuilderMethods {
    object intPlusMonoid {
      def unapply(d: Def[_]): Option[Rep[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MonoidBuilderElem[_]] && method.getName == "intPlusMonoid" =>
          Some(receiver).asInstanceOf[Option[Rep[MonoidBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MonoidBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object longPlusMonoid {
      def unapply(d: Def[_]): Option[Rep[MonoidBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MonoidBuilderElem[_]] && method.getName == "longPlusMonoid" =>
          Some(receiver).asInstanceOf[Option[Rep[MonoidBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[MonoidBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MonoidBuilderCompanionMethods {
  }

  registerModule(MonoidsModule)
}

object MonoidsModule extends scalan.ModuleInfo("scalan.collection", "Monoids")
}

trait MonoidsModule extends scalan.collection.impl.MonoidsDefs {self: Library =>}
