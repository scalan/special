package wrappers.special

import scalan._
import impl._
import special.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSpecialPredefsDefs extends scalan.Scalan with WSpecialPredefs {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WEither._
import WOption._
import WSpecialPredef._

object WSpecialPredef extends EntityObject("WSpecialPredef") {
  // entityProxy: single proxy for each type family
  implicit def proxyWSpecialPredef(p: Rep[WSpecialPredef]): WSpecialPredef = {
    proxyOps[WSpecialPredef](p)(scala.reflect.classTag[WSpecialPredef])
  }

  // familyElem
  class WSpecialPredefElem[To <: WSpecialPredef]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[WSpecialPredef].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WSpecialPredef] => convertWSpecialPredef(x) }
      tryConvert(element[WSpecialPredef], this, x, conv)
    }

    def convertWSpecialPredef(x: Rep[WSpecialPredef]): Rep[To] = {
      x.elem match {
        case _: WSpecialPredefElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have WSpecialPredefElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wSpecialPredefElement: Elem[WSpecialPredef] =
    cachedElem[WSpecialPredefElem[WSpecialPredef]]()

  implicit case object WSpecialPredefCompanionElem extends CompanionElem[WSpecialPredefCompanionCtor] {
    lazy val tag = weakTypeTag[WSpecialPredefCompanionCtor]
    protected def getDefaultRep = RWSpecialPredef
  }

  abstract class WSpecialPredefCompanionCtor extends CompanionDef[WSpecialPredefCompanionCtor] with WSpecialPredefCompanion {
    def selfType = WSpecialPredefCompanionElem
    override def toString = "WSpecialPredef"
  }
  implicit def proxyWSpecialPredefCompanionCtor(p: Rep[WSpecialPredefCompanionCtor]): WSpecialPredefCompanionCtor =
    proxyOps[WSpecialPredefCompanionCtor](p)

  lazy val RWSpecialPredef: Rep[WSpecialPredefCompanionCtor] = new WSpecialPredefCompanionCtor {
    def optionGetOrElse[A](opt: Rep[WOption[A]], default: Rep[A]): Rep[A] = {
      implicit val eA = opt.eA
      mkMethodCall(self,
        this.getClass.getMethod("optionGetOrElse", classOf[Sym], classOf[Sym]),
        List(opt, default),
        true, element[A]).asRep[A]
    }

    def right[A, B](b: Rep[B])(implicit emA: Elem[A]): Rep[WEither[A, B]] = {
      implicit val eB = b.elem
      mkMethodCall(self,
        this.getClass.getMethod("right", classOf[Sym], classOf[Sym]),
        List(b, emA),
        true, element[WEither[A, B]]).asRep[WEither[A, B]]
    }

    def left[A, B](a: Rep[A])(implicit emB: Elem[B]): Rep[WEither[A, B]] = {
      implicit val eA = a.elem
      mkMethodCall(self,
        this.getClass.getMethod("left", classOf[Sym], classOf[Sym]),
        List(a, emB),
        true, element[WEither[A, B]]).asRep[WEither[A, B]]
    }

    def none[A](implicit emA: Elem[A]): Rep[WOption[A]] = {
      mkMethodCall(self,
        this.getClass.getMethod("none", classOf[Sym]),
        List(emA),
        true, element[WOption[A]]).asRep[WOption[A]]
    }

    def some[A](x: Rep[A]): Rep[WOption[A]] = {
      implicit val eA = x.elem
      mkMethodCall(self,
        this.getClass.getMethod("some", classOf[Sym]),
        List(x),
        true, element[WOption[A]]).asRep[WOption[A]]
    }

    def eitherMap[A, B, C, D](e: Rep[WEither[A, B]], fa: Rep[A => C], fb: Rep[B => D]): Rep[WEither[C, D]] = {
      implicit val eA = e.eA
implicit val eB = e.eB
implicit val eC = fa.elem.eRange
implicit val eD = fb.elem.eRange
      mkMethodCall(self,
        this.getClass.getMethod("eitherMap", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(e, fa, fb),
        true, element[WEither[C, D]]).asRep[WEither[C, D]]
    }

    def cast[T](v: Rep[Any])(implicit emT: Elem[T]): Rep[WOption[T]] = {
      mkMethodCall(self,
        this.getClass.getMethod("cast", classOf[Sym], classOf[Sym]),
        List(v, emT),
        true, element[WOption[T]]).asRep[WOption[T]]
    }

    def loopUntil[A](s1: Rep[A], isMatch: Rep[A => Boolean], step: Rep[A => A]): Rep[A] = {
      implicit val eA = s1.elem
      mkMethodCall(self,
        this.getClass.getMethod("loopUntil", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(s1, isMatch, step),
        true, element[A]).asRep[A]
    }
  }

  object WSpecialPredefMethods {
  }

  object WSpecialPredefCompanionMethods {
    object optionGetOrElse {
      def unapply(d: Def[_]): Option[(Rep[WOption[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(opt, default, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "optionGetOrElse" =>
          Some((opt, default)).asInstanceOf[Option[(Rep[WOption[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WOption[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object right {
      def unapply(d: Def[_]): Option[(Rep[B], Elem[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(b, emA, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "right" =>
          Some((b, emA)).asInstanceOf[Option[(Rep[B], Elem[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[B], Elem[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object left {
      def unapply(d: Def[_]): Option[(Rep[A], Elem[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(a, emB, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "left" =>
          Some((a, emB)).asInstanceOf[Option[(Rep[A], Elem[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[A], Elem[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object none {
      def unapply(d: Def[_]): Option[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(emA, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "none" =>
          Some(emA).asInstanceOf[Option[Elem[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Elem[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object some {
      def unapply(d: Def[_]): Option[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "some" =>
          Some(x).asInstanceOf[Option[Rep[A] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[A] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object eitherMap {
      def unapply(d: Def[_]): Option[(Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(e, fa, fb, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "eitherMap" =>
          Some((e, fa, fb)).asInstanceOf[Option[(Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[WEither[A, B]], Rep[A => C], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cast {
      def unapply(d: Def[_]): Option[(Rep[Any], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(v, emT, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "cast" =>
          Some((v, emT)).asInstanceOf[Option[(Rep[Any], Elem[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[Any], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object loopUntil {
      def unapply(d: Def[_]): Option[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(s1, isMatch, step, _*), _) if receiver.elem == WSpecialPredefCompanionElem && method.getName == "loopUntil" =>
          Some((s1, isMatch, step)).asInstanceOf[Option[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
} // of object WSpecialPredef
  registerEntityObject("WSpecialPredef", WSpecialPredef)

  registerModule(WSpecialPredefsModule)
}

object WSpecialPredefsModule extends scalan.ModuleInfo("wrappers.special", "WSpecialPredefs")
}

trait WSpecialPredefsModule extends wrappers.special.impl.WSpecialPredefsDefs {self: WrappersModule =>}
