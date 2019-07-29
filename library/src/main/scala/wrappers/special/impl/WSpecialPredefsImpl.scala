package wrappers.special

import scalan._
import impl._
import special.wrappers.WrappersModule
import special.wrappers.SpecialPredefWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSpecialPredefsDefs extends scalan.Scalan with WSpecialPredefs {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WOption._
import WSpecialPredef._

object WSpecialPredef extends EntityObject("WSpecialPredef") {
  private val WSpecialPredefClass = classOf[WSpecialPredef]

  // entityAdapter for WSpecialPredef trait
  case class WSpecialPredefAdapter(source: Rep[WSpecialPredef])
      extends WSpecialPredef
      with Def[WSpecialPredef] {
    val selfType: Elem[WSpecialPredef] = element[WSpecialPredef]
    override def transform(t: Transformer) = WSpecialPredefAdapter(t(source))
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWSpecialPredef(p: Rep[WSpecialPredef]): WSpecialPredef = {
    if (p.rhs.isInstanceOf[WSpecialPredef]) p.rhs.asInstanceOf[WSpecialPredef]
    else
      WSpecialPredefAdapter(p)
  }

  // familyElem
  class WSpecialPredefElem[To <: WSpecialPredef]
    extends EntityElem[To] {
  }

  implicit lazy val wSpecialPredefElement: Elem[WSpecialPredef] =
    new WSpecialPredefElem[WSpecialPredef]

  implicit case object WSpecialPredefCompanionElem extends CompanionElem[WSpecialPredefCompanionCtor]

  abstract class WSpecialPredefCompanionCtor extends CompanionDef[WSpecialPredefCompanionCtor] with WSpecialPredefCompanion {
    def selfType = WSpecialPredefCompanionElem
    override def toString = "WSpecialPredef"
  }
  implicit def proxyWSpecialPredefCompanionCtor(p: Rep[WSpecialPredefCompanionCtor]): WSpecialPredefCompanionCtor =
    p.rhs.asInstanceOf[WSpecialPredefCompanionCtor]

  lazy val RWSpecialPredef: Rep[WSpecialPredefCompanionCtor] = new WSpecialPredefCompanionCtor {
    private val thisClass = classOf[WSpecialPredefCompanion]

    def optionGetOrElse[A](opt: Rep[WOption[A]], default: Rep[A]): Rep[A] = {
      implicit val eA = opt.eA
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("optionGetOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](opt, default),
        true, false, element[A]))
    }

    def none[A](implicit emA: Elem[A]): Rep[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        thisClass.getMethod("none", classOf[Elem[_]]),
        Array[AnyRef](emA),
        true, false, element[WOption[A]]))
    }

    def some[A](x: Rep[A]): Rep[WOption[A]] = {
      implicit val eA = x.elem
      asRep[WOption[A]](mkMethodCall(self,
        thisClass.getMethod("some", classOf[Sym]),
        Array[AnyRef](x),
        true, false, element[WOption[A]]))
    }

    def cast[T](v: Rep[Any])(implicit emT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("cast", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](v, emT),
        true, false, element[WOption[T]]))
    }

    def loopUntil[A](s1: Rep[A], isMatch: Rep[A => Boolean], step: Rep[A => A]): Rep[A] = {
      implicit val eA = s1.elem
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("loopUntil", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](s1, isMatch, step),
        true, false, element[A]))
    }
  }

  object WSpecialPredefMethods {
  }

  object WSpecialPredefCompanionMethods {
    object optionGetOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[WOption[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "optionGetOrElse" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WOption[A]], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WOption[A]], Rep[A]) forSome {type A}] = unapply(exp.rhs)
    }

    object none {
      def unapply(d: Def[_]): Nullable[Elem[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "none" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Elem[A] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Elem[A] forSome {type A}] = unapply(exp.rhs)
    }

    object some {
      def unapply(d: Def[_]): Nullable[Rep[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "some" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Rep[A] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[A] forSome {type A}] = unapply(exp.rhs)
    }

    object cast {
      def unapply(d: Def[_]): Nullable[(Rep[Any], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "cast" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Any], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Any], Elem[T]) forSome {type T}] = unapply(exp.rhs)
    }

    object loopUntil {
      def unapply(d: Def[_]): Nullable[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "loopUntil" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = (args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[A], Rep[A => Boolean], Rep[A => A]) forSome {type A}] = unapply(exp.rhs)
    }
  }
} // of object WSpecialPredef
  registerEntityObject("WSpecialPredef", WSpecialPredef)

  registerModule(WSpecialPredefsModule)
}

object WSpecialPredefsModule extends scalan.ModuleInfo("wrappers.special", "WSpecialPredefs")
}

trait WSpecialPredefsModule extends wrappers.special.impl.WSpecialPredefsDefs {self: WrappersModule =>}
