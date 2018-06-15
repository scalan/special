package scalan

import scalan._
import impl._
import scala.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WSpecialPredefsDefs extends scalan.Scalan with WSpecialPredefs {
  self: WrappersModule =>

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
    protected def getDefaultRep = WSpecialPredef
  }

  abstract class WSpecialPredefCompanionCtor extends CompanionDef[WSpecialPredefCompanionCtor] with WSpecialPredefCompanion {
    def selfType = WSpecialPredefCompanionElem
    override def toString = "WSpecialPredef"
  }
  implicit def proxyWSpecialPredefCompanionCtor(p: Rep[WSpecialPredefCompanionCtor]): WSpecialPredefCompanionCtor =
    proxyOps[WSpecialPredefCompanionCtor](p)

  lazy val WSpecialPredef: Rep[WSpecialPredefCompanionCtor] = new WSpecialPredefCompanionCtor {
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

  registerModule(WSpecialPredefsModule)
}

object WSpecialPredefsModule extends scalan.ModuleInfo("scalan", "WSpecialPredefs")
}

trait WSpecialPredefsModule extends scalan.impl.WSpecialPredefsDefs {self: WrappersModule =>}
