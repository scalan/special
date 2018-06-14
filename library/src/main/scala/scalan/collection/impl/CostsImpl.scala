package scalan.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CostsDefs extends scalan.Scalan with Costs {
  self: Library =>

  // entityProxy: single proxy for each type family
  implicit def proxyCosted[T](p: Rep[Costed[T]]): Costed[T] = {
    proxyOps[Costed[T]](p)(scala.reflect.classTag[Costed[T]])
  }

  // familyElem
  class CostedElem[T, To <: Costed[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[Costed[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Costed[T]] => convertCosted(x) }
      tryConvert(element[Costed[T]], this, x, conv)
    }

    def convertCosted(x: Rep[Costed[T]]): Rep[To] = {
      x.elem match {
        case _: CostedElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CostedElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedElement[T](implicit eT: Elem[T]): Elem[Costed[T]] =
    cachedElem[CostedElem[T, Costed[T]]](eT)

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor] {
    lazy val tag = weakTypeTag[CostedCompanionCtor]
    protected def getDefaultRep = Costed
  }

  abstract class CostedCompanionCtor extends CompanionDef[CostedCompanionCtor] with CostedCompanion {
    def selfType = CostedCompanionElem
    override def toString = "Costed"
  }
  implicit def proxyCostedCompanionCtor(p: Rep[CostedCompanionCtor]): CostedCompanionCtor =
    proxyOps[CostedCompanionCtor](p)

  lazy val Costed: Rep[CostedCompanionCtor] = new CostedCompanionCtor {
  }

  object CostedMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[Costed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Costed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[Costed[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedCompanionMethods {
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCostedBuilder(p: Rep[CostedBuilder]): CostedBuilder = {
    proxyOps[CostedBuilder](p)(scala.reflect.classTag[CostedBuilder])
  }

  // familyElem
  class CostedBuilderElem[To <: CostedBuilder]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[CostedBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[CostedBuilder] => convertCostedBuilder(x) }
      tryConvert(element[CostedBuilder], this, x, conv)
    }

    def convertCostedBuilder(x: Rep[CostedBuilder]): Rep[To] = {
      x.elem match {
        case _: CostedBuilderElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have CostedBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedBuilderElement: Elem[CostedBuilder] =
    cachedElem[CostedBuilderElem[CostedBuilder]]()

  implicit case object CostedBuilderCompanionElem extends CompanionElem[CostedBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CostedBuilderCompanionCtor]
    protected def getDefaultRep = CostedBuilder
  }

  abstract class CostedBuilderCompanionCtor extends CompanionDef[CostedBuilderCompanionCtor] with CostedBuilderCompanion {
    def selfType = CostedBuilderCompanionElem
    override def toString = "CostedBuilder"
  }
  implicit def proxyCostedBuilderCompanionCtor(p: Rep[CostedBuilderCompanionCtor]): CostedBuilderCompanionCtor =
    proxyOps[CostedBuilderCompanionCtor](p)

  lazy val CostedBuilder: Rep[CostedBuilderCompanionCtor] = new CostedBuilderCompanionCtor {
  }

  object CostedBuilderMethods {
  }

  object CostedBuilderCompanionMethods {
  }

  registerModule(CostsModule)
}

object CostsModule extends scalan.ModuleInfo("scalan.collection", "Costs")
}

trait CostsModule extends scalan.collection.impl.CostsDefs {self: Library =>}
