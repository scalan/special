package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait CostsDefs extends scalan.Scalan with Costs {
  self: Library =>
import IsoUR._
import Converter._
import CostedBuilder._
import Costed._

object Costed extends EntityObject("Costed") {
  // entityProxy: single proxy for each type family
  implicit def proxyCosted[Val](p: Rep[Costed[Val]]): Costed[Val] = {
    proxyOps[Costed[Val]](p)(scala.reflect.classTag[Costed[Val]])
  }

  // familyElem
  class CostedElem[Val, To <: Costed[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagVal = eVal.tag
      weakTypeTag[Costed[Val]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Costed[Val]] => convertCosted(x) }
      tryConvert(element[Costed[Val]], this, x, conv)
    }

    def convertCosted(x: Rep[Costed[Val]]): Rep[To] = {
      x.elem match {
        case _: CostedElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CostedElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def costedElement[Val](implicit eVal: Elem[Val]): Elem[Costed[Val]] =
    cachedElem[CostedElem[Val, Costed[Val]]](eVal)

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor] {
    lazy val tag = weakTypeTag[CostedCompanionCtor]
    protected def getDefaultRep = RCosted
  }

  abstract class CostedCompanionCtor extends CompanionDef[CostedCompanionCtor] with CostedCompanion {
    def selfType = CostedCompanionElem
    override def toString = "Costed"
  }
  implicit def proxyCostedCompanionCtor(p: Rep[CostedCompanionCtor]): CostedCompanionCtor =
    proxyOps[CostedCompanionCtor](p)

  lazy val RCosted: Rep[CostedCompanionCtor] = new CostedCompanionCtor {
  }

  object CostedMethods {
    object builder {
      def unapply(d: Def[_]): Option[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "builder" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object cost {
      def unapply(d: Def[_]): Option[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "cost" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Option[Rep[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedElem[_, _]] && method.getName == "dataSize" =>
          Some(receiver).asInstanceOf[Option[Rep[Costed[Val]] forSome {type Val}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[Costed[Val]] forSome {type Val}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedCompanionMethods {
  }
} // of object Costed
  registerEntityObject("Costed", Costed)

object CostedBuilder extends EntityObject("CostedBuilder") {
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
    protected def getDefaultRep = RCostedBuilder
  }

  abstract class CostedBuilderCompanionCtor extends CompanionDef[CostedBuilderCompanionCtor] with CostedBuilderCompanion {
    def selfType = CostedBuilderCompanionElem
    override def toString = "CostedBuilder"
  }
  implicit def proxyCostedBuilderCompanionCtor(p: Rep[CostedBuilderCompanionCtor]): CostedBuilderCompanionCtor =
    proxyOps[CostedBuilderCompanionCtor](p)

  lazy val RCostedBuilder: Rep[CostedBuilderCompanionCtor] = new CostedBuilderCompanionCtor {
  }

  object CostedBuilderMethods {
    object ConstructTupleCost {
      def unapply(d: Def[_]): Option[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "ConstructTupleCost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object ConstructSumCost {
      def unapply(d: Def[_]): Option[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "ConstructSumCost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object SelectFieldCost {
      def unapply(d: Def[_]): Option[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "SelectFieldCost" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object SumTagSize {
      def unapply(d: Def[_]): Option[Rep[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "SumTagSize" =>
          Some(receiver).asInstanceOf[Option[Rep[CostedBuilder]]]
        case _ => None
      }
      def unapply(exp: Sym): Option[Rep[CostedBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object costedValue {
      def unapply(d: Def[_]): Option[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(x, optCost, _*), _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "costedValue" =>
          Some((receiver, x, optCost)).asInstanceOf[Option[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CostedBuilder], Rep[T], Rep[WOption[Int]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object defaultValue {
      def unapply(d: Def[_]): Option[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(valueType, _*), _) if receiver.elem.isInstanceOf[CostedBuilderElem[_]] && method.getName == "defaultValue" =>
          Some((receiver, valueType)).asInstanceOf[Option[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Sym): Option[(Rep[CostedBuilder], Rep[WRType[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CostedBuilderCompanionMethods {
  }
} // of object CostedBuilder
  registerEntityObject("CostedBuilder", CostedBuilder)

  registerModule(CostsModule)
}

object CostsModule extends scalan.ModuleInfo("special.collection", "Costs")
}

trait CostsModule extends special.collection.impl.CostsDefs {self: Library =>}
