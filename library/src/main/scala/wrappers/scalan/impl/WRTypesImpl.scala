package wrappers.scalan

import scalan._
import impl._
import scalan.RType
import special.wrappers.WrappersModule
import special.wrappers.RTypeWrapSpec
import scala.collection.mutable.WrappedArray
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WRTypesDefs extends scalan.Scalan with WRTypes {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WRType._

object WRType extends EntityObject("WRType") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WRTypeConst[SA, A](
        constValue: RType[SA],
        lA: Liftable[SA, A]
      ) extends WRType[A] with LiftedConst[RType[SA], WRType[A]]
        with Def[WRType[A]] with WRTypeConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[RType[SA], WRType[A]] = liftableRType(lA)
    val resultType: Elem[WRType[A]] = liftable.eW
  }

  trait WRTypeConstMethods[A] extends WRType[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val WRTypeClass = classOf[WRType[A]]

    override def name: Rep[String] = {
      asRep[String](mkMethodCall(self,
        WRTypeClass.getMethod("name"),
        WrappedArray.empty,
        true, false, element[String]))
    }
  }

  case class LiftableRType[SA, A](lA: Liftable[SA, A])
    extends Liftable[RType[SA], WRType[A]] {
    lazy val eW: Elem[WRType[A]] = wRTypeElement(lA.eW)
    lazy val sourceType: RType[RType[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[RType[SA]]
    }
    def lift(x: RType[SA]): Rep[WRType[A]] = WRTypeConst(x, lA)
    def unlift(w: Rep[WRType[A]]): RType[SA] = w match {
      case Def(WRTypeConst(x: RType[_], _lA))
            if _lA == lA => x.asInstanceOf[RType[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableRType[SA, A](implicit lA: Liftable[SA,A]): Liftable[RType[SA], WRType[A]] =
    LiftableRType(lA)

  private val _RTypeWrapSpec = new RTypeWrapSpec {}

  private val WRTypeClass = classOf[WRType[_]]

  // entityAdapter for WRType trait
  case class WRTypeAdapter[A](source: Rep[WRType[A]])
      extends WRType[A]
      with Def[WRType[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val resultType: Elem[WRType[A]] = element[WRType[A]]
    override def transform(t: Transformer) = WRTypeAdapter[A](t(source))

    def name: Rep[String] = {
      asRep[String](mkMethodCall(source,
        WRTypeClass.getMethod("name"),
        WrappedArray.empty,
        true, true, element[String]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWRType[A](p: Rep[WRType[A]]): WRType[A] = {
    if (p.rhs.isInstanceOf[WRType[A]@unchecked]) p.rhs.asInstanceOf[WRType[A]]
    else
      WRTypeAdapter(p)
  }

  // familyElem
  class WRTypeElem[A, To <: WRType[A]](implicit _eA: Elem[A])
    extends EntityElem[To] {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[RType[_], To](liftableRType(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_RTypeWrapSpec, classOf[WRType[A]], Set(
        "name"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  implicit def wRTypeElement[A](implicit eA: Elem[A]): Elem[WRType[A]] =
    cachedElemByClass(eA)(classOf[WRTypeElem[A, WRType[A]]])

  implicit case object WRTypeCompanionElem extends CompanionElem[WRTypeCompanionCtor]

  abstract class WRTypeCompanionCtor extends CompanionDef[WRTypeCompanionCtor] with WRTypeCompanion {
    def resultType = WRTypeCompanionElem
    override def toString = "WRType"
  }
  implicit def proxyWRTypeCompanionCtor(p: Rep[WRTypeCompanionCtor]): WRTypeCompanionCtor =
    p.rhs.asInstanceOf[WRTypeCompanionCtor]

  lazy val RWRType: Rep[WRTypeCompanionCtor] = new WRTypeCompanionCtor {
    private val thisClass = classOf[WRTypeCompanion]
  }

  object WRTypeMethods {
    object name {
      def unapply(d: Def[_]): Nullable[Rep[WRType[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "name" && receiver.elem.isInstanceOf[WRTypeElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WRType[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WRType[A]] forSome {type A}] = unapply(exp.rhs)
    }
  }

  object WRTypeCompanionMethods {
  }
} // of object WRType
  registerEntityObject("WRType", WRType)

  registerModule(WRTypesModule)
}

object WRTypesModule extends scalan.ModuleInfo("wrappers.scalan", "WRTypes")
}

trait WRTypesModule extends wrappers.scalan.impl.WRTypesDefs {self: WrappersModule =>}
