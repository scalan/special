package wrappers.scala.util

import scalan._
import impl._
import special.wrappers.WrappersModule
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  // manual fix
  import special.wrappers.EitherWrapSpec

  // Abs -----------------------------------
trait WEithersDefs extends scalan.Scalan with WEithers {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WEither._

object WEither extends EntityObject("WEither") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WEitherConst[SA, SB, A, B](
        constValue: Either[SA, SB],
        lA: Liftable[SA, A], lB: Liftable[SB, B]
      ) extends WEither[A, B] with LiftedConst[Either[SA, SB], WEither[A, B]] {
    implicit def eA: Elem[A] = lA.eW
    implicit def eB: Elem[B] = lB.eW
    val liftable: Liftable[Either[SA, SB], WEither[A, B]] = liftableEither(lA,lB)
    val selfType: Elem[WEither[A, B]] = liftable.eW
    private val thisClass = classOf[WEither[A, B]]

    def fold[C](fa: Rep[A => C], fb: Rep[B => C]): Rep[C] = {
      implicit val eC = fa.elem.eRange
      asRep[C](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(fa, fb),
        true, isAdapterCall = false, element[C]))
    }
  }

  case class LiftableEither[SA, SB, A, B](lA: Liftable[SA, A],lB: Liftable[SB, B])
    extends Liftable[Either[SA, SB], WEither[A, B]] {
    lazy val eW: Elem[WEither[A, B]] = wEitherElement(lA.eW,lB.eW)
    lazy val sourceClassTag: ClassTag[Either[SA, SB]] = {
            implicit val tagSA = lA.eW.sourceClassTag.asInstanceOf[ClassTag[SA]]
      implicit val tagSB = lB.eW.sourceClassTag.asInstanceOf[ClassTag[SB]]
      classTag[Either[SA, SB]]
    }
    def lift(x: Either[SA, SB]): Rep[WEither[A, B]] = WEitherConst(x, lA,lB)
    def unlift(w: Rep[WEither[A, B]]): Either[SA, SB] = w match {
      case Def(WEitherConst(x: Either[_,_], _lA,_lB))
            if _lA == lA && _lB == lB => x.asInstanceOf[Either[SA, SB]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableEither[SA, SB, A, B](implicit lA: Liftable[SA,A],lB: Liftable[SB,B]): Liftable[Either[SA, SB], WEither[A, B]] =
    LiftableEither(lA,lB)

  private val _EitherWrapSpec = new EitherWrapSpec
  // entityAdapter for WEither trait
  case class WEitherAdapter[A, B](source: Rep[WEither[A, B]])
      extends WEither[A, B] with Def[WEither[A, B]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A];
implicit lazy val eB = source.elem.typeArgs("B")._1.asElem[B]
    val selfType: Elem[WEither[A, B]] = element[WEither[A, B]]
    private val thisClass = classOf[WEither[A, B]]

    def fold[C](fa: Rep[A => C], fb: Rep[B => C]): Rep[C] = {
      implicit val eC = fa.elem.eRange
      asRep[C](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(fa, fb),
        true, isAdapterCall = false, element[C]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWEither[A, B](p: Rep[WEither[A, B]]): WEither[A, B] = {
    if (p.rhs.isInstanceOf[WEither[A, B]@unchecked]) p.rhs.asInstanceOf[WEither[A, B]]
    else
      WEitherAdapter(p)
  }

  // familyElem
  class WEitherElem[A, B, To <: WEither[A, B]](implicit _eA: Elem[A], _eB: Elem[B])
    extends EntityElem[To] {
    def eA = _eA
    def eB = _eB

    override val liftable = liftableEither(_eA.liftable, _eB.liftable).asLiftable[Either[_,_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_EitherWrapSpec, classOf[WEither[A, B]], Set(
        "fold"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[WEither[A, B]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WEither[A, B]] => convertWEither(x) }
      tryConvert(element[WEither[A, B]], this, x, conv)
    }

    def convertWEither(x: Rep[WEither[A, B]]): Rep[To] = {
      x.elem match {
        case _: WEitherElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have WEitherElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wEitherElement[A, B](implicit eA: Elem[A], eB: Elem[B]): Elem[WEither[A, B]] =
    cachedElem[WEitherElem[A, B, WEither[A, B]]](eA, eB)

  implicit case object WEitherCompanionElem extends CompanionElem[WEitherCompanionCtor] {
    lazy val tag = weakTypeTag[WEitherCompanionCtor]
    protected def getDefaultRep = RWEither
  }

  abstract class WEitherCompanionCtor extends CompanionDef[WEitherCompanionCtor] with WEitherCompanion {
    def selfType = WEitherCompanionElem
    override def toString = "WEither"
  }
  implicit def proxyWEitherCompanionCtor(p: Rep[WEitherCompanionCtor]): WEitherCompanionCtor =
    proxyOps[WEitherCompanionCtor](p)

  lazy val RWEither: Rep[WEitherCompanionCtor] = new WEitherCompanionCtor {
    private val thisClass = classOf[WEitherCompanion]

    def cond[A, B](test: Rep[Boolean], right: Rep[Thunk[B]], left: Rep[Thunk[A]]): Rep[WEither[A, B]] = {
      implicit val eA = left.elem.eItem
implicit val eB = right.elem.eItem
      asRep[WEither[A, B]](mkMethodCall(self,
        thisClass.getMethod("cond", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(test, right, left),
        true, isAdapterCall = false, element[WEither[A, B]]))
    }
  }

  object WEitherMethods {
    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WEitherElem[_, _, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WEither[A, B]], Rep[A => C], Rep[B => C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WEitherCompanionMethods {
    object cond {
      def unapply(d: Def[_]): Nullable[(Rep[Boolean], Rep[Thunk[B]], Rep[Thunk[A]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WEitherCompanionElem && method.getName == "cond" =>
          val res = (args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[Boolean], Rep[Thunk[B]], Rep[Thunk[A]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Boolean], Rep[Thunk[B]], Rep[Thunk[A]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object WEither
  registerEntityObject("WEither", WEither)

  registerModule(WEithersModule)
}

object WEithersModule extends scalan.ModuleInfo("wrappers.scala.util", "WEithers")
}

trait WEithersModule extends wrappers.scala.util.impl.WEithersDefs {self: WrappersModule =>}
