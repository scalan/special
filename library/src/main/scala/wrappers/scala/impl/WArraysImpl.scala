package wrappers.scala

import scalan._
import impl._
import special.wrappers.WrappersModule
import special.wrappers.ArrayWrapSpec
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait WArraysDefs extends scalan.Scalan with WArrays {
  self: WrappersModule =>
import IsoUR._
import Converter._
import WArray._

object WArray extends EntityObject("WArray") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  case class WArrayConst[ST, T](
        constValue: Array[ST],
        lT: Liftable[ST, T]
      ) extends WArray[T] with LiftedConst[Array[ST], WArray[T]] {
    implicit def eT: Elem[T] = lT.eW
    val liftable: Liftable[Array[ST], WArray[T]] = liftableArray(lT)
    val selfType: Elem[WArray[T]] = liftable.eW
    private val thisClass = classOf[WArray[T]]

    def apply(i: Rep[Int]): Rep[T] = {
      asRep[T](mkMethodCall(self,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, false, element[T]))
    }

    def foreach(f: Rep[T => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(self,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, false, element[Unit]))
    }

    def exists(p: Rep[T => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    def forall(p: Rep[T => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    def filter(p: Rep[T => Boolean]): Rep[WArray[T]] = {
      asRep[WArray[T]](mkMethodCall(self,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, false, element[WArray[T]]))
    }

    def foldLeft[B](zero: Rep[B], op: Rep[((B, T)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]] = {
      asRep[WArray[T]](mkMethodCall(self,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, false, element[WArray[T]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("length"),
        List(),
        true, false, element[Int]))
    }

    def map[B](f: Rep[T => B]): Rep[WArray[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WArray[B]](mkMethodCall(self,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, false, element[WArray[B]]))
    }

    def zip[B](ys: Rep[WArray[B]]): Rep[WArray[(T, B)]] = {
      implicit val eB = ys.eT
      asRep[WArray[(T, B)]](mkMethodCall(self,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, false, element[WArray[(T, B)]]))
    }
  }

  case class LiftableArray[ST, T](lT: Liftable[ST, T])
    extends Liftable[Array[ST], WArray[T]] {
    lazy val eW: Elem[WArray[T]] = wArrayElement(lT.eW)
    lazy val sourceClassTag: ClassTag[Array[ST]] = {
            implicit val tagST = lT.eW.sourceClassTag.asInstanceOf[ClassTag[ST]]
      classTag[Array[ST]]
    }
    def lift(x: Array[ST]): Rep[WArray[T]] = WArrayConst(x, lT)
    def unlift(w: Rep[WArray[T]]): Array[ST] = w match {
      case Def(WArrayConst(x: Array[_], _lT))
            if _lT == lT => x.asInstanceOf[Array[ST]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableArray[ST, T](implicit lT: Liftable[ST,T]): Liftable[Array[ST], WArray[T]] =
    LiftableArray(lT)

  private val _ArrayWrapSpec = new ArrayWrapSpec
  // entityAdapter for WArray trait
  case class WArrayAdapter[T](source: Rep[WArray[T]])
      extends WArray[T] with Def[WArray[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]

    val selfType: Elem[WArray[T]] = element[WArray[T]]
    override def transform(t: Transformer) = WArrayAdapter[T](t(source))
    private val thisClass = classOf[WArray[T]]

    def apply(i: Rep[Int]): Rep[T] = {
      asRep[T](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[T]))
    }

    def foreach(f: Rep[T => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[T => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[T => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[T => Boolean]): Rep[WArray[T]] = {
      asRep[WArray[T]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[WArray[T]]))
    }

    def foldLeft[B](zero: Rep[B], op: Rep[((B, T)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[WArray[T]] = {
      asRep[WArray[T]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[WArray[T]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def map[B](f: Rep[T => B]): Rep[WArray[B]] = {
      implicit val eB = f.elem.eRange
      asRep[WArray[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[WArray[B]]))
    }

    def zip[B](ys: Rep[WArray[B]]): Rep[WArray[(T, B)]] = {
      implicit val eB = ys.eT
      asRep[WArray[(T, B)]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[WArray[(T, B)]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyWArray[T](p: Rep[WArray[T]]): WArray[T] = {
    if (p.rhs.isInstanceOf[WArray[T]@unchecked]) p.rhs.asInstanceOf[WArray[T]]
    else
      WArrayAdapter(p)
  }

  implicit def castWArrayElement[T](elem: Elem[WArray[T]]): WArrayElem[T, WArray[T]] =
    elem.asInstanceOf[WArrayElem[T, WArray[T]]]

  implicit lazy val containerWArray: Functor[WArray] = new Functor[WArray] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[WArray[A]]
    def lift[A](implicit evA: Elem[A]) = element[WArray[A]]
    def unlift[A](implicit eFT: Elem[WArray[A]]) =
      castWArrayElement(eFT).eT
    def getElem[A](fa: Rep[WArray[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: WArrayElem[_,_] => Some(e.asElem[WArray[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[WArray[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class WArrayIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, WArray] {
    lazy val selfType = new ConcreteIsoElem[WArray[A], WArray[B], WArrayIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[WArray[A], WArray[B]]]]
    def cC = container[WArray]
    def from(x: Rep[WArray[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[WArray[A]]) = x.map(innerIso.toFun)
  }

  def wArrayIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(WArrayIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, WArray]]

  // familyElem
  class WArrayElem[T, To <: WArray[T]](implicit _eT: Elem[T])
    extends EntityElem1[T, To, WArray](_eT, container[WArray]) {
    def eT = _eT

    override val liftable = liftableArray(_eT.liftable).asLiftable[Array[_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredWrapperMethods(_ArrayWrapSpec, classOf[WArray[T]], Set(
        "apply", "foreach", "exists", "forall", "filter", "foldLeft", "slice", "length", "map", "zip"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[WArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[WArray[T]] => convertWArray(x) }
      tryConvert(element[WArray[T]], this, x, conv)
    }

    def convertWArray(x: Rep[WArray[T]]): Rep[To] = {
      x.elem match {
        case _: WArrayElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have WArrayElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def wArrayElement[T](implicit eT: Elem[T]): Elem[WArray[T]] =
    cachedElem[WArrayElem[T, WArray[T]]](eT)

  implicit case object WArrayCompanionElem extends CompanionElem[WArrayCompanionCtor] {
    lazy val tag = weakTypeTag[WArrayCompanionCtor]
    protected def getDefaultRep = RWArray
  }

  abstract class WArrayCompanionCtor extends CompanionDef[WArrayCompanionCtor] with WArrayCompanion {
    def selfType = WArrayCompanionElem
    override def toString = "WArray"
  }
  implicit def proxyWArrayCompanionCtor(p: Rep[WArrayCompanionCtor]): WArrayCompanionCtor =
    proxyOps[WArrayCompanionCtor](p)

  lazy val RWArray: Rep[WArrayCompanionCtor] = new WArrayCompanionCtor {
    private val thisClass = classOf[WArrayCompanion]

    def fill[T](n: Rep[Int], elem: Rep[Thunk[T]]): Rep[WArray[T]] = {
      implicit val eT = elem.elem.eItem
      asRep[WArray[T]](mkMethodCall(self,
        thisClass.getMethod("fill", classOf[Sym], classOf[Sym]),
        List(n, elem),
        true, false, element[WArray[T]]))
    }
  }

  case class ViewWArray[A, B](source: Rep[WArray[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, WArray](wArrayIso(innerIso)) {
    override def toString = s"ViewWArray[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewWArray[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object WArrayMethods {
    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[T => Unit]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[T => Boolean]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foldLeft {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "foldLeft" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[B], Rep[((B, T)) => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[Int], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[WArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[WArray[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[WArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[T => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[WArrayElem[_, _]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[WArray[T]], Rep[WArray[B]]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object WArrayCompanionMethods {
    object fill {
      def unapply(d: Def[_]): Nullable[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem == WArrayCompanionElem && method.getName == "fill" =>
          val res = (args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Int], Rep[Thunk[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Int], Rep[Thunk[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object WArray
  registerEntityObject("WArray", WArray)

  object UserTypeWArray {
    def unapply(s: Sym): Option[Iso[_, _]] = {
      s.elem match {
        case e: WArrayElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewWArray[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeWArray(iso: Iso[a, b]) =>
      val newIso = wArrayIso(iso)
      val repr = reifyObject(UnpackView(asRep[WArray[b]](s), newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type RepWArray[T] = Rep[WArray[T]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewWArray(Def(view2@ViewWArray(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewWArray(arr, compIso)

    case WArrayMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepWArray[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = asRep[a => c](f)
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewWArray(s, iso)
        res
      case (HasViews(source, Def(contIso: WArrayIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = asRep[b => c](f)
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        asRep[WArray[a]](source).map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }

  registerModule(WArraysModule)
}

object WArraysModule extends scalan.ModuleInfo("wrappers.scala", "WArrays")
}

trait WArraysModule extends wrappers.scala.impl.WArraysDefs {self: WrappersModule =>}
