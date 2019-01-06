package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait ColsDefs extends scalan.Scalan with Cols {
  self: Library =>
import IsoUR._
import Converter._
import Col._
import ColBuilder._
import PairCol._
import WArray._
import ReplCol._

object Col extends EntityObject("Col") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SCol[A] = special.collection.Col[A]
  case class ColConst[SA, A](
        constValue: SCol[SA],
        lA: Liftable[SA, A]
      ) extends Col[A] with LiftedConst[SCol[SA], Col[A]] {
    implicit def eA: Elem[A] = lA.eW
    val liftable: Liftable[SCol[SA], Col[A]] = liftableCol(lA)
    val selfType: Elem[Col[A]] = liftable.eW
    private val thisClass = classOf[Col[A]]

    def builder: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(self,
        thisClass.getMethod("builder"),
        List(),
        true, false, element[ColBuilder]))
    }

    def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(self,
        thisClass.getMethod("arr"),
        List(),
        true, false, element[WArray[A]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("length"),
        List(),
        true, false, element[Int]))
    }

    def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, false, element[A]))
    }

    def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, false, element[A]))
    }

    def map[B](f: Rep[A => B]): Rep[Col[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Col[B]](mkMethodCall(self,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, false, element[Col[B]]))
    }

    def zip[B](ys: Rep[Col[B]]): Rep[PairCol[A, B]] = {
      implicit val eB = ys.eA
      asRep[PairCol[A, B]](mkMethodCall(self,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, false, element[PairCol[A, B]]))
    }

    def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(self,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, false, element[Unit]))
    }

    def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, false, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(self,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, false, element[Col[A]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, false, element[B]))
    }

    def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(self,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, false, element[A]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(self,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, false, element[Col[A]]))
    }

    def append(other: Rep[Col[A]]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(self,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, false, element[Col[A]]))
    }
  }

  case class LiftableCol[SA, A](lA: Liftable[SA, A])
    extends Liftable[SCol[SA], Col[A]] {
    lazy val eW: Elem[Col[A]] = colElement(lA.eW)
    lazy val sourceClassTag: ClassTag[SCol[SA]] = {
            implicit val tagSA = lA.eW.sourceClassTag.asInstanceOf[ClassTag[SA]]
      classTag[SCol[SA]]
    }
    def lift(x: SCol[SA]): Rep[Col[A]] = ColConst(x, lA)
    def unlift(w: Rep[Col[A]]): SCol[SA] = w match {
      case Def(ColConst(x: SCol[_], _lA))
            if _lA == lA => x.asInstanceOf[SCol[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableCol[SA, A](implicit lA: Liftable[SA,A]): Liftable[SCol[SA], Col[A]] =
    LiftableCol(lA)

  // entityAdapter for Col trait
  case class ColAdapter[A](source: Rep[Col[A]])
      extends Col[A] with Def[Col[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val selfType: Elem[Col[A]] = element[Col[A]]
    override def transform(t: Transformer) = ColAdapter[A](t(source))
    private val thisClass = classOf[Col[A]]

    def builder: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[ColBuilder]))
    }

    def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[A]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[A]))
    }

    def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[A]))
    }

    def map[B](f: Rep[A => B]): Rep[Col[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Col[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Col[B]]))
    }

    def zip[B](ys: Rep[Col[B]]): Rep[PairCol[A, B]] = {
      implicit val eB = ys.eA
      asRep[PairCol[A, B]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[PairCol[A, B]]))
    }

    def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Col[A]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[A]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Col[A]]))
    }

    def append(other: Rep[Col[A]]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Col[A]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyCol[A](p: Rep[Col[A]]): Col[A] = {
    if (p.rhs.isInstanceOf[Col[A]@unchecked]) p.rhs.asInstanceOf[Col[A]]
    else
      ColAdapter(p)
  }

  implicit def castColElement[A](elem: Elem[Col[A]]): ColElem[A, Col[A]] =
    elem.asInstanceOf[ColElem[A, Col[A]]]

  implicit lazy val containerCol: Functor[Col] = new Functor[Col] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[Col[A]]
    def lift[A](implicit evA: Elem[A]) = element[Col[A]]
    def unlift[A](implicit eFT: Elem[Col[A]]) =
      castColElement(eFT).eA
    def getElem[A](fa: Rep[Col[A]]) = fa.elem
    def unapply[T](e: Elem[_]) = e match {
      case e: ColElem[_,_] => Some(e.asElem[Col[T]])
      case _ => None
    }
    def map[A,B](xs: Rep[Col[A]])(f: Rep[A] => Rep[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  case class ColIso[A, B](innerIso: Iso[A, B]) extends Iso1UR[A, B, Col] {
    lazy val selfType = new ConcreteIsoElem[Col[A], Col[B], ColIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[IsoUR[Col[A], Col[B]]]]
    def cC = container[Col]
    def from(x: Rep[Col[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[Col[A]]) = x.map(innerIso.toFun)
  }

  def colIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(ColIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, Col]]

  // familyElem
  class ColElem[A, To <: Col[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, Col](_eA, container[Col]) {
    def eA = _eA

    override val liftable = liftableCol(_eA.liftable).asLiftable[SCol[_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Col[A]], classOf[SCol[_]], Set(
        "builder", "arr", "length", "apply", "getOrElse", "map", "zip", "foreach", "exists", "forall", "filter", "where", "fold", "sum", "slice", "append"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Col[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Col[A]] => convertCol(x) }
      tryConvert(element[Col[A]], this, x, conv)
    }

    def convertCol(x: Rep[Col[A]]): Rep[To] = {
      x.elem match {
        case _: ColElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have ColElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def colElement[A](implicit eA: Elem[A]): Elem[Col[A]] =
    cachedElem[ColElem[A, Col[A]]](eA)

  implicit case object ColCompanionElem extends CompanionElem[ColCompanionCtor] {
    lazy val tag = weakTypeTag[ColCompanionCtor]
    protected def getDefaultRep = RCol
  }

  abstract class ColCompanionCtor extends CompanionDef[ColCompanionCtor] with ColCompanion {
    def selfType = ColCompanionElem
    override def toString = "Col"
  }
  implicit def proxyColCompanionCtor(p: Rep[ColCompanionCtor]): ColCompanionCtor =
    proxyOps[ColCompanionCtor](p)

  lazy val RCol: Rep[ColCompanionCtor] = new ColCompanionCtor {
    private val thisClass = classOf[ColCompanion]
  }

  case class ViewCol[A, B](source: Rep[Col[A]], override val innerIso: Iso[A, B])
    extends View1[A, B, Col](colIso(innerIso)) {
    override def toString = s"ViewCol[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewCol[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  object ColMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[Col[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Col[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Col[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object arr {
      def unapply(d: Def[_]): Nullable[Rep[Col[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "arr" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Col[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Col[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[Col[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Col[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Col[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "apply" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Int], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "getOrElse" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Int], Rep[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Int], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "map" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "zip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object foreach {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => Unit]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "foreach" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => Unit]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => Unit]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "exists" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "forall" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "filter" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object where {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "where" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[A => Boolean]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fold {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "fold" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[B], Rep[((B, A)) => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "sum" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Monoid[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Int], Rep[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "slice" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Int], Rep[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Int], Rep[Int]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Rep[Col[A]], Rep[Col[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColElem[_, _]] && method.getName == "append" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Col[A]], Rep[Col[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Col[A]], Rep[Col[A]]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ColCompanionMethods {
  }
} // of object Col
  registerEntityObject("Col", Col)

  object UserTypeCol {
    def unapply(s: Sym): Option[Iso[_, _]] = {
      s.elem match {
        case e: ColElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewCol[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeCol(iso: Iso[a, b]) =>
      val newIso = colIso(iso)
      val repr = reifyObject(UnpackView(asRep[Col[b]](s), newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  type RepCol[A] = Rep[Col[A]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case view1@ViewCol(Def(view2@ViewCol(arr, innerIso2)), innerIso1) =>
      val compIso = composeIso(innerIso1, innerIso2)
      implicit val eAB = compIso.eTo
      ViewCol(arr, compIso)

    case ColMethods.map(xs, f) => (xs, f) match {
      case (_, Def(IdentityLambda())) =>
        xs
      case (xs: RepCol[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = asRep[a => c](f)
        implicit val eB = iso.eFrom
        val s = xs.map(f1 >> iso.fromFun)
        val res = ViewCol(s, iso)
        res
      case (HasViews(source, Def(contIso: ColIso[a, b])), f: RFunc[_, c]@unchecked) =>
        val f1 = asRep[b => c](f)
        val iso = contIso.innerIso
        implicit val eC = f1.elem.eRange
        asRep[Col[a]](source).map(iso.toFun >> f1)
      case _ =>
        super.rewriteDef(d)
    }
    case _ => super.rewriteDef(d)
  }

object PairCol extends EntityObject("PairCol") {
  // entityAdapter for PairCol trait
  case class PairColAdapter[L, R](source: Rep[PairCol[L, R]])
      extends PairCol[L, R] with Def[PairCol[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asElem[L];
implicit lazy val eR = source.elem.typeArgs("R")._1.asElem[R]
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val selfType: Elem[PairCol[L, R]] = element[PairCol[L, R]]
    override def transform(t: Transformer) = PairColAdapter[L, R](t(source))
    private val thisClass = classOf[PairCol[L, R]]

    def ls: Rep[Col[L]] = {
      asRep[Col[L]](mkMethodCall(source,
        thisClass.getMethod("ls"),
        List(),
        true, true, element[Col[L]]))
    }

    def rs: Rep[Col[R]] = {
      asRep[Col[R]](mkMethodCall(source,
        thisClass.getMethod("rs"),
        List(),
        true, true, element[Col[R]]))
    }

    def builder: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[ColBuilder]))
    }

    def arr: Rep[WArray[(L, R)]] = {
      asRep[WArray[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[(L, R)]]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def apply(i: Rep[Int]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[(L, R)]))
    }

    def getOrElse(i: Rep[Int], default: Rep[(L, R)]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[(L, R)]))
    }

    def map[B](f: Rep[((L, R)) => B]): Rep[Col[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Col[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Col[B]]))
    }

    def zip[B](ys: Rep[Col[B]]): Rep[PairCol[(L, R), B]] = {
      implicit val eB = ys.eA
      asRep[PairCol[(L, R), B]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[PairCol[(L, R), B]]))
    }

    def foreach(f: Rep[((L, R)) => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[((L, R)) => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[((L, R)) => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[((L, R)) => Boolean]): Rep[Col[(L, R)]] = {
      asRep[Col[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Col[(L, R)]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, (L, R))) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def sum(m: Rep[Monoid[(L, R)]]): Rep[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[(L, R)]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[(L, R)]] = {
      asRep[Col[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Col[(L, R)]]))
    }

    def append(other: Rep[Col[(L, R)]]): Rep[Col[(L, R)]] = {
      asRep[Col[(L, R)]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Col[(L, R)]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyPairCol[L, R](p: Rep[PairCol[L, R]]): PairCol[L, R] = {
    if (p.rhs.isInstanceOf[PairCol[L, R]@unchecked]) p.rhs.asInstanceOf[PairCol[L, R]]
    else
      PairColAdapter(p)
  }

  // familyElem
  class PairColElem[L, R, To <: PairCol[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends ColElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(colElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagL = eL.tag
      implicit val tagR = eR.tag
      weakTypeTag[PairCol[L, R]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[PairCol[L, R]] => convertPairCol(x) }
      tryConvert(element[PairCol[L, R]], this, x, conv)
    }

    def convertPairCol(x: Rep[PairCol[L, R]]): Rep[To] = {
      x.elem match {
        case _: PairColElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have PairColElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def pairColElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[PairCol[L, R]] =
    cachedElem[PairColElem[L, R, PairCol[L, R]]](eL, eR)

  implicit case object PairColCompanionElem extends CompanionElem[PairColCompanionCtor] {
    lazy val tag = weakTypeTag[PairColCompanionCtor]
    protected def getDefaultRep = RPairCol
  }

  abstract class PairColCompanionCtor extends CompanionDef[PairColCompanionCtor] with PairColCompanion {
    def selfType = PairColCompanionElem
    override def toString = "PairCol"
  }
  implicit def proxyPairColCompanionCtor(p: Rep[PairColCompanionCtor]): PairColCompanionCtor =
    proxyOps[PairColCompanionCtor](p)

  lazy val RPairCol: Rep[PairColCompanionCtor] = new PairColCompanionCtor {
    private val thisClass = classOf[PairColCompanion]
  }

  object PairColMethods {
    object ls {
      def unapply(d: Def[_]): Nullable[Rep[PairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairColElem[_, _, _]] && method.getName == "ls" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object rs {
      def unapply(d: Def[_]): Nullable[Rep[PairCol[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairColElem[_, _, _]] && method.getName == "rs" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[PairCol[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[PairCol[L, R]] forSome {type L; type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object PairColCompanionMethods {
  }
} // of object PairCol
  registerEntityObject("PairCol", PairCol)

object ReplCol extends EntityObject("ReplCol") {
  // entityAdapter for ReplCol trait
  case class ReplColAdapter[A](source: Rep[ReplCol[A]])
      extends ReplCol[A] with Def[ReplCol[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A]

    val selfType: Elem[ReplCol[A]] = element[ReplCol[A]]
    override def transform(t: Transformer) = ReplColAdapter[A](t(source))
    private val thisClass = classOf[ReplCol[A]]

    def value: Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("value"),
        List(),
        true, true, element[A]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("length"),
        List(),
        true, true, element[Int]))
    }

    def builder: Rep[ColBuilder] = {
      asRep[ColBuilder](mkMethodCall(source,
        thisClass.getMethod("builder"),
        List(),
        true, true, element[ColBuilder]))
    }

    def arr: Rep[WArray[A]] = {
      asRep[WArray[A]](mkMethodCall(source,
        thisClass.getMethod("arr"),
        List(),
        true, true, element[WArray[A]]))
    }

    def apply(i: Rep[Int]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("apply", classOf[Sym]),
        List(i),
        true, true, element[A]))
    }

    def getOrElse(i: Rep[Int], default: Rep[A]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        List(i, default),
        true, true, element[A]))
    }

    def map[B](f: Rep[A => B]): Rep[Col[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Col[B]](mkMethodCall(source,
        thisClass.getMethod("map", classOf[Sym]),
        List(f),
        true, true, element[Col[B]]))
    }

    def zip[B](ys: Rep[Col[B]]): Rep[PairCol[A, B]] = {
      implicit val eB = ys.eA
      asRep[PairCol[A, B]](mkMethodCall(source,
        thisClass.getMethod("zip", classOf[Sym]),
        List(ys),
        true, true, element[PairCol[A, B]]))
    }

    def foreach(f: Rep[A => Unit]): Rep[Unit] = {
      asRep[Unit](mkMethodCall(source,
        thisClass.getMethod("foreach", classOf[Sym]),
        List(f),
        true, true, element[Unit]))
    }

    def exists(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("exists", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def forall(p: Rep[A => Boolean]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("forall", classOf[Sym]),
        List(p),
        true, true, element[Boolean]))
    }

    def filter(p: Rep[A => Boolean]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("filter", classOf[Sym]),
        List(p),
        true, true, element[Col[A]]))
    }

    def fold[B](zero: Rep[B], op: Rep[((B, A)) => B]): Rep[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("fold", classOf[Sym], classOf[Sym]),
        List(zero, op),
        true, true, element[B]))
    }

    def sum(m: Rep[Monoid[A]]): Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("sum", classOf[Sym]),
        List(m),
        true, true, element[A]))
    }

    def slice(from: Rep[Int], until: Rep[Int]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        List(from, until),
        true, true, element[Col[A]]))
    }

    def append(other: Rep[Col[A]]): Rep[Col[A]] = {
      asRep[Col[A]](mkMethodCall(source,
        thisClass.getMethod("append", classOf[Sym]),
        List(other),
        true, true, element[Col[A]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyReplCol[A](p: Rep[ReplCol[A]]): ReplCol[A] = {
    if (p.rhs.isInstanceOf[ReplCol[A]@unchecked]) p.rhs.asInstanceOf[ReplCol[A]]
    else
      ReplColAdapter(p)
  }

  // familyElem
  class ReplColElem[A, To <: ReplCol[A]](implicit _eA: Elem[A])
    extends ColElem[A, To] {
    override def eA = _eA

    override lazy val parent: Option[Elem[_]] = Some(colElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[ReplCol[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ReplCol[A]] => convertReplCol(x) }
      tryConvert(element[ReplCol[A]], this, x, conv)
    }

    def convertReplCol(x: Rep[ReplCol[A]]): Rep[To] = {
      x.elem match {
        case _: ReplColElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have ReplColElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def replColElement[A](implicit eA: Elem[A]): Elem[ReplCol[A]] =
    cachedElem[ReplColElem[A, ReplCol[A]]](eA)

  implicit case object ReplColCompanionElem extends CompanionElem[ReplColCompanionCtor] {
    lazy val tag = weakTypeTag[ReplColCompanionCtor]
    protected def getDefaultRep = RReplCol
  }

  abstract class ReplColCompanionCtor extends CompanionDef[ReplColCompanionCtor] with ReplColCompanion {
    def selfType = ReplColCompanionElem
    override def toString = "ReplCol"
  }
  implicit def proxyReplColCompanionCtor(p: Rep[ReplColCompanionCtor]): ReplColCompanionCtor =
    proxyOps[ReplColCompanionCtor](p)

  lazy val RReplCol: Rep[ReplColCompanionCtor] = new ReplColCompanionCtor {
    private val thisClass = classOf[ReplColCompanion]
  }

  object ReplColMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Rep[ReplCol[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReplColElem[_, _]] && method.getName == "value" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ReplCol[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ReplCol[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[ReplCol[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReplColElem[_, _]] && method.getName == "length" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ReplCol[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ReplCol[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ReplColCompanionMethods {
  }
} // of object ReplCol
  registerEntityObject("ReplCol", ReplCol)

object ColBuilder extends EntityObject("ColBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SColBuilder = special.collection.ColBuilder
  case class ColBuilderConst(
        constValue: SColBuilder
      ) extends ColBuilder with LiftedConst[SColBuilder, ColBuilder] {
    val liftable: Liftable[SColBuilder, ColBuilder] = LiftableColBuilder
    val selfType: Elem[ColBuilder] = liftable.eW
    private val thisClass = classOf[ColBuilder]

    def pairCol[A, B](as: Rep[Col[A]], bs: Rep[Col[B]]): Rep[PairCol[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairCol[A, B]](mkMethodCall(self,
        thisClass.getMethod("pairCol", classOf[Sym], classOf[Sym]),
        List(as, bs),
        true, false, element[PairCol[A, B]]))
    }

    def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Col[T]] = {
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, false, element[Col[T]]))
    }

    def xor(left: Rep[Col[Byte]], right: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(self,
        thisClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, false, element[Col[Byte]]))
    }

    def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = {
      implicit val eT = arr.eT
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, false, element[Col[T]]))
    }

    def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = {
      implicit val eT = v.elem
      asRep[Col[T]](mkMethodCall(self,
        thisClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, false, element[Col[T]]))
    }
  }

  implicit object LiftableColBuilder
    extends Liftable[SColBuilder, ColBuilder] {
    lazy val eW: Elem[ColBuilder] = colBuilderElement
    lazy val sourceClassTag: ClassTag[SColBuilder] = {
      classTag[SColBuilder]
    }
    def lift(x: SColBuilder): Rep[ColBuilder] = ColBuilderConst(x)
    def unlift(w: Rep[ColBuilder]): SColBuilder = w match {
      case Def(ColBuilderConst(x: SColBuilder))
            => x.asInstanceOf[SColBuilder]
      case _ => unliftError(w)
    }
  }

  // entityAdapter for ColBuilder trait
  case class ColBuilderAdapter(source: Rep[ColBuilder])
      extends ColBuilder with Def[ColBuilder] {
    val selfType: Elem[ColBuilder] = element[ColBuilder]
    override def transform(t: Transformer) = ColBuilderAdapter(t(source))
    private val thisClass = classOf[ColBuilder]

    def pairCol[A, B](as: Rep[Col[A]], bs: Rep[Col[B]]): Rep[PairCol[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairCol[A, B]](mkMethodCall(source,
        thisClass.getMethod("pairCol", classOf[Sym], classOf[Sym]),
        List(as, bs),
        true, true, element[PairCol[A, B]]))
    }

    def fromItems[T](items: Rep[T]*)(implicit cT: Elem[T]): Rep[Col[T]] = {
      asRep[Col[T]](mkMethodCall(source,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cT),
        true, true, element[Col[T]]))
    }

    def xor(left: Rep[Col[Byte]], right: Rep[Col[Byte]]): Rep[Col[Byte]] = {
      asRep[Col[Byte]](mkMethodCall(source,
        thisClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        List(left, right),
        true, true, element[Col[Byte]]))
    }

    def fromArray[T](arr: Rep[WArray[T]]): Rep[Col[T]] = {
      implicit val eT = arr.eT
      asRep[Col[T]](mkMethodCall(source,
        thisClass.getMethod("fromArray", classOf[Sym]),
        List(arr),
        true, true, element[Col[T]]))
    }

    def replicate[T](n: Rep[Int], v: Rep[T]): Rep[Col[T]] = {
      implicit val eT = v.elem
      asRep[Col[T]](mkMethodCall(source,
        thisClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        List(n, v),
        true, true, element[Col[T]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyColBuilder(p: Rep[ColBuilder]): ColBuilder = {
    if (p.rhs.isInstanceOf[ColBuilder@unchecked]) p.rhs.asInstanceOf[ColBuilder]
    else
      ColBuilderAdapter(p)
  }

  // familyElem
  class ColBuilderElem[To <: ColBuilder]
    extends EntityElem[To] {
    override val liftable = LiftableColBuilder.asLiftable[SColBuilder, To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[ColBuilder], classOf[SColBuilder], Set(
        "pairCol", "fromItems", "unzip", "xor", "fromArray", "replicate"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[ColBuilder].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[ColBuilder] => convertColBuilder(x) }
      tryConvert(element[ColBuilder], this, x, conv)
    }

    def convertColBuilder(x: Rep[ColBuilder]): Rep[To] = {
      x.elem match {
        case _: ColBuilderElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have ColBuilderElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val colBuilderElement: Elem[ColBuilder] =
    new ColBuilderElem[ColBuilder]

  implicit case object ColBuilderCompanionElem extends CompanionElem[ColBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[ColBuilderCompanionCtor]
    protected def getDefaultRep = RColBuilder
  }

  abstract class ColBuilderCompanionCtor extends CompanionDef[ColBuilderCompanionCtor] with ColBuilderCompanion {
    def selfType = ColBuilderCompanionElem
    override def toString = "ColBuilder"
  }
  implicit def proxyColBuilderCompanionCtor(p: Rep[ColBuilderCompanionCtor]): ColBuilderCompanionCtor =
    proxyOps[ColBuilderCompanionCtor](p)

  lazy val RColBuilder: Rep[ColBuilderCompanionCtor] = new ColBuilderCompanionCtor {
    private val thisClass = classOf[ColBuilderCompanion]
  }

  object ColBuilderMethods {
    object pairCol {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "pairCol" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Rep[Col[A]], Rep[Col[B]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Seq[Rep[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object unzip {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Rep[Col[(A, B)]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "unzip" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Rep[Col[(A, B)]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Rep[Col[(A, B)]]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Rep[Col[Byte]], Rep[Col[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "xor" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Rep[Col[Byte]], Rep[Col[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Rep[Col[Byte]], Rep[Col[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Rep[WArray[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "fromArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Rep[WArray[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Rep[WArray[T]]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Rep[ColBuilder], Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ColBuilderElem[_]] && method.getName == "replicate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[ColBuilder], Rep[Int], Rep[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ColBuilder], Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ColBuilderCompanionMethods {
  }
} // of object ColBuilder
  registerEntityObject("ColBuilder", ColBuilder)

  registerModule(ColsModule)
}

object ColsModule extends scalan.ModuleInfo("special.collection", "Cols")
}

trait ColsModule extends special.collection.impl.ColsDefs {self: Library =>}
