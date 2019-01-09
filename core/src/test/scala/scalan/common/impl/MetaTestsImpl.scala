package scalan.common

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MetaTestsDefs extends scalan.Scalan with MetaTests {
  self: MetaTestsModule =>
import IsoUR._
import Converter._
import MT0._
import MT1._
import MT2._
import MetaPair._
import MetaTest._

object MetaTest extends EntityObject("MetaTest") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SMetaTest[T] = scalan.common.MetaTest[T]
  case class MetaTestConst[ST, T](
        constValue: SMetaTest[ST],
        lT: Liftable[ST, T]
      ) extends MetaTest[T] with LiftedConst[SMetaTest[ST], MetaTest[T]]
        with Def[MetaTest[T]] with MetaTestConstMethods[T] {
    implicit def eT: Elem[T] = lT.eW

    val liftable: Liftable[SMetaTest[ST], MetaTest[T]] = liftableMetaTest(lT)
    val selfType: Elem[MetaTest[T]] = liftable.eW
  }

  trait MetaTestConstMethods[T] extends MetaTest[T]  { thisConst: Def[_] =>
    implicit def eT: Elem[T]
    private val MetaTestClass = classOf[MetaTest[T]]

    override def test: RMetaTest[T] = {
      asRep[MetaTest[T]](mkMethodCall(self,
        MetaTestClass.getMethod("test"),
        List(),
        true, false, element[MetaTest[T]]))
    }

    override def give: Rep[T] = {
      asRep[T](mkMethodCall(self,
        MetaTestClass.getMethod("give"),
        List(),
        true, false, element[T]))
    }

    override def size: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        MetaTestClass.getMethod("size"),
        List(),
        true, false, element[Int]))
    }

    override def fromItems[B](items: Rep[B]*)(implicit cB: Elem[B]): Rep[MetaTest[B]] = {
      asRep[MetaTest[B]](mkMethodCall(self,
        MetaTestClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cB),
        true, false, element[MetaTest[B]]))
    }
  }

  case class LiftableMetaTest[ST, T](lT: Liftable[ST, T])
    extends Liftable[SMetaTest[ST], MetaTest[T]] {
    lazy val eW: Elem[MetaTest[T]] = metaTestElement(lT.eW)
    lazy val sourceClassTag: ClassTag[SMetaTest[ST]] = {
            implicit val tagST = lT.eW.sourceClassTag.asInstanceOf[ClassTag[ST]]
      classTag[SMetaTest[ST]]
    }
    def lift(x: SMetaTest[ST]): Rep[MetaTest[T]] = MetaTestConst(x, lT)
    def unlift(w: Rep[MetaTest[T]]): SMetaTest[ST] = w match {
      case Def(MetaTestConst(x: SMetaTest[_], _lT))
            if _lT == lT => x.asInstanceOf[SMetaTest[ST]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableMetaTest[ST, T](implicit lT: Liftable[ST,T]): Liftable[SMetaTest[ST], MetaTest[T]] =
    LiftableMetaTest(lT)

  // entityAdapter for MetaTest trait
  case class MetaTestAdapter[T](source: Rep[MetaTest[T]])
      extends MetaTest[T] with Def[MetaTest[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asElem[T]

    val selfType: Elem[MetaTest[T]] = element[MetaTest[T]]
    override def transform(t: Transformer) = MetaTestAdapter[T](t(source))
    private val thisClass = classOf[MetaTest[T]]

    def test: RMetaTest[T] = {
      asRep[MetaTest[T]](mkMethodCall(source,
        thisClass.getMethod("test"),
        List(),
        true, true, element[MetaTest[T]]))
    }

    def give: Rep[T] = {
      asRep[T](mkMethodCall(source,
        thisClass.getMethod("give"),
        List(),
        true, true, element[T]))
    }

    def size: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Int]))
    }

    def fromItems[B](items: Rep[B]*)(implicit cB: Elem[B]): Rep[MetaTest[B]] = {
      asRep[MetaTest[B]](mkMethodCall(source,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cB),
        true, true, element[MetaTest[B]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyMetaTest[T](p: Rep[MetaTest[T]]): MetaTest[T] = {
    if (p.rhs.isInstanceOf[MetaTest[T]@unchecked]) p.rhs.asInstanceOf[MetaTest[T]]
    else
      MetaTestAdapter(p)
  }

  // familyElem
  class MetaTestElem[T, To <: MetaTest[T]](implicit _eT: Elem[T])
    extends EntityElem[To] {
    def eT = _eT

    override val liftable: Liftables.Liftable[_, To] = liftableMetaTest(_eT.liftable).asLiftable[SMetaTest[_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[MetaTest[T]], classOf[SMetaTest[_]], Set(
        "test", "give", "size", "fromItems"
        ))
    }

    lazy val parent: Option[Elem[_]] = None
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MetaTest[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[MetaTest[T]] => convertMetaTest(x) }
      tryConvert(element[MetaTest[T]], this, x, conv)
    }

    def convertMetaTest(x: Rep[MetaTest[T]]): Rep[To] = {
      x.elem match {
        case _: MetaTestElem[_, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have MetaTestElem[_, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def metaTestElement[T](implicit eT: Elem[T]): Elem[MetaTest[T]] =
    cachedElem[MetaTestElem[T, MetaTest[T]]](eT)

  implicit case object MetaTestCompanionElem extends CompanionElem[MetaTestCompanionCtor] {
    lazy val tag = weakTypeTag[MetaTestCompanionCtor]
    protected def getDefaultRep = RMetaTest
  }

  abstract class MetaTestCompanionCtor extends CompanionDef[MetaTestCompanionCtor] with MetaTestCompanion {
    def selfType = MetaTestCompanionElem
    override def toString = "MetaTest"
  }
  implicit def proxyMetaTestCompanionCtor(p: Rep[MetaTestCompanionCtor]): MetaTestCompanionCtor =
    proxyOps[MetaTestCompanionCtor](p)

  lazy val RMetaTest: Rep[MetaTestCompanionCtor] = new MetaTestCompanionCtor {
    private val thisClass = classOf[MetaTestCompanion]
  }

  object MetaTestMethods {
    object test {
      def unapply(d: Def[_]): Nullable[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "test" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaTest[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object give {
      def unapply(d: Def[_]): Nullable[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "give" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaTest[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object size {
      def unapply(d: Def[_]): Nullable[Rep[MetaTest[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "size" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaTest[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaTest[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[MetaTest[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[MetaTestElem[_, _]] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[MetaTest[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[MetaTest[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object MetaTestCompanionMethods {
  }
} // of object MetaTest
  registerEntityObject("MetaTest", MetaTest)

object MetaPair extends EntityObject("MetaPair") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SMetaPair[A, B] = scalan.common.MetaPair[A, B]
  case class MetaPairConst[SA, SB, A, B](
        constValue: SMetaPair[SA, SB],
        lA: Liftable[SA, A], lB: Liftable[SB, B]
      ) extends MetaPair[A, B] with LiftedConst[SMetaPair[SA, SB], MetaPair[A, B]]
        with Def[MetaPair[A, B]] with MetaPairConstMethods[A, B] {
    implicit def eA: Elem[A] = lA.eW
    implicit def eB: Elem[B] = lB.eW
    implicit def eT: Elem[(A, B)] = element[(A, B)]

    val liftable: Liftable[SMetaPair[SA, SB], MetaPair[A, B]] = liftableMetaPair(lA,lB)
    val selfType: Elem[MetaPair[A, B]] = liftable.eW
  }

  trait MetaPairConstMethods[A, B] extends MetaPair[A, B] with MetaTestConstMethods[(A, B)] { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    implicit def eB: Elem[B]
    private val MetaPairClass = classOf[MetaPair[A, B]]

    override def indices: Rep[A] = {
      asRep[A](mkMethodCall(self,
        MetaPairClass.getMethod("indices"),
        List(),
        true, false, element[A]))
    }

    override def values: Rep[B] = {
      asRep[B](mkMethodCall(self,
        MetaPairClass.getMethod("values"),
        List(),
        true, false, element[B]))
    }

    override def give: Rep[(A, B)] = {
      asRep[(A, B)](mkMethodCall(self,
        MetaPairClass.getMethod("give"),
        List(),
        true, false, element[(A, B)]))
    }
  }

  case class LiftableMetaPair[SA, SB, A, B](lA: Liftable[SA, A],lB: Liftable[SB, B])
    extends Liftable[SMetaPair[SA, SB], MetaPair[A, B]] {
    lazy val eW: Elem[MetaPair[A, B]] = metaPairElement(lA.eW,lB.eW)
    lazy val sourceClassTag: ClassTag[SMetaPair[SA, SB]] = {
            implicit val tagSA = lA.eW.sourceClassTag.asInstanceOf[ClassTag[SA]]
      implicit val tagSB = lB.eW.sourceClassTag.asInstanceOf[ClassTag[SB]]
      classTag[SMetaPair[SA, SB]]
    }
    def lift(x: SMetaPair[SA, SB]): Rep[MetaPair[A, B]] = MetaPairConst(x, lA,lB)
    def unlift(w: Rep[MetaPair[A, B]]): SMetaPair[SA, SB] = w match {
      case Def(MetaPairConst(x: SMetaPair[_,_], _lA,_lB))
            if _lA == lA && _lB == lB => x.asInstanceOf[SMetaPair[SA, SB]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableMetaPair[SA, SB, A, B](implicit lA: Liftable[SA,A],lB: Liftable[SB,B]): Liftable[SMetaPair[SA, SB], MetaPair[A, B]] =
    LiftableMetaPair(lA,lB)

  // entityAdapter for MetaPair trait
  case class MetaPairAdapter[A, B](source: Rep[MetaPair[A, B]])
      extends MetaPair[A, B] with Def[MetaPair[A, B]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asElem[A];
implicit lazy val eB = source.elem.typeArgs("B")._1.asElem[B]
    override lazy val eT: Elem[(A, B)] = implicitly[Elem[(A, B)]]
    val selfType: Elem[MetaPair[A, B]] = element[MetaPair[A, B]]
    override def transform(t: Transformer) = MetaPairAdapter[A, B](t(source))
    private val thisClass = classOf[MetaPair[A, B]]

    def indices: Rep[A] = {
      asRep[A](mkMethodCall(source,
        thisClass.getMethod("indices"),
        List(),
        true, true, element[A]))
    }

    def values: Rep[B] = {
      asRep[B](mkMethodCall(source,
        thisClass.getMethod("values"),
        List(),
        true, true, element[B]))
    }

    def give: Rep[(A, B)] = {
      asRep[(A, B)](mkMethodCall(source,
        thisClass.getMethod("give"),
        List(),
        true, true, element[(A, B)]))
    }

    def test: RMetaTest[(A, B)] = {
      asRep[MetaTest[(A, B)]](mkMethodCall(source,
        thisClass.getMethod("test"),
        List(),
        true, true, element[MetaTest[(A, B)]]))
    }

    def size: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        thisClass.getMethod("size"),
        List(),
        true, true, element[Int]))
    }

    def fromItems[B1](items: Rep[B1]*)(implicit cB: Elem[B1]): Rep[MetaTest[B1]] = {
      asRep[MetaTest[B1]](mkMethodCall(source,
        thisClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        List(items, cB),
        true, true, element[MetaTest[B1]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyMetaPair[A, B](p: Rep[MetaPair[A, B]]): MetaPair[A, B] = {
    if (p.rhs.isInstanceOf[MetaPair[A, B]@unchecked]) p.rhs.asInstanceOf[MetaPair[A, B]]
    else
      MetaPairAdapter(p)
  }

  // familyElem
  class MetaPairElem[A, B, To <: MetaPair[A, B]](implicit _eA: Elem[A], _eB: Elem[B])
    extends MetaTestElem[(A, B), To] {
    def eA = _eA
    def eB = _eB

    override val liftable: Liftables.Liftable[_, To] = liftableMetaPair(_eA.liftable, _eB.liftable).asLiftable[SMetaPair[_,_], To]

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[MetaPair[A, B]], classOf[SMetaPair[_,_]], Set(
        "indices", "values", "give"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(pairElement(element[A],element[B])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[MetaPair[A, B]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[MetaPair[A, B]] => convertMetaPair(x) }
      tryConvert(element[MetaPair[A, B]], this, x, conv)
    }

    def convertMetaPair(x: Rep[MetaPair[A, B]]): Rep[To] = {
      x.elem match {
        case _: MetaPairElem[_, _, _] => asRep[To](x)
        case e => !!!(s"Expected $x to have MetaPairElem[_, _, _], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def metaPairElement[A, B](implicit eA: Elem[A], eB: Elem[B]): Elem[MetaPair[A, B]] =
    cachedElem[MetaPairElem[A, B, MetaPair[A, B]]](eA, eB)

  implicit case object MetaPairCompanionElem extends CompanionElem[MetaPairCompanionCtor] {
    lazy val tag = weakTypeTag[MetaPairCompanionCtor]
    protected def getDefaultRep = RMetaPair
  }

  abstract class MetaPairCompanionCtor extends CompanionDef[MetaPairCompanionCtor] {
    def selfType = MetaPairCompanionElem
    override def toString = "MetaPair"
  }
  implicit def proxyMetaPairCompanionCtor(p: Rep[MetaPairCompanionCtor]): MetaPairCompanionCtor =
    proxyOps[MetaPairCompanionCtor](p)

  lazy val RMetaPair: Rep[MetaPairCompanionCtor] = new MetaPairCompanionCtor {
  }

  object MetaPairMethods {
    object indices {
      def unapply(d: Def[_]): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaPairElem[_, _, _]] && method.getName == "indices" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object values {
      def unapply(d: Def[_]): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaPairElem[_, _, _]] && method.getName == "values" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object give {
      def unapply(d: Def[_]): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MetaPairElem[_, _, _]] && method.getName == "give" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MetaPair[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object MetaPair
  registerEntityObject("MetaPair", MetaPair)

object MT0 extends EntityObject("MT0") {
  case class MT0Ctor
      (override val size: Rep[Int])
    extends MT0(size) with Def[MT0] {
    lazy val selfType = element[MT0]
    override def transform(t: Transformer) = MT0Ctor(t(size))
  }
  // elem for concrete class
  class MT0Elem(val iso: Iso[MT0Data, MT0])
    extends MetaTestElem[Unit, MT0]
    with ConcreteElem[MT0Data, MT0] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(UnitElement))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertMetaTest(x: Rep[MetaTest[Unit]]) = RMT0(x.size)
    override def getDefaultRep = RMT0(0)
    override lazy val tag = {
      weakTypeTag[MT0]
    }
  }

  // state representation type
  type MT0Data = Int

  // 3) Iso for concrete class
  class MT0Iso
    extends EntityIso[MT0Data, MT0] with Def[MT0Iso] {
    override def transform(t: Transformer) = new MT0Iso()
    private lazy val _safeFrom = fun { p: Rep[MT0] => p.size }
    override def from(p: Rep[MT0]) =
      tryConvert[MT0, Int](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Int]) = {
      val size = p
      RMT0(size)
    }
    lazy val eFrom = element[Int]
    lazy val eTo = new MT0Elem(self)
    lazy val selfType = new MT0IsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class MT0IsoElem() extends Elem[MT0Iso] {
    def getDefaultRep = reifyObject(new MT0Iso())
    lazy val tag = {
      weakTypeTag[MT0Iso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class MT0CompanionCtor extends CompanionDef[MT0CompanionCtor] with MT0Companion {
    def selfType = MT0CompanionElem
    override def toString = "MT0Companion"

    @scalan.OverloadId("fromFields")
    def apply(size: Rep[Int]): Rep[MT0] =
      mkMT0(size)

    def unapply(p: Rep[MetaTest[Unit]]) = unmkMT0(p)
  }
  lazy val MT0Rep: Rep[MT0CompanionCtor] = new MT0CompanionCtor
  lazy val RMT0: MT0CompanionCtor = proxyMT0Companion(MT0Rep)
  implicit def proxyMT0Companion(p: Rep[MT0CompanionCtor]): MT0CompanionCtor = {
    if (p.rhs.isInstanceOf[MT0CompanionCtor])
      p.rhs.asInstanceOf[MT0CompanionCtor]
    else
      proxyOps[MT0CompanionCtor](p)
  }

  implicit case object MT0CompanionElem extends CompanionElem[MT0CompanionCtor] {
    lazy val tag = weakTypeTag[MT0CompanionCtor]
    protected def getDefaultRep = MT0Rep
  }

  implicit def proxyMT0(p: Rep[MT0]): MT0 =
    proxyOps[MT0](p)

  implicit class ExtendedMT0(p: Rep[MT0]) {
    def toData: Rep[MT0Data] = {
      isoMT0.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT0: Iso[MT0Data, MT0] =
    reifyObject(new MT0Iso())

  def mkMT0
    (size: Rep[Int]): Rep[MT0] = {
    new MT0Ctor(size)
  }
  def unmkMT0(p: Rep[MetaTest[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT0Elem @unchecked =>
      Some((asRep[MT0](p).size))
    case _ =>
      None
  }

    object MT0Methods {
    object test {
      def unapply(d: Def[_]): Nullable[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "test" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT0]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object give {
      def unapply(d: Def[_]): Nullable[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "give" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT0]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object eT {
      def unapply(d: Def[_]): Nullable[Rep[MT0]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "eT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT0]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT0]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[MT0], Seq[Rep[B]], Elem[B]) forSome {type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[MT0Elem] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[MT0], Seq[Rep[B]], Elem[B]) forSome {type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[MT0], Seq[Rep[B]], Elem[B]) forSome {type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object MT0CompanionMethods {
  }
} // of object MT0
  registerEntityObject("MT0", MT0)

object MT1 extends EntityObject("MT1") {
  case class MT1Ctor[T]
      (override val data: Rep[T], override val size: Rep[Int])
    extends MT1[T](data, size) with Def[MT1[T]] {
    implicit lazy val eT = data.elem

    lazy val selfType = element[MT1[T]]
    override def transform(t: Transformer) = MT1Ctor[T](t(data), t(size))
  }
  // elem for concrete class
  class MT1Elem[T](val iso: Iso[MT1Data[T], MT1[T]])(implicit override val eT: Elem[T])
    extends MetaTestElem[T, MT1[T]]
    with ConcreteElem[MT1Data[T], MT1[T]] {
    override lazy val parent: Option[Elem[_]] = Some(metaTestElement(element[T]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertMetaTest(x: Rep[MetaTest[T]]) = // Converter is not generated by meta
!!!("Cannot convert from MetaTest to MT1: missing fields List(data)")
    override def getDefaultRep = RMT1(element[T].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MT1[T]]
    }
  }

  // state representation type
  type MT1Data[T] = (T, Int)

  // 3) Iso for concrete class
  class MT1Iso[T](implicit eT: Elem[T])
    extends EntityIso[MT1Data[T], MT1[T]] with Def[MT1Iso[T]] {
    override def transform(t: Transformer) = new MT1Iso[T]()(eT)
    private lazy val _safeFrom = fun { p: Rep[MT1[T]] => (p.data, p.size) }
    override def from(p: Rep[MT1[T]]) =
      tryConvert[MT1[T], (T, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(T, Int)]) = {
      val Pair(data, size) = p
      RMT1(data, size)
    }
    lazy val eFrom = pairElement(element[T], element[Int])
    lazy val eTo = new MT1Elem[T](self)
    lazy val selfType = new MT1IsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class MT1IsoElem[T](eT: Elem[T]) extends Elem[MT1Iso[T]] {
    def getDefaultRep = reifyObject(new MT1Iso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MT1Iso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class MT1CompanionCtor extends CompanionDef[MT1CompanionCtor] {
    def selfType = MT1CompanionElem
    override def toString = "MT1Companion"
    @scalan.OverloadId("fromData")
    def apply[T](p: Rep[MT1Data[T]]): Rep[MT1[T]] = {
      implicit val eT = p._1.elem
      isoMT1[T].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[T](data: Rep[T], size: Rep[Int]): Rep[MT1[T]] =
      mkMT1(data, size)

    def unapply[T](p: Rep[MetaTest[T]]) = unmkMT1(p)
  }
  lazy val MT1Rep: Rep[MT1CompanionCtor] = new MT1CompanionCtor
  lazy val RMT1: MT1CompanionCtor = proxyMT1Companion(MT1Rep)
  implicit def proxyMT1Companion(p: Rep[MT1CompanionCtor]): MT1CompanionCtor = {
    if (p.rhs.isInstanceOf[MT1CompanionCtor])
      p.rhs.asInstanceOf[MT1CompanionCtor]
    else
      proxyOps[MT1CompanionCtor](p)
  }

  implicit case object MT1CompanionElem extends CompanionElem[MT1CompanionCtor] {
    lazy val tag = weakTypeTag[MT1CompanionCtor]
    protected def getDefaultRep = MT1Rep
  }

  implicit def proxyMT1[T](p: Rep[MT1[T]]): MT1[T] =
    proxyOps[MT1[T]](p)

  implicit class ExtendedMT1[T](p: Rep[MT1[T]]) {
    def toData: Rep[MT1Data[T]] = {
      implicit val eT = p.data.elem
      isoMT1(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT1[T](implicit eT: Elem[T]): Iso[MT1Data[T], MT1[T]] =
    reifyObject(new MT1Iso[T]()(eT))

  def mkMT1[T]
    (data: Rep[T], size: Rep[Int]): Rep[MT1[T]] = {
    new MT1Ctor[T](data, size)
  }
  def unmkMT1[T](p: Rep[MetaTest[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT1Elem[T] @unchecked =>
      Some((asRep[MT1[T]](p).data, asRep[MT1[T]](p).size))
    case _ =>
      None
  }

    object MT1Methods {
    object test {
      def unapply(d: Def[_]): Nullable[Rep[MT1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "test" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT1[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object give {
      def unapply(d: Def[_]): Nullable[Rep[MT1[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "give" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT1[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT1[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[MT1[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[MT1Elem[_]] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[MT1[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[MT1[T]], Seq[Rep[B]], Elem[B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object MT1
  registerEntityObject("MT1", MT1)

object MT2 extends EntityObject("MT2") {
  case class MT2Ctor[A, B]
      (override val indices: Rep[A], override val values: Rep[B], override val size: Rep[Int])
    extends MT2[A, B](indices, values, size) with Def[MT2[A, B]] {
    implicit lazy val eA = indices.elem;
implicit lazy val eB = values.elem
    override lazy val eT: Elem[(A, B)] = implicitly[Elem[(A, B)]]
    lazy val selfType = element[MT2[A, B]]
    override def transform(t: Transformer) = MT2Ctor[A, B](t(indices), t(values), t(size))
  }
  // elem for concrete class
  class MT2Elem[A, B](val iso: Iso[MT2Data[A, B], MT2[A, B]])(implicit override val eA: Elem[A], override val eB: Elem[B])
    extends MetaPairElem[A, B, MT2[A, B]]
    with ConcreteElem[MT2Data[A, B], MT2[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(metaPairElement(element[A], element[B]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
    override def convertMetaPair(x: Rep[MetaPair[A, B]]) = RMT2(x.indices, x.values, x.size)
    override def getDefaultRep = RMT2(element[A].defaultRepValue, element[B].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[MT2[A, B]]
    }
  }

  // state representation type
  type MT2Data[A, B] = (A, (B, Int))

  // 3) Iso for concrete class
  class MT2Iso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends EntityIso[MT2Data[A, B], MT2[A, B]] with Def[MT2Iso[A, B]] {
    override def transform(t: Transformer) = new MT2Iso[A, B]()(eA, eB)
    private lazy val _safeFrom = fun { p: Rep[MT2[A, B]] => (p.indices, p.values, p.size) }
    override def from(p: Rep[MT2[A, B]]) =
      tryConvert[MT2[A, B], (A, (B, Int))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(A, (B, Int))]) = {
      val Pair(indices, Pair(values, size)) = p
      RMT2(indices, values, size)
    }
    lazy val eFrom = pairElement(element[A], pairElement(element[B], element[Int]))
    lazy val eTo = new MT2Elem[A, B](self)
    lazy val selfType = new MT2IsoElem[A, B](eA, eB)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eA
      case 1 => eB
    }
  }
  case class MT2IsoElem[A, B](eA: Elem[A], eB: Elem[B]) extends Elem[MT2Iso[A, B]] {
    def getDefaultRep = reifyObject(new MT2Iso[A, B]()(eA, eB))
    lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[MT2Iso[A, B]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class MT2CompanionCtor extends CompanionDef[MT2CompanionCtor] {
    def selfType = MT2CompanionElem
    override def toString = "MT2Companion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Rep[MT2Data[A, B]]): Rep[MT2[A, B]] = {
      implicit val eA = p._1.elem;
implicit val eB = p._2.elem
      isoMT2[A, B].to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](indices: Rep[A], values: Rep[B], size: Rep[Int]): Rep[MT2[A, B]] =
      mkMT2(indices, values, size)

    def unapply[A, B](p: Rep[MetaPair[A, B]]) = unmkMT2(p)
  }
  lazy val MT2Rep: Rep[MT2CompanionCtor] = new MT2CompanionCtor
  lazy val RMT2: MT2CompanionCtor = proxyMT2Companion(MT2Rep)
  implicit def proxyMT2Companion(p: Rep[MT2CompanionCtor]): MT2CompanionCtor = {
    if (p.rhs.isInstanceOf[MT2CompanionCtor])
      p.rhs.asInstanceOf[MT2CompanionCtor]
    else
      proxyOps[MT2CompanionCtor](p)
  }

  implicit case object MT2CompanionElem extends CompanionElem[MT2CompanionCtor] {
    lazy val tag = weakTypeTag[MT2CompanionCtor]
    protected def getDefaultRep = MT2Rep
  }

  implicit def proxyMT2[A, B](p: Rep[MT2[A, B]]): MT2[A, B] =
    proxyOps[MT2[A, B]](p)

  implicit class ExtendedMT2[A, B](p: Rep[MT2[A, B]]) {
    def toData: Rep[MT2Data[A, B]] = {
      implicit val eA = p.indices.elem;
implicit val eB = p.values.elem
      isoMT2(eA, eB).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoMT2[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[MT2Data[A, B], MT2[A, B]] =
    reifyObject(new MT2Iso[A, B]()(eA, eB))

  def mkMT2[A, B]
    (indices: Rep[A], values: Rep[B], size: Rep[Int]): Rep[MT2[A, B]] = {
    new MT2Ctor[A, B](indices, values, size)
  }
  def unmkMT2[A, B](p: Rep[MetaPair[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MT2Elem[A, B] @unchecked =>
      Some((asRep[MT2[A, B]](p).indices, asRep[MT2[A, B]](p).values, asRep[MT2[A, B]](p).size))
    case _ =>
      None
  }

    object MT2Methods {
    object test {
      def unapply(d: Def[_]): Nullable[Rep[MT2[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "test" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT2[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT2[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object give {
      def unapply(d: Def[_]): Nullable[Rep[MT2[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "give" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[MT2[A, B]] forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[MT2[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Rep[MT2[A, B]], Seq[Rep[C]], Elem[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[MT2Elem[_, _]] && method.getName == "fromItems" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[MT2[A, B]], Seq[Rep[C]], Elem[C]) forSome {type A; type B; type C}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[MT2[A, B]], Seq[Rep[C]], Elem[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }
} // of object MT2
  registerEntityObject("MT2", MT2)

  registerModule(MetaTestsModule)
}

object MetaTestsModule extends scalan.ModuleInfo("scalan.common", "MetaTests")
}

