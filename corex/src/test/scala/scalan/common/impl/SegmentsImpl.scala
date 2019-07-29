package scalan.common

import scala.reflect.runtime.universe._
import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait SegmentsDefs extends scalan.Scalan with Segments {
  self: SegmentsModule =>
import IsoUR._
import Converter._
import Segment._
import Slice._
import Interval._
import Centered._

object Segment extends EntityObject("Segment") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SSegment = scalan.common.Segment
  case class SegmentConst(
        constValue: SSegment
      ) extends Segment with LiftedConst[SSegment, Segment]
        with Def[Segment] with SegmentConstMethods {
    val liftable: Liftable[SSegment, Segment] = LiftableSegment
    val selfType: Elem[Segment] = liftable.eW
  }

  trait SegmentConstMethods extends Segment  { thisConst: Def[_] =>

    private val SegmentClass = classOf[Segment]

    override def start: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("start"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def length: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("length"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def end: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("end"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def shift(ofs: Rep[Int]): Rep[Segment] = {
      asRep[Segment](mkMethodCall(self,
        SegmentClass.getMethod("shift", classOf[Sym]),
        Array[AnyRef](ofs),
        true, false, element[Segment]))
    }

    override def attach(seg: Rep[Segment]): Rep[Segment] = {
      asRep[Segment](mkMethodCall(self,
        SegmentClass.getMethod("attach", classOf[Sym]),
        Array[AnyRef](seg),
        true, false, element[Segment]))
    }
  }

  implicit object LiftableSegment
    extends Liftable[SSegment, Segment] {
    lazy val eW: Elem[Segment] = segmentElement
    lazy val sourceType: RType[SSegment] = {
      RType[SSegment]
    }
    def lift(x: SSegment): Rep[Segment] = SegmentConst(x)
    def unlift(w: Rep[Segment]): SSegment = w match {
      case Def(SegmentConst(x: SSegment))
            => x.asInstanceOf[SSegment]
      case _ => unliftError(w)
    }
  }

  private val SegmentClass = classOf[Segment]

  // entityAdapter for Segment trait
  case class SegmentAdapter(source: Rep[Segment])
      extends Segment
      with Def[Segment] {
    val selfType: Elem[Segment] = element[Segment]
    override def transform(t: Transformer) = SegmentAdapter(t(source))

    def start: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("start"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def length: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("length"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def end: Rep[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("end"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def shift(ofs: Rep[Int]): Rep[Segment] = {
      asRep[Segment](mkMethodCall(source,
        SegmentClass.getMethod("shift", classOf[Sym]),
        Array[AnyRef](ofs),
        true, true, element[Segment]))
    }

    def attach(seg: Rep[Segment]): Rep[Segment] = {
      asRep[Segment](mkMethodCall(source,
        SegmentClass.getMethod("attach", classOf[Sym]),
        Array[AnyRef](seg),
        true, true, element[Segment]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxySegment(p: Rep[Segment]): Segment = {
    if (p.rhs.isInstanceOf[Segment]) p.rhs.asInstanceOf[Segment]
    else
      SegmentAdapter(p)
  }

  // familyElem
  class SegmentElem[To <: Segment]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SSegment, To](LiftableSegment)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Segment], classOf[SSegment], Set(
        "start", "length", "end", "shift", "attach"
        ))
    }

    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[Segment] => convertSegment(x) }
      tryConvert(element[Segment], this, x, conv)
    }

    def convertSegment(x: Rep[Segment]): Rep[To] = {
      x.elem match {
        case _: SegmentElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have SegmentElem[_], but got $e", x)
      }
    }
  }

  implicit lazy val segmentElement: Elem[Segment] =
    new SegmentElem[Segment]

  implicit case object SegmentCompanionElem extends CompanionElem[SegmentCompanionCtor]

  abstract class SegmentCompanionCtor extends CompanionDef[SegmentCompanionCtor] with SegmentCompanion {
    def selfType = SegmentCompanionElem
    override def toString = "Segment"
  }
  implicit def proxySegmentCompanionCtor(p: Rep[SegmentCompanionCtor]): SegmentCompanionCtor =
    p.rhs.asInstanceOf[SegmentCompanionCtor]

  lazy val RSegment: Rep[SegmentCompanionCtor] = new SegmentCompanionCtor {
    private val thisClass = classOf[SegmentCompanion]
  }

  object SegmentMethods {
    object start {
      def unapply(d: Def[_]): Nullable[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "start" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Segment]] = unapply(exp.rhs)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Segment]] = unapply(exp.rhs)
    }

    object end {
      def unapply(d: Def[_]): Nullable[Rep[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Segment]] = unapply(exp.rhs)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Rep[Segment], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Segment], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Segment], Rep[Int])] = unapply(exp.rhs)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Rep[Segment], Rep[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Segment], Rep[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Segment], Rep[Segment])] = unapply(exp.rhs)
    }
  }

  object SegmentCompanionMethods {
  }
} // of object Segment
  registerEntityObject("Segment", Segment)

object Interval extends EntityObject("Interval") {
  case class IntervalCtor
      (override val start: Rep[Int], override val end: Rep[Int])
    extends Interval(start, end) with Def[Interval] {
    lazy val selfType = element[Interval]
    override def transform(t: Transformer) = IntervalCtor(t(start), t(end))
    private val thisClass = classOf[Segment]

    override def attach(seg: Rep[Segment]): Rep[Segment] = {
      asRep[Segment](mkMethodCall(self,
        thisClass.getMethod("attach", classOf[Sym]),
        Array[AnyRef](seg),
        true, false, element[Segment]))
    }
  }
  // elem for concrete class
  class IntervalElem(val iso: Iso[IntervalData, Interval])
    extends SegmentElem[Interval]
    with ConcreteElem[IntervalData, Interval] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)

    override def convertSegment(x: Rep[Segment]) = RInterval(x.start, x.end)
  }

  // state representation type
  type IntervalData = (Int, Int)

  // 3) Iso for concrete class
  class IntervalIso
    extends EntityIso[IntervalData, Interval] with Def[IntervalIso] {
    override def transform(t: Transformer) = new IntervalIso()
    private lazy val _safeFrom = fun { p: Rep[Interval] => (p.start, p.end) }
    override def from(p: Rep[Interval]) =
      tryConvert[Interval, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, end) = p
      RInterval(start, end)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new IntervalElem(self)
    lazy val selfType = new IntervalIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class IntervalIsoElem() extends Elem[IntervalIso] {
  }
  // 4) constructor and deconstructor
  class IntervalCompanionCtor extends CompanionDef[IntervalCompanionCtor] with IntervalCompanion {
    def selfType = IntervalCompanionElem
    override def toString = "IntervalCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[IntervalData]): Rep[Interval] = {
      isoInterval.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Rep[Int], end: Rep[Int]): Rep[Interval] =
      mkInterval(start, end)

    def unapply(p: Rep[Segment]) = unmkInterval(p)
  }
  lazy val IntervalRep: Rep[IntervalCompanionCtor] = new IntervalCompanionCtor
  lazy val RInterval: IntervalCompanionCtor = proxyIntervalCompanion(IntervalRep)
  implicit def proxyIntervalCompanion(p: Rep[IntervalCompanionCtor]): IntervalCompanionCtor = {
    if (p.rhs.isInstanceOf[IntervalCompanionCtor])
      p.rhs.asInstanceOf[IntervalCompanionCtor]
    else
      proxyOps[IntervalCompanionCtor](p)
  }

  implicit case object IntervalCompanionElem extends CompanionElem[IntervalCompanionCtor]

  implicit def proxyInterval(p: Rep[Interval]): Interval = {
    if (p.rhs.isInstanceOf[Interval])
      p.rhs.asInstanceOf[Interval]
    else
      proxyOps[Interval](p)
  }

  implicit class ExtendedInterval(p: Rep[Interval]) {
    def toData: Rep[IntervalData] = {
      isoInterval.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    reifyObject(new IntervalIso())

  def mkInterval
    (start: Rep[Int], end: Rep[Int]): Rep[Interval] = {
    new IntervalCtor(start, end)
  }
  def unmkInterval(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntervalElem @unchecked =>
      Some((asRep[Interval](p).start, asRep[Interval](p).end))
    case _ =>
      None
  }

    object IntervalMethods {
    object length {
      def unapply(d: Def[_]): Nullable[Rep[Interval]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Interval]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Interval]] = unapply(exp.rhs)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Rep[Interval], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Interval], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Interval], Rep[Int])] = unapply(exp.rhs)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Rep[Interval], Rep[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Interval], Rep[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Interval], Rep[Segment])] = unapply(exp.rhs)
    }
  }

  object IntervalCompanionMethods {
  }
} // of object Interval
  registerEntityObject("Interval", Interval)

object Slice extends EntityObject("Slice") {
  case class SliceCtor
      (override val start: Rep[Int], override val length: Rep[Int])
    extends Slice(start, length) with Def[Slice] {
    lazy val selfType = element[Slice]
    override def transform(t: Transformer) = SliceCtor(t(start), t(length))
  }
  // elem for concrete class
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ConcreteElem[SliceData, Slice] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)

    override def convertSegment(x: Rep[Segment]) = RSlice(x.start, x.length)
  }

  // state representation type
  type SliceData = (Int, Int)

  // 3) Iso for concrete class
  class SliceIso
    extends EntityIso[SliceData, Slice] with Def[SliceIso] {
    override def transform(t: Transformer) = new SliceIso()
    private lazy val _safeFrom = fun { p: Rep[Slice] => (p.start, p.length) }
    override def from(p: Rep[Slice]) =
      tryConvert[Slice, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(start, length) = p
      RSlice(start, length)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new SliceElem(self)
    lazy val selfType = new SliceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SliceIsoElem() extends Elem[SliceIso] {
  }
  // 4) constructor and deconstructor
  class SliceCompanionCtor extends CompanionDef[SliceCompanionCtor] with SliceCompanion {
    def selfType = SliceCompanionElem
    override def toString = "SliceCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[SliceData]): Rep[Slice] = {
      isoSlice.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Rep[Int], length: Rep[Int]): Rep[Slice] =
      mkSlice(start, length)

    def unapply(p: Rep[Segment]) = unmkSlice(p)
  }
  lazy val SliceRep: Rep[SliceCompanionCtor] = new SliceCompanionCtor
  lazy val RSlice: SliceCompanionCtor = proxySliceCompanion(SliceRep)
  implicit def proxySliceCompanion(p: Rep[SliceCompanionCtor]): SliceCompanionCtor = {
    if (p.rhs.isInstanceOf[SliceCompanionCtor])
      p.rhs.asInstanceOf[SliceCompanionCtor]
    else
      proxyOps[SliceCompanionCtor](p)
  }

  implicit case object SliceCompanionElem extends CompanionElem[SliceCompanionCtor]

  implicit def proxySlice(p: Rep[Slice]): Slice = {
    if (p.rhs.isInstanceOf[Slice])
      p.rhs.asInstanceOf[Slice]
    else
      proxyOps[Slice](p)
  }

  implicit class ExtendedSlice(p: Rep[Slice]) {
    def toData: Rep[SliceData] = {
      isoSlice.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    reifyObject(new SliceIso())

  def mkSlice
    (start: Rep[Int], length: Rep[Int]): Rep[Slice] = {
    new SliceCtor(start, length)
  }
  def unmkSlice(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SliceElem @unchecked =>
      Some((asRep[Slice](p).start, asRep[Slice](p).length))
    case _ =>
      None
  }

    object SliceMethods {
    object end {
      def unapply(d: Def[_]): Nullable[Rep[Slice]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Slice]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Slice]] = unapply(exp.rhs)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Rep[Slice], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Slice], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Slice], Rep[Int])] = unapply(exp.rhs)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Rep[Slice], Rep[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Slice], Rep[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Slice], Rep[Segment])] = unapply(exp.rhs)
    }
  }

  object SliceCompanionMethods {
  }
} // of object Slice
  registerEntityObject("Slice", Slice)

object Centered extends EntityObject("Centered") {
  case class CenteredCtor
      (override val center: Rep[Int], override val radius: Rep[Int])
    extends Centered(center, radius) with Def[Centered] {
    lazy val selfType = element[Centered]
    override def transform(t: Transformer) = CenteredCtor(t(center), t(radius))
  }
  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ConcreteElem[CenteredData, Centered] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)

    override def convertSegment(x: Rep[Segment]) = // Converter is not generated by meta
!!!("Cannot convert from Segment to Centered: missing fields List(center, radius)")
  }

  // state representation type
  type CenteredData = (Int, Int)

  // 3) Iso for concrete class
  class CenteredIso
    extends EntityIso[CenteredData, Centered] with Def[CenteredIso] {
    override def transform(t: Transformer) = new CenteredIso()
    private lazy val _safeFrom = fun { p: Rep[Centered] => (p.center, p.radius) }
    override def from(p: Rep[Centered]) =
      tryConvert[Centered, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Int, Int)]) = {
      val Pair(center, radius) = p
      RCentered(center, radius)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new CenteredElem(self)
    lazy val selfType = new CenteredIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CenteredIsoElem() extends Elem[CenteredIso] {
  }
  // 4) constructor and deconstructor
  class CenteredCompanionCtor extends CompanionDef[CenteredCompanionCtor] with CenteredCompanion {
    def selfType = CenteredCompanionElem
    override def toString = "CenteredCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CenteredData]): Rep[Centered] = {
      isoCentered.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(center: Rep[Int], radius: Rep[Int]): Rep[Centered] =
      mkCentered(center, radius)

    def unapply(p: Rep[Segment]) = unmkCentered(p)
  }
  lazy val CenteredRep: Rep[CenteredCompanionCtor] = new CenteredCompanionCtor
  lazy val RCentered: CenteredCompanionCtor = proxyCenteredCompanion(CenteredRep)
  implicit def proxyCenteredCompanion(p: Rep[CenteredCompanionCtor]): CenteredCompanionCtor = {
    if (p.rhs.isInstanceOf[CenteredCompanionCtor])
      p.rhs.asInstanceOf[CenteredCompanionCtor]
    else
      proxyOps[CenteredCompanionCtor](p)
  }

  implicit case object CenteredCompanionElem extends CompanionElem[CenteredCompanionCtor]

  implicit def proxyCentered(p: Rep[Centered]): Centered = {
    if (p.rhs.isInstanceOf[Centered])
      p.rhs.asInstanceOf[Centered]
    else
      proxyOps[Centered](p)
  }

  implicit class ExtendedCentered(p: Rep[Centered]) {
    def toData: Rep[CenteredData] = {
      isoCentered.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    reifyObject(new CenteredIso())

  def mkCentered
    (center: Rep[Int], radius: Rep[Int]): Rep[Centered] = {
    new CenteredCtor(center, radius)
  }
  def unmkCentered(p: Rep[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CenteredElem @unchecked =>
      Some((asRep[Centered](p).center, asRep[Centered](p).radius))
    case _ =>
      None
  }

    object CenteredMethods {
    object start {
      def unapply(d: Def[_]): Nullable[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "start" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Centered]] = unapply(exp.rhs)
    }

    object end {
      def unapply(d: Def[_]): Nullable[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Centered]] = unapply(exp.rhs)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Rep[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[Centered]] = unapply(exp.rhs)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Rep[Centered], Rep[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Centered], Rep[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Centered], Rep[Int])] = unapply(exp.rhs)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Rep[Centered], Rep[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[Centered], Rep[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[Centered], Rep[Segment])] = unapply(exp.rhs)
    }
  }

  object CenteredCompanionMethods {
  }
} // of object Centered
  registerEntityObject("Centered", Centered)

  registerModule(SegmentsModule)
}

object SegmentsModule extends scalan.ModuleInfo("scalan.common", "Segments")
}

trait SegmentsModule extends scalan.common.impl.SegmentsDefs
