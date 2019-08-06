package scalan.common

import scala.reflect.runtime.universe._
import scalan._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait SegmentsDefs extends scalan.Scalan with Segments {
  self: SegmentsModule =>
import IsoUR._
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
    val resultType: Elem[Segment] = liftable.eW
  }

  trait SegmentConstMethods extends Segment  { thisConst: Def[_] =>

    private val SegmentClass = classOf[Segment]

    override def start: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("start"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def length: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("length"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def end: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        SegmentClass.getMethod("end"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def shift(ofs: Ref[Int]): Ref[Segment] = {
      asRep[Segment](mkMethodCall(self,
        SegmentClass.getMethod("shift", classOf[Sym]),
        Array[AnyRef](ofs),
        true, false, element[Segment]))
    }

    override def attach(seg: Ref[Segment]): Ref[Segment] = {
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
    def lift(x: SSegment): Ref[Segment] = SegmentConst(x)
    def unlift(w: Ref[Segment]): SSegment = w match {
      case Def(SegmentConst(x: SSegment))
            => x.asInstanceOf[SSegment]
      case _ => unliftError(w)
    }
  }

  private val SegmentClass = classOf[Segment]

  // entityAdapter for Segment trait
  case class SegmentAdapter(source: Ref[Segment])
      extends Segment
      with Def[Segment] {
    val resultType: Elem[Segment] = element[Segment]
    override def transform(t: Transformer) = SegmentAdapter(t(source))

    def start: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("start"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def length: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("length"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def end: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        SegmentClass.getMethod("end"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def shift(ofs: Ref[Int]): Ref[Segment] = {
      asRep[Segment](mkMethodCall(source,
        SegmentClass.getMethod("shift", classOf[Sym]),
        Array[AnyRef](ofs),
        true, true, element[Segment]))
    }

    def attach(seg: Ref[Segment]): Ref[Segment] = {
      asRep[Segment](mkMethodCall(source,
        SegmentClass.getMethod("attach", classOf[Sym]),
        Array[AnyRef](seg),
        true, true, element[Segment]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefSegment(p: Ref[Segment]): Segment = {
    if (p.node.isInstanceOf[Segment]) p.node.asInstanceOf[Segment]
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

    override def convert(x: Ref[Def[_]]) = {
      val conv = fun {x: Ref[Segment] => convertSegment(x) }
      tryConvert(element[Segment], this, x, conv)
    }

    def convertSegment(x: Ref[Segment]): Ref[To] = {
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
    def resultType = SegmentCompanionElem
    override def toString = "Segment"
  }
  implicit def unrefSegmentCompanionCtor(p: Ref[SegmentCompanionCtor]): SegmentCompanionCtor =
    p.node.asInstanceOf[SegmentCompanionCtor]

  lazy val RSegment: Ref[SegmentCompanionCtor] = new SegmentCompanionCtor {
    private val thisClass = classOf[SegmentCompanion]
  }

  object SegmentMethods {
    object start {
      def unapply(d: Def[_]): Nullable[Ref[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "start" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Segment]] = unapply(exp.node)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Ref[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Segment]] = unapply(exp.node)
    }

    object end {
      def unapply(d: Def[_]): Nullable[Ref[Segment]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Segment]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Segment]] = unapply(exp.node)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Ref[Segment], Ref[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Segment], Ref[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Segment], Ref[Int])] = unapply(exp.node)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Ref[Segment], Ref[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[SegmentElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Segment], Ref[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Segment], Ref[Segment])] = unapply(exp.node)
    }
  }

  object SegmentCompanionMethods {
  }
} // of object Segment
  registerEntityObject("Segment", Segment)

object Interval extends EntityObject("Interval") {
  case class IntervalCtor
      (override val start: Ref[Int], override val end: Ref[Int])
    extends Interval(start, end) with Def[Interval] {
    lazy val resultType = element[Interval]
    override def transform(t: Transformer) = IntervalCtor(t(start), t(end))
    private val thisClass = classOf[Segment]

    override def attach(seg: Ref[Segment]): Ref[Segment] = {
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

    override def convertSegment(x: Ref[Segment]) = RInterval(x.start, x.end)
  }

  // state representation type
  type IntervalData = (Int, Int)

  // 3) Iso for concrete class
  class IntervalIso
    extends EntityIso[IntervalData, Interval] with Def[IntervalIso] {
    override def transform(t: Transformer) = new IntervalIso()
    private lazy val _safeFrom = fun { p: Ref[Interval] => (p.start, p.end) }
    override def from(p: Ref[Interval]) =
      tryConvert[Interval, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Int, Int)]) = {
      val Pair(start, end) = p
      RInterval(start, end)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new IntervalElem(self)
    lazy val resultType = new IntervalIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class IntervalIsoElem() extends Elem[IntervalIso] {
  }
  // 4) constructor and deconstructor
  class IntervalCompanionCtor extends CompanionDef[IntervalCompanionCtor] with IntervalCompanion {
    def resultType = IntervalCompanionElem
    override def toString = "IntervalCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[IntervalData]): Ref[Interval] = {
      isoInterval.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Ref[Int], end: Ref[Int]): Ref[Interval] =
      mkInterval(start, end)

    def unapply(p: Ref[Segment]) = unmkInterval(p)
  }
  lazy val IntervalRef: Ref[IntervalCompanionCtor] = new IntervalCompanionCtor
  lazy val RInterval: IntervalCompanionCtor = unrefIntervalCompanion(IntervalRef)
  implicit def unrefIntervalCompanion(p: Ref[IntervalCompanionCtor]): IntervalCompanionCtor = {
    if (p.node.isInstanceOf[IntervalCompanionCtor])
      p.node.asInstanceOf[IntervalCompanionCtor]
    else
      unrefDelegate[IntervalCompanionCtor](p)
  }

  implicit case object IntervalCompanionElem extends CompanionElem[IntervalCompanionCtor]

  implicit def unrefInterval(p: Ref[Interval]): Interval = {
    if (p.node.isInstanceOf[Interval])
      p.node.asInstanceOf[Interval]
    else
      unrefDelegate[Interval](p)
  }

  implicit class ExtendedInterval(p: Ref[Interval]) {
    def toData: Ref[IntervalData] = {
      isoInterval.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoInterval: Iso[IntervalData, Interval] =
    reifyObject(new IntervalIso())

  def mkInterval
    (start: Ref[Int], end: Ref[Int]): Ref[Interval] = {
    new IntervalCtor(start, end)
  }
  def unmkInterval(p: Ref[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntervalElem @unchecked =>
      Some((asRep[Interval](p).start, asRep[Interval](p).end))
    case _ =>
      None
  }

    object IntervalMethods {
    object length {
      def unapply(d: Def[_]): Nullable[Ref[Interval]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Interval]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Interval]] = unapply(exp.node)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Ref[Interval], Ref[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Interval], Ref[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Interval], Ref[Int])] = unapply(exp.node)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Ref[Interval], Ref[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[IntervalElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Interval], Ref[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Interval], Ref[Segment])] = unapply(exp.node)
    }
  }

  object IntervalCompanionMethods {
  }
} // of object Interval
  registerEntityObject("Interval", Interval)

object Slice extends EntityObject("Slice") {
  case class SliceCtor
      (override val start: Ref[Int], override val length: Ref[Int])
    extends Slice(start, length) with Def[Slice] {
    lazy val resultType = element[Slice]
    override def transform(t: Transformer) = SliceCtor(t(start), t(length))
  }
  // elem for concrete class
  class SliceElem(val iso: Iso[SliceData, Slice])
    extends SegmentElem[Slice]
    with ConcreteElem[SliceData, Slice] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)

    override def convertSegment(x: Ref[Segment]) = RSlice(x.start, x.length)
  }

  // state representation type
  type SliceData = (Int, Int)

  // 3) Iso for concrete class
  class SliceIso
    extends EntityIso[SliceData, Slice] with Def[SliceIso] {
    override def transform(t: Transformer) = new SliceIso()
    private lazy val _safeFrom = fun { p: Ref[Slice] => (p.start, p.length) }
    override def from(p: Ref[Slice]) =
      tryConvert[Slice, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Int, Int)]) = {
      val Pair(start, length) = p
      RSlice(start, length)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new SliceElem(self)
    lazy val resultType = new SliceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class SliceIsoElem() extends Elem[SliceIso] {
  }
  // 4) constructor and deconstructor
  class SliceCompanionCtor extends CompanionDef[SliceCompanionCtor] with SliceCompanion {
    def resultType = SliceCompanionElem
    override def toString = "SliceCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[SliceData]): Ref[Slice] = {
      isoSlice.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(start: Ref[Int], length: Ref[Int]): Ref[Slice] =
      mkSlice(start, length)

    def unapply(p: Ref[Segment]) = unmkSlice(p)
  }
  lazy val SliceRef: Ref[SliceCompanionCtor] = new SliceCompanionCtor
  lazy val RSlice: SliceCompanionCtor = unrefSliceCompanion(SliceRef)
  implicit def unrefSliceCompanion(p: Ref[SliceCompanionCtor]): SliceCompanionCtor = {
    if (p.node.isInstanceOf[SliceCompanionCtor])
      p.node.asInstanceOf[SliceCompanionCtor]
    else
      unrefDelegate[SliceCompanionCtor](p)
  }

  implicit case object SliceCompanionElem extends CompanionElem[SliceCompanionCtor]

  implicit def unrefSlice(p: Ref[Slice]): Slice = {
    if (p.node.isInstanceOf[Slice])
      p.node.asInstanceOf[Slice]
    else
      unrefDelegate[Slice](p)
  }

  implicit class ExtendedSlice(p: Ref[Slice]) {
    def toData: Ref[SliceData] = {
      isoSlice.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoSlice: Iso[SliceData, Slice] =
    reifyObject(new SliceIso())

  def mkSlice
    (start: Ref[Int], length: Ref[Int]): Ref[Slice] = {
    new SliceCtor(start, length)
  }
  def unmkSlice(p: Ref[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SliceElem @unchecked =>
      Some((asRep[Slice](p).start, asRep[Slice](p).length))
    case _ =>
      None
  }

    object SliceMethods {
    object end {
      def unapply(d: Def[_]): Nullable[Ref[Slice]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Slice]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Slice]] = unapply(exp.node)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Ref[Slice], Ref[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Slice], Ref[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Slice], Ref[Int])] = unapply(exp.node)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Ref[Slice], Ref[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[SliceElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Slice], Ref[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Slice], Ref[Segment])] = unapply(exp.node)
    }
  }

  object SliceCompanionMethods {
  }
} // of object Slice
  registerEntityObject("Slice", Slice)

object Centered extends EntityObject("Centered") {
  case class CenteredCtor
      (override val center: Ref[Int], override val radius: Ref[Int])
    extends Centered(center, radius) with Def[Centered] {
    lazy val resultType = element[Centered]
    override def transform(t: Transformer) = CenteredCtor(t(center), t(radius))
  }
  // elem for concrete class
  class CenteredElem(val iso: Iso[CenteredData, Centered])
    extends SegmentElem[Centered]
    with ConcreteElem[CenteredData, Centered] {
    override lazy val parent: Option[Elem[_]] = Some(segmentElement)

    override def convertSegment(x: Ref[Segment]) = // Converter is not generated by meta
!!!("Cannot convert from Segment to Centered: missing fields List(center, radius)")
  }

  // state representation type
  type CenteredData = (Int, Int)

  // 3) Iso for concrete class
  class CenteredIso
    extends EntityIso[CenteredData, Centered] with Def[CenteredIso] {
    override def transform(t: Transformer) = new CenteredIso()
    private lazy val _safeFrom = fun { p: Ref[Centered] => (p.center, p.radius) }
    override def from(p: Ref[Centered]) =
      tryConvert[Centered, (Int, Int)](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Int, Int)]) = {
      val Pair(center, radius) = p
      RCentered(center, radius)
    }
    lazy val eFrom = pairElement(element[Int], element[Int])
    lazy val eTo = new CenteredElem(self)
    lazy val resultType = new CenteredIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CenteredIsoElem() extends Elem[CenteredIso] {
  }
  // 4) constructor and deconstructor
  class CenteredCompanionCtor extends CompanionDef[CenteredCompanionCtor] with CenteredCompanion {
    def resultType = CenteredCompanionElem
    override def toString = "CenteredCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CenteredData]): Ref[Centered] = {
      isoCentered.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(center: Ref[Int], radius: Ref[Int]): Ref[Centered] =
      mkCentered(center, radius)

    def unapply(p: Ref[Segment]) = unmkCentered(p)
  }
  lazy val CenteredRef: Ref[CenteredCompanionCtor] = new CenteredCompanionCtor
  lazy val RCentered: CenteredCompanionCtor = unrefCenteredCompanion(CenteredRef)
  implicit def unrefCenteredCompanion(p: Ref[CenteredCompanionCtor]): CenteredCompanionCtor = {
    if (p.node.isInstanceOf[CenteredCompanionCtor])
      p.node.asInstanceOf[CenteredCompanionCtor]
    else
      unrefDelegate[CenteredCompanionCtor](p)
  }

  implicit case object CenteredCompanionElem extends CompanionElem[CenteredCompanionCtor]

  implicit def unrefCentered(p: Ref[Centered]): Centered = {
    if (p.node.isInstanceOf[Centered])
      p.node.asInstanceOf[Centered]
    else
      unrefDelegate[Centered](p)
  }

  implicit class ExtendedCentered(p: Ref[Centered]) {
    def toData: Ref[CenteredData] = {
      isoCentered.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCentered: Iso[CenteredData, Centered] =
    reifyObject(new CenteredIso())

  def mkCentered
    (center: Ref[Int], radius: Ref[Int]): Ref[Centered] = {
    new CenteredCtor(center, radius)
  }
  def unmkCentered(p: Ref[Segment]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CenteredElem @unchecked =>
      Some((asRep[Centered](p).center, asRep[Centered](p).radius))
    case _ =>
      None
  }

    object CenteredMethods {
    object start {
      def unapply(d: Def[_]): Nullable[Ref[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "start" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Centered]] = unapply(exp.node)
    }

    object end {
      def unapply(d: Def[_]): Nullable[Ref[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "end" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Centered]] = unapply(exp.node)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Ref[Centered]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Centered]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Centered]] = unapply(exp.node)
    }

    object shift {
      def unapply(d: Def[_]): Nullable[(Ref[Centered], Ref[Int])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "shift" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Centered], Ref[Int])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Centered], Ref[Int])] = unapply(exp.node)
    }

    object attach {
      def unapply(d: Def[_]): Nullable[(Ref[Centered], Ref[Segment])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "attach" && receiver.elem.isInstanceOf[CenteredElem] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Centered], Ref[Segment])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Centered], Ref[Segment])] = unapply(exp.node)
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
