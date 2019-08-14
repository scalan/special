package scalan.common

import scala.reflect.runtime.universe._
import scalan._

trait Segments { self: SegmentsModule =>

  import Segment._
  import Slice._
  import Interval._
  import Centered._

  type RSeg = Ref[Segment]
  @scalan.Liftable
  @Convertible
  @WithMethodCallRecognizers
  trait Segment extends Def[Segment] { self =>
    def start: Ref[Int]
    def length: Ref[Int]
    def end: Ref[Int]
    def shift(ofs: Ref[Int]): Ref[Segment]
    def attach(seg: Ref[Segment]): Ref[Segment]
  }
  trait SegmentCompanion

  @WithMethodCallRecognizers
  @Isospec
  abstract class Interval(val start: Ref[Int], val end: Ref[Int]) extends Segment {
    def length = end - start
    def shift(ofs: Ref[Int]) = RInterval(start + ofs, end + ofs)
    @NeverInline
    def attach(seg: Ref[Segment]): Ref[Segment] = seg match {
      case RInterval(start, end) =>
        seg
      case RSlice(start, length) =>
        self
      case RCentered(center, radius) =>
        self
      case _ => seg attach self
    }
  }
  trait IntervalCompanion

  @WithMethodCallRecognizers
  @Isospec
  abstract class Slice(val start: Ref[Int], val length: Ref[Int]) extends Segment {
    def end = start + length
    def shift(ofs: Ref[Int]) = RSlice(start + ofs, length)
    def attach(seg: Ref[Segment]): Ref[Segment] = self
  }
  trait SliceCompanion

  @WithMethodCallRecognizers
  @Isospec
  abstract class Centered(val center: Ref[Int], val radius: Ref[Int]) extends Segment {
    def start = center - radius
    def end = center + radius
    def length = radius * 2
    def shift(ofs: Ref[Int]) = RCentered(center + ofs, radius)
    def attach(seg: Ref[Segment]): Ref[Segment] = self
  }
  trait CenteredCompanion
}
