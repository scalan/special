package scalan.common

import scala.reflect.runtime.universe._
import scalan._

trait Segments { self: SegmentsModule =>

  import Segment._
  import Slice._
  import Interval._
  import Centered._

  type RSeg = Rep[Segment]
  @scalan.Liftable
  @Convertible
  @WithMethodCallRecognizers
  trait Segment extends Def[Segment] { self =>
    def start: Rep[Int]
    def length: Rep[Int]
    def end: Rep[Int]
    def shift(ofs: Rep[Int]): Rep[Segment]
    def attach(seg: Rep[Segment]): Rep[Segment]
  }
  trait SegmentCompanion

  @WithMethodCallRecognizers
  abstract class Interval(val start: Rep[Int], val end: Rep[Int]) extends Segment {
    def length = end - start
    def shift(ofs: Rep[Int]) = RInterval(start + ofs, end + ofs)
    @NeverInline
    def attach(seg: Rep[Segment]): Rep[Segment] = seg match {
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
  abstract class Slice(val start: Rep[Int], val length: Rep[Int]) extends Segment {
    def end = start + length
    def shift(ofs: Rep[Int]) = RSlice(start + ofs, length)
    def attach(seg: Rep[Segment]): Rep[Segment] = self
  }
  trait SliceCompanion

  @WithMethodCallRecognizers
  abstract class Centered(val center: Rep[Int], val radius: Rep[Int]) extends Segment {
    def start = center - radius
    def end = center + radius
    def length = radius * 2
    def shift(ofs: Rep[Int]) = RCentered(center + ofs, radius)
    def attach(seg: Rep[Segment]): Rep[Segment] = self
  }
  trait CenteredCompanion
}
