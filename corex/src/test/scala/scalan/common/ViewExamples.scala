package scalan.common

import scalan.Scalan

trait ViewExamples extends Scalan with SegmentsModule {
  import Slice._
  import Interval._
  lazy val v1 = fun { (in: Ref[Interval]) => in }
  lazy val v2 = fun { (in: Ref[Interval]) => Pair(in,in) }
  lazy val v3 = fun { (in: Ref[Interval]) => Pair(in, in.length) }
  lazy val v4 = fun { (in: Ref[Interval]) => Pair(in.length, in) }
  lazy val v5 = fun { (in: Ref[Interval]) => in.length }
  lazy val v6 = fun { (in: Ref[Interval]) => Pair(in.length, 1) }
  lazy val v7 = fun { (in: Ref[Interval]) => Pair(in.length, 1) }

  lazy val v8 = fun { (in: Ref[Interval]) => in.asLeft[Unit] }
  lazy val v9 = fun { (in: Ref[Interval]) => in.asLeft[Slice] }
}
