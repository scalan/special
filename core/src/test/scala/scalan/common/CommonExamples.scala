package scalan.common

import scalan.Scalan

trait CommonExamples extends Scalan with SegmentsModule {
  import Segment._
  import Slice._
  import Interval._
  import Centered._
  lazy val t1 = fun { (in: Rep[Interval]) => in.convertTo[Slice] }
  lazy val t2 = fun { (in: Rep[Slice]) => in.convertTo[Interval] }
  lazy val t3 = fun { (in: Rep[IntervalData]) => RInterval(in).convertTo[Slice].toData }
  lazy val t4 = fun { (in: Rep[SliceData]) => RSlice(in).convertTo[Interval].toData }
  lazy val t5 = fun { (in: Rep[CenteredData]) => RCentered(in).convertTo[Interval].toData }
  lazy val t6 = fun { (in: Rep[IntervalData]) => RInterval(in).convertTo[Centered].toData }
  lazy val t7 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN { RInterval(in):RSeg } ELSE { RSlice(0, l):RSeg }
    res.length
  }
  lazy val t8 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val Pair(i, res) = IF (s < 0) THEN { Pair(1, RInterval(in):RSeg) } ELSE { Pair(2, RSlice(0, l):RSeg) }
    i + res.length
  }
  lazy val t9 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val segm = IF (s < 0) THEN { RInterval(in):RSeg } ELSE { RSlice(0, l):RSeg }
    val res = IF (l > 10) THEN { segm.shift(1) } ELSE { RSlice(0, l):RSeg }
    res.length
  }
  lazy val t10 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN { (RInterval(in): RSeg).asRight[Unit] } ELSE { (RSlice(0, l): RSeg).asRight[Unit] }
    res.fold(_ => 0, s => s.length)
  }
  lazy val t10_1 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN { (RInterval(in): RSeg).asLeft[Unit] } ELSE { (RSlice(0, l): RSeg).asLeft[Unit] }
    res.fold(s => s.length, _ => 0)
  }
  lazy val t10_2 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN {
      (RInterval(in):RSeg).asLeft[Segment]
    } ELSE {
      (RSlice(0, l):RSeg).asRight[Segment]
    }
    res.fold(s => s.length, _ => 0)
  }
  lazy val t10_3 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN {
      (RInterval(in):RSeg).asRight[Segment]
    } ELSE {
      (RSlice(0, l):RSeg).asLeft[Segment]
    }
    res.fold(s => s.length, _ => 0)
  }
  lazy val t10_4 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN {
      (RInterval(in)).asLeft[Slice]
    } ELSE {
      (RSlice(0, l)).asRight[Interval]
    }
    res.fold(s => s.length, _ => 0)
  }
  lazy val t10_5 = fun { (in: Rep[Segment]) =>
    val s = in.start
    val l = in.length
    val res = IF (s < 0) THEN {
      Pair(s,l).asLeft[Segment]
    } ELSE {
      in.asRight[(Int,Int)]
    }
    res.fold(_ => 0, r => r.length)
  }
  lazy val t11 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN {
      (RInterval(in):RSeg).asLeft[Segment]
    } ELSE {
      (RSlice(0, l):RSeg).asRight[Segment]
    }
    res.fold(l => l.length, r => r.length)
  }
  lazy val t12 = fun { (in: Rep[IntervalData]) =>
    val Pair(s, l) = in
    val res = IF (s < 0) THEN {
      (RInterval(in):RSeg).asRight[Segment]
    } ELSE {
      (RSlice(0, l):RSeg).asLeft[Segment]
    }
    res.fold(l => l.length, r => r.length)
  }

//  lazy val t13 = fun { (in: Rep[(Int|Array[Int])]) =>
//    val res = in.fold(i => Interval(-1,i):RSeg, a => Slice(0, a.length):RSeg)
//    res.length
//  }
}
