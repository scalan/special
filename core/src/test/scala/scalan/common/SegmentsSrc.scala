package scalan.common

trait Segment {
  def start: Int
  def length: Int
  def end: Int
  def shift(ofs: Int): Segment
  def attach(seg: Segment): Segment
}

case class Interval(val start: Int, val end: Int) extends Segment { self =>
  def length = end - start
  def shift(ofs: Int) = Interval(start + ofs, end + ofs)
  def attach(seg: Segment): Segment = seg match {
    case Interval(start, end) =>
      seg
    case Slice(start, length) =>
      self
    case Centered(center, radius) =>
      self
    case _ => seg attach self
  }
}

case class Slice(val start: Int, val length: Int) extends Segment { self =>
  def end = start + length
  def shift(ofs: Int) = Slice(start + ofs, length)
  def attach(seg: Segment): Segment = self
}

case class Centered(val center: Int, val radius: Int) extends Segment { self =>
  def start = center - radius
  def end = center + radius
  def length = radius * 2
  def shift(ofs: Int) = Centered(center + ofs, radius)
  def attach(seg: Segment): Segment = self
}
