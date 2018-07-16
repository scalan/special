package special

import scalan.Builtin

@Builtin("IsoUR")
trait Iso[A,B] {
  def to(a: A): B
  def from(b: B): A
  val fromFun: B => A = b => from(b)
  val tofun: A => B = a => to(a)
}

