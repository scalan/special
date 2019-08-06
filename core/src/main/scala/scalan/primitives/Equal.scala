package scalan.primitives

import scalan.{Base, Scalan}

trait Equal extends Base { self: Scalan =>
  case class Equals[A: Elem]() extends BinOp[A, Boolean]("==", equalValues[A](_, _))
  case class NotEquals[A: Elem]() extends BinOp[A, Boolean]("!=", !equalValues[A](_, _))

  def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = x == y

  implicit class EqualOps[A](x: Ref[A]) {
    implicit private val eA = x.elem
    def ===(y: Ref[A]): Ref[Boolean] = Equals[A].apply(x, y)
    def !==(y: Ref[A]): Ref[Boolean] = NotEquals[A].apply(x, y)
  }
}
