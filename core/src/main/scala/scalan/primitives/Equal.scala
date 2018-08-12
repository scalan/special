package scalan.primitives

import scalan.{Base, Scalan}

trait Equal extends Base { self: Scalan =>
  case class Equals[A: Elem]() extends BinOp[A, Boolean]("==", equalValues[A](_, _))
  case class NotEquals[A: Elem]() extends BinOp[A, Boolean]("!=", !equalValues[A](_, _))

  def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = x == y

  implicit class EqualOps[A](x: Rep[A]) {
    implicit val eA = x.elem
    def ===(y: Rep[A]): Rep[Boolean] = Equals[A].apply(x, y)
    def !==(y: Rep[A]): Rep[Boolean] = NotEquals[A].apply(x, y)
  }
  override def rewriteDef[T](d: Def[T]) = d match {
    // rules for Equals
    case ApplyBinOp(_: Equals[_], x, y) if x == y => true
    case ApplyBinOp(_: Equals[_], x, Def(Const(b: Boolean))) if x.elem == BooleanElement =>
      if (b) x else !x.asRep[Boolean]
    case ApplyBinOp(_: Equals[_], Def(Const(b: Boolean)), x) if x.elem == BooleanElement =>
      if (b) x else !x.asRep[Boolean]
    // rules for NotEquals
    case ApplyBinOp(_: NotEquals[_], x, y) if x == y => false
    case ApplyBinOp(_: NotEquals[_], x, Def(Const(b: Boolean))) if x.elem == BooleanElement =>
      if (b) !x.asRep[Boolean] else x
    case ApplyBinOp(_: NotEquals[_], Def(Const(b: Boolean)), x) if x.elem == BooleanElement =>
      if (b) !x.asRep[Boolean] else x
    case _ => super.rewriteDef(d)
  }
}
