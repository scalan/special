package scalan.primitives

import scalan.{Base, Scalan}

trait Equal extends Base { self: Scalan =>
  case class Equals[A: Elem]() extends BinOp[A, Boolean]("==", equalValues[A](_, _))
  case class NotEquals[A: Elem]() extends BinOp[A, Boolean]("!=", !equalValues[A](_, _))

  def equalValues[A](x: Any, y: Any)(implicit eA: Elem[A]) = x == y

  implicit class EqualOps[A](x: Rep[A]) {
    implicit private val eA = x.elem
    def ===(y: Rep[A]): Rep[Boolean] = Equals[A].apply(x, y)
    def !==(y: Rep[A]): Rep[Boolean] = NotEquals[A].apply(x, y)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // rules for Equals
    case app @ ApplyBinOp(_: Equals[_], _, _) =>
      if (app.lhs == app.rhs) Const(true)
      else {
        app.rhs match {
          case Def(Const(b: Boolean)) if app.lhs.elem == BooleanElement =>
            if (b) app.lhs else Not(asRep[Boolean](app.lhs))
          case _ =>
            app.lhs match {
              case Def(Const(b: Boolean)) if app.rhs.elem == BooleanElement =>
                if (b) app.rhs else Not(asRep[Boolean](app.rhs))
              case _ =>
                super.rewriteDef(d)
            }
        }
      }

    // rules for NotEquals
    case ApplyBinOp(_: NotEquals[_], x, y) if x == y => false
    case ApplyBinOp(_: NotEquals[_], x, Def(Const(b: Boolean))) if x.elem == BooleanElement =>
      if (b) Not(asRep[Boolean](x)) else x
    case ApplyBinOp(_: NotEquals[_], Def(Const(b: Boolean)), x) if x.elem == BooleanElement =>
      if (b) Not(asRep[Boolean](x)) else x
    case _ => super.rewriteDef(d)
  }
}
