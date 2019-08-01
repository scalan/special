package scalan.primitives

import scalan.{Base, Scalan}

trait LogicalOps extends Base { self: Scalan =>
  val And = new EndoBinOp[Boolean]("&&", _ && _)

  val Or = new EndoBinOp[Boolean]("||", _ || _)

  val Not = new EndoUnOp[Boolean]("!", !_)

  val BinaryXorOp = new EndoBinOp[Boolean]("^", _ ^ _)

  val BooleanToInt = new UnOp[Boolean, Int]("ToInt", if (_) 1 else 0)

  implicit class RepBooleanOps(value: Rep[Boolean]) {
    def &&(y: Rep[Boolean]): Rep[Boolean] = And(value, y)
    def ||(y: Rep[Boolean]): Rep[Boolean] = Or(value, y)
    def ^(y: Rep[Boolean]): Rep[Boolean] = BinaryXorOp(value, y)

    def lazy_&&(y: Rep[Thunk[Boolean]]): Rep[Boolean] = And.applyLazy(value, y)
    def lazy_||(y: Rep[Thunk[Boolean]]): Rep[Boolean] = Or.applyLazy(value, y)

    def unary_!() : Rep[Boolean] = Not(value)
    def toInt: Rep[Int] = BooleanToInt(value)
  }

  implicit class BooleanFuncOps[A](f: Rep[A => Boolean]) {
    def &&&(g: Rep[A => Boolean]) =
      if (f == g)
        f
      else
        composeBi(f, g)(_ && _)

    def |||(g: Rep[A => Boolean]) =
      if (f == g)
        f
      else
        composeBi(f, g)(_ || _)

    def !!! = sameArgFun(f) { x => !f(x) }
  }

  @inline
  final def rewriteBoolConsts(d: Def[_], lhs: Sym, rhs: Sym, ifTrue: Sym => Sym, ifFalse: Sym => Sym, ifEqual: Sym => Sym, ifNegated: Sym => Sym): Sym =
    lhs match {
      // op(x, x)
      case `rhs` =>
        ifEqual(lhs)

      // op(!x, x) => ifNegated(!x)
      case Def(ApplyUnOp(op, `rhs`)) if op == Not =>
        ifNegated(lhs)

      // op(true, x) => ifTrue(x) | op(false, x) => ifFalse(x)
      case Def(Const(b: Boolean)) =>
        if (b) ifTrue(rhs) else ifFalse(rhs)

      case _ =>
        rhs match {
          // op(x, true) => ifTrue(x) | op(false, x) => ifFalse(x)
          case Def(Const(b: Boolean)) =>
            if (b) ifTrue(lhs) else ifFalse(lhs)

          // op(x, !x) => ifNegated(!x)
          case Def(ApplyUnOp(op, `lhs`)) if op == Not =>
            ifNegated(rhs)
          case _ => null
        }
    }
}
