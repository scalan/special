package scalan.primitives

import scalan.{Base, Scalan}

trait IfThenElse extends Base { self: Scalan =>

  /** If c then t else e construction with standard lazy evaluation of branches.
    * The representation uses Thunk for each branch */
  def IF(cond: Rep[Boolean]): IfBranch = new IfBranch(cond)

  class IfBranch(cond: Rep[Boolean]) {
    def apply[T](thenp: => Rep[T]) = THEN(thenp)

    def THEN[T](thenp: => Rep[T]) = new ThenIfBranch[T](cond, thenp)
  }

  class ElseIfBranch[T](cond: Rep[Boolean], outer: ThenIfBranch[T]) {
    def apply(thenp: => Rep[T]) = THEN(thenp)

    def THEN(thenp: => Rep[T]) = new ThenIfBranch[T](cond, thenp) {
      override def ELSE(elsep: => Rep[T]) = outer.elseIf(cond, thenp, elsep)
    }
  }

  class ThenIfBranch[T](cond: Rep[Boolean], thenp: => Rep[T]) {
    def ELSE(elsep: => Rep[T]): Rep[T] = ifThenElseLazy(cond, thenp, elsep)

    def elseIf(cond1: => Rep[Boolean], thenp1: => Rep[T], elsep1: => Rep[T]) =
      ELSE(ifThenElseLazy(cond1, thenp1, elsep1))

    def ELSEIF(cond1: => Rep[Boolean]) = new ElseIfBranch[T](cond1, this)
  }

  case class IfThenElseLazy[T](cond: Rep[Boolean], thenp: Rep[Thunk[T]], elsep: Rep[Thunk[T]]) extends Def[T] {
    lazy val selfType = {
      val eThen = thenp.elem.eItem
      val eElse = elsep.elem.eItem
      eThen.leastUpperBound(eElse).asElem[T]
    }
    override def transform(t: Transformer) = IfThenElseLazy(t(cond), t(thenp), t(elsep))
  }

  def ifThenElseLazy[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T] = {
    val t = Thunk(thenp)
    val e = Thunk(elsep)
    IfThenElseLazy(cond, t, e)
  }

}
