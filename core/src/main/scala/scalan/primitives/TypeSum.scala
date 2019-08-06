package scalan.primitives

import scalan.{Base, Scalan}

trait TypeSum extends Base { self: Scalan =>

  trait SumOps[A, B] {
    def isLeft: Ref[Boolean]

    def isRight: Ref[Boolean]

    def fold[R: Elem](l: Ref[A] => Ref[R], r: Ref[B] => Ref[R]): Ref[R]

    def foldBy[R](l: Ref[A => R], r: Ref[B => R]): Ref[R]

    def mapSum[C: Elem, D: Elem](fl: Ref[A] => Ref[C], fr: Ref[B] => Ref[D]): Ref[C | D]

    def mapSumBy[C, D](fl: Ref[A => C], fr: Ref[B => D]): Ref[C | D]
  }

  implicit class RepExtensionsForSum[A](x: Ref[A]) {
    def asLeft[B: Elem]: Ref[A | B] = mkLeft[A, B](x)

    def asRight[B: Elem]: Ref[B | A] = mkRight[B, A](x)
  }

  implicit class JoinSumOps[A](sum: Ref[A | A]) {
    def joinSum: Ref[A] = {
      implicit val eA = sum.elem.eLeft
      sum.foldBy(identityFun, identityFun)
    }
  }

  implicit class OptionOps[A](opt: ROptional[A]) {
    implicit def eA: Elem[A] = opt.elem.eRight
    def map[B](f: Ref[A] => Ref[B]): ROptional[B] =
      mapBy(fun(f))

    def mapBy[B](f: Ref[A => B]): ROptional[B] =
      opt.mapSumBy(identityFun, f)

    def flatMap[B](f: Ref[A] => ROptional[B]): ROptional[B] =
      flatMapBy(fun(f))

    def flatMapBy[B](f: Ref[A => SOptional[B]]): ROptional[B] = {
      implicit val eB = f.elem.eRange.eRight
      opt.foldBy(constFun(SOptional.none[B]), f)
    }

    def getOrElse[B >: A](default: Ref[B]): Ref[B] =
      opt.foldBy(constFun(default), upcastFun[A,B])

    def isEmpty = opt.isLeft

    def isDefined = opt.isRight
  }

  type SOptional[A] = Unit | A
  type ROptional[A] = Ref[SOptional[A]]

  object SOptional {
    def none[A: Elem] = mkLeft[Unit, A](())

    def some[A](x: Ref[A]) = mkRight[Unit, A](x)
  }

  // TODO used by generated code; ideally should be unnecessary
  def sOptionElement[A: Elem] = element[SOptional[A]]

  case class SLeft[A, B](left: Ref[A], eRight: Elem[B]) extends BaseDef[A | B]()(sumElement(left.elem, eRight)) {
    override def transform(t: Transformer): Def[A | B] = SLeft(t(left), eRight)
  }

  case class SRight[A, B](right: Ref[B], eLeft: Elem[A]) extends BaseDef[A | B]()(sumElement(eLeft, right.elem)) {
    override def transform(t: Transformer): Def[A | B] = SRight(t(right), eLeft)
  }

  def mkLeft[A, B: Elem](a: Ref[A]): Ref[A | B] = SLeft[A, B](a, element[B])

  def mkRight[A: Elem, B](b: Ref[B]): Ref[A | B] = SRight[A, B](b, element[A])

  case class SumFold[A, B, R](sum: Ref[A | B], left: Ref[A => R], right: Ref[B => R])
    extends BaseDef[R]()(left.elem.eRange) {
    override def transform(t: Transformer): Def[R] = SumFold(t(sum), t(left), t(right))
  }

  case class SumMap[A, B, C, D](sum: Ref[A | B], left: Ref[A => C], right: Ref[B => D])
    extends BaseDef[C | D]()(sumElement(left.elem.eRange, right.elem.eRange)) {
    override def transform(t: Transformer): Def[C | D] = SumMap(t(sum), t(left), t(right))
  }

  class SumOpsExp[A, B](s: Ref[A | B]) extends SumOps[A, B] {
    implicit def eLeft: Elem[A] = s.elem.eLeft

    implicit def eRight: Elem[B] = s.elem.eRight

    def fold[R: Elem](l: Ref[A] => Ref[R], r: Ref[B] => Ref[R]): Ref[R] = foldBy(fun(l), fun(r))

    def foldBy[R](l: Ref[A => R], r: Ref[B => R]): Ref[R] = SumFold(s, l, r)

    def mapSum[C: Elem, D: Elem](fl: Ref[A] => Ref[C], fr: Ref[B] => Ref[D]) = mapSumBy(fun(fl), fun(fr))

    def mapSumBy[C, D](l: Ref[A => C], r: Ref[B => D]): Ref[C | D] = SumMap(s, l, r)

    def isLeft = foldBy(constFun(true), constFun(false))

    def isRight = foldBy(constFun(false), constFun(true))
  }

  implicit def pimpSum[A, B](s: Ref[A | B]): SumOps[A, B] = new SumOpsExp[A, B](s)

}


