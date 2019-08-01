package scalan.primitives

import java.math.BigInteger

import scalan.{Base, Scalan}

trait NumericOps extends Base { self: Scalan =>
  implicit class NumericOpsCls[T](x: Rep[T])(implicit val n: Numeric[T]) {
    def +(y: Rep[T]) = NumericPlus(n)(x.elem).apply(x, y)
    def -(y: Rep[T]) = NumericMinus(n)(x.elem).apply(x, y)
    def *(y: Rep[T]) = NumericTimes(n)(x.elem).apply(x, y)
    def unary_- = NumericNegate(n)(x.elem).apply(x)
    def abs = Math.abs(x)
    def toFloat = NumericToFloat(n).apply(x)
    def toDouble = NumericToDouble(n).apply(x)
    def toInt = NumericToInt(n).apply(x)
    def toLong = NumericToLong(n).apply(x)
    def ceil = Math.ceil(toDouble)
    def floor = Math.floor(toDouble)
  }

  implicit class FractionalOpsCls[T](x: Rep[T])(implicit f: Fractional[T]) {
    def /(y: Rep[T]): Rep[T] = FractionalDivide(f)(x.elem).apply(x, y)
  }

  implicit class IntegralOpsCls[T](x: Rep[T])(implicit i: Integral[T]) {
    def div(y: Rep[T]): Rep[T] = IntegralDivide(i)(x.elem).apply(x, y)
    def mod(y: Rep[T]): Rep[T] = IntegralMod(i)(x.elem).apply(x, y)
    // avoid / due to conflicts
    def /!(y: Rep[T]): Rep[T] = div(y)
    def %(y: Rep[T]): Rep[T] = mod(y)
  }

  def numeric[T:Numeric]: Numeric[T] = implicitly[Numeric[T]]
  def fractional[T:Fractional]: Fractional[T] = implicitly[Fractional[T]]
  def integral[T:Integral]: Integral[T] = implicitly[Integral[T]]

  case class NumericPlus[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("+", n.plus)

  case class NumericMinus[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("-", n.minus)

  case class NumericTimes[T: Elem](n: Numeric[T]) extends EndoBinOp[T]("*", n.times)

  class DivOp[T: Elem](opName: String, applySeq: (T, T) => T, n: Numeric[T]) extends EndoBinOp[T](opName, applySeq) {
    override def shouldPropagate(lhs: T, rhs: T) = rhs != n.zero
  }

  case class NumericNegate[T: Elem](n: Numeric[T]) extends UnOp[T, T]("-", n.negate)

  case class NumericToDouble[T](n: Numeric[T]) extends UnOp[T,Double]("ToDouble", n.toDouble)

  case class NumericToFloat[T](n: Numeric[T]) extends UnOp[T, Float]("ToFloat", n.toFloat)

  case class NumericToInt[T](n: Numeric[T]) extends UnOp[T,Int]("ToInt", n.toInt)

  case class NumericToLong[T](n: Numeric[T]) extends UnOp[T,Long]("ToLong", n.toLong)

  case class FractionalDivide[T](f: Fractional[T])(implicit elem: Elem[T]) extends DivOp[T]("/", f.div, f)

  case class IntegralDivide[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("/", i.quot, i)

  case class IntegralMod[T](i: Integral[T])(implicit elem: Elem[T]) extends DivOp[T]("%", i.rem, i)

  case class NumericRand[T](bound: Rep[T], id: Int = IdSupply.nextId)(implicit val eT: Elem[T]) extends BaseDef[T] {
    override def transform(t: Transformer) = NumericRand(t(bound))
  }

  def random[T](bound: Rep[T])(implicit n: Numeric[T]): Rep[T] =
    NumericRand(bound)(bound.elem)

  @inline final def isZero[T](x: T, n: Numeric[T]) = x == n.zero
  @inline final def isOne[T](x: T, n: Numeric[T]) = x == n.fromInt(1)
  
}
