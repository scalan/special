package scalan.primitives

import scalan.Scalan

trait MathOps { self: Scalan =>
  object Math {
    def ceil(x: Ref[Double]) = MathCeil(x)
    def floor(x: Ref[Double]) = MathFloor(x)
    def exp(x: Ref[Double]) = MathExp(x)
    def log(x: Ref[Double]) = MathLog(x)
    def sqrt(x: Ref[Double]) = MathSqrt(x)
    def sin(x: Ref[Double]) = MathSin(x)
    def cos(x: Ref[Double]) = MathCos(x)
    def acos(x: Ref[Double]) = MathAcos(x)
    def atan(x: Ref[Double]) = MathAtan(x)
    def atan2(x: Ref[Double], y: Ref[Double]) = MathAtan2(x,y)
    def pow(x: Ref[Double], y: Ref[Double]) = MathPow(x,y)
    def tanh(x: Ref[Double]) = MathTanh(x)

    def max[A](x: Ref[A], y: Ref[A])(implicit e: Elem[A], n:Ordering[A]) = x.max(y)
    def min[A](x: Ref[A], y: Ref[A])(implicit e: Elem[A], n:Ordering[A]) = x.min(y)
    val Pi = toRep(math.Pi)
    val E = toRep(math.E)
    val MathCeil = new UnDoubleOp("Ceil", scala.math.ceil)

    val MathFloor = new UnDoubleOp("Floor", scala.math.floor)

    val MathExp = new UnDoubleOp("Exp", scala.math.exp)

    val MathLog = new UnDoubleOp("Log", scala.math.log)

    val MathSqrt = new UnDoubleOp("Sqrt", scala.math.sqrt)

    val MathSin = new UnDoubleOp("Sin", scala.math.sin)

    val MathCos = new UnDoubleOp("Cos", scala.math.cos)

    val MathAcos = new UnDoubleOp("Acos", scala.math.acos)

    val MathAtan = new UnDoubleOp("Atan", scala.math.atan)

    val MathAtan2 = new BinDoubleOp("Atan2", scala.math.atan2)

    val MathPow = new BinDoubleOp("Pow", scala.math.pow)

    val MathTanh = new UnDoubleOp("Tanh", scala.math.tanh)
  }

  type UnDoubleOp = UnOp[Double, Double]

  type BinDoubleOp = BinOp[Double, Double]


}
