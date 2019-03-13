package scalan.primitives

import scalan.{AVHashMap, Base, Scalan, Nullable}

trait UniversalOps extends Base { self: Scalan =>
  case class HashCode[A]() extends UnOp[A, Int]("hashCode", _.hashCode)

  case class ToString[A]() extends UnOp[A, String]("toString", _.toString)

  /** Represents calculation of size in bytes of the given value.
    * The descriptor value.elem can be used to decompose value into components.
    */
  case class SizeOf[T](value: Rep[T]) extends BaseDef[Long] {
    override def transform(t: Transformer) = SizeOf(t(value))
  }

  def sizeOf[T](value: Rep[T]): Rep[Long] = SizeOf(value)

  case class Downcast[From, To](input: Rep[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Downcast(t(input), eTo)
  }
  case class Upcast[From, To](input: Rep[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Upcast(t(input), eTo)
  }

  def downcast[To:Elem](value: Rep[_]): Rep[To] = Downcast(value, element[To])
  def upcast[To:Elem](value: Rep[_]): Rep[To] = Upcast(value, element[To])

  implicit class RepUniversalOps[A](x: Rep[A]) {
    def hashCodeRep: Rep[Int] = HashCode[A]().apply(x)
    def toStringRep = ToString[A]().apply(x)
  }
  override def rewriteDef[T](d: Def[T]) = d match {
    case ApplyUnOp(ToString(), x) if x.elem == StringElement => x
    case _ => super.rewriteDef(d)
  }
}
