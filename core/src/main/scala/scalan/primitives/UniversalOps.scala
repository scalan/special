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

  /** Special graph node to represent accumulation of the operation costs.
    * In general, due to node sharing it is incorrect to just sum up all the `args` costs
    * and add `resCost` to that value.
    * Example: <br>
    * <code>
    * val x = ..
    * val y = op1(x)
    * val z = op2(x)
    * val res = op3(y, z)
    * </code>
    * The naive summation will lead to the cost of x` is accumulated both into `cost of y`
    * and into `cost of z`, so in the `cost of res` it is accumulated twice.
    * To avoid this problem OpCost nodes require special handling in during evaluation.
    *
    * @param  args    costs of the arguments, which are here represent dependency information.
    * @param  opCost operation cost, which should be added to the currently accumulated cost
    * @see `Evaluation`
    */
  case class OpCost(costedValueId: Int, args: Seq[Rep[Int]], opCost: Rep[Int]) extends BaseDef[Int] {
    override def transform(t: Transformer) = OpCost(costedValueId, t(args), t(opCost))
  }
  def opCost(costedValue: Sym, args: Seq[Rep[Int]], opCost: Rep[Int]): Rep[Int] = {
    val id = costedValue.rhs.nodeId
    OpCost(id, args, opCost)
  }

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
