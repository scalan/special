package scalan.primitives

import scalan.{Base, Scalan}

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

  /** Represents information about calculation of size (i.e. size formula) in bytes of the given type structure.
    * The descriptor value.elem can be used to decompose value into constructors components.
    * Example:
    * value.elem match {
    *   case ce: CollElem[a,_] if ce.eItem == LongElement => ... // here we know T represents size of collection
    *   case LongElement => ... // here we know T represents size of some primitive value (non-constructed type)
    * }
    * @tparam TVal    type of the value which is sized
    * @tparam TInfo   type of the size info representation. For example if TVal == Coll[A] then TInfo == Coll[Long]
    */
  case class SizeData[TVal, TInfo](value: Rep[TVal], sizeInfo: Rep[TInfo]) extends BaseDef[Long] {
    override def transform(t: Transformer) = SizeData(t(value), t(sizeInfo))
  }

  def dataSize[TVal, TSize](value: Rep[TVal], size: Rep[TSize]): Rep[Long] = SizeData(value, size)

  /** Transform size formula into size computation graph.
    * The resulting graph should be free from SizeData nodes. */
  def calcSize[V,S](data: SizeData[V, S]): Rep[Long] = data.value.elem match {
    case _: BaseElem[_] => asRep[Long](data.sizeInfo)
    case pe: PairElem[a,b] =>
      val info = asRep[(Long, Long)](data.sizeInfo)
      info._1 + info._2
    case se: StructElem[_] =>
      val info = asRep[Struct](data.sizeInfo)
      val zero = toRep[Long](0L)
      val size = se.fields match {
        case fields if fields.isEmpty => zero
        case fields =>
          fields.foldLeft(zero) { case (acc, (fn,_)) => acc + info.get[Long](fn) }
      }
      size
    case _ => !!!(s"Don't know how to calcSize($data)", data.value)
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
