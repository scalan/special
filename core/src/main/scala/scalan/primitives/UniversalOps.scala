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
  case class SizeData[TVal, TInfo](eVal: Elem[TVal], sizeInfo: Rep[TInfo]) extends BaseDef[Long] {
    override def transform(t: Transformer) = SizeData(eVal, t(sizeInfo))
  }

  protected def sizeDataElem(size: Rep[Long]): Elem[_] = size match {
    case Def(SizeData(_, info)) => info.elem
    case _ => size.elem
  }

  protected def extractSizeData(size: Rep[Long]): Rep[_] = size match {
    case Def(SizeData(_, info)) => info
    case _ => size
  }

  protected def correctSizeDataType[TVal, TSize](eVal: Elem[TVal], eSize: Elem[TSize]): Boolean = eVal match {
    case e: BaseElem[_]   => eSize == LongElement
    case e: PairElem[_,_] => eSize.isInstanceOf[PairElem[_,_]]
    case e: SumElem[_,_]  => eSize.isInstanceOf[PairElem[_,_]]
    case _ => true
  }

  def msgIncorrectSizeDataStructure(eVal: Elem[_], eSize: Elem[_]) =
    s"SizeData should have the same structure with the data, but was ${eVal} and ${eSize}"

  def sizeData[TVal, TSize](eVal: Elem[TVal], size: Rep[TSize]): Rep[Long] = {
    val okTypes = correctSizeDataType(eVal, size.elem)
    assert(okTypes, msgIncorrectSizeDataStructure(eVal, size.elem))
    SizeData(eVal, size)
  }

  /** Transform size primitive into the correspoding size computation graph. */
  protected def calcSizeFromData[V,S](data: SizeData[V, S]): Rep[Long] = data.eVal match {
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
    case _ => !!!(s"Don't know how to calcSizeFromData($data): data.value.elem = ${data.eVal}, data.sizeInfo.elem = ${data.sizeInfo.elem}", data.sizeInfo)
  }

//  /** Eliminate SizeData nodes (if any) by resursively applying calcSizeFromData.
//    * The resulting graph should be free from SizeData nodes.
//    * Internally, keeps track of symbol mapping to make the procedure linear in the size of the graph.
//    */
//  def calcSizeGraph(sizeData: Rep[Long]): Rep[Long] = {
//    val mapping = AVHashMap[Rep[Long], Rep[Long]](16)
//    def calc(size: Rep[Long]): Rep[Long] = mapping.get(size) match {
//      case Nullable(mapped) => mapped
//      case _ => size match {
//        case Def(SizeData())
//      }
//    }
//    calc(sizeData)
//  }
//
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
