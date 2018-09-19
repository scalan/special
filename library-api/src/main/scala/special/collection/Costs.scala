package special.collection

import scalan.meta.RType

trait Costed[Val] {
  def builder: CostedBuilder
  def value: Val
  def cost: Int
  def dataSize: Long
}

trait CostedBuilder {
  def ConstructTupleCost: Int = 1
  def ConstructSumCost: Int = 1
  def SelectFieldCost: Int = 1
  def SumTagSize: Long = 1
  def costedValue[T](x: T, optCost: Option[Int])(implicit cT: RType[T]): Costed[T]
  def defaultValue[T](valueType: RType[T]): T
}


