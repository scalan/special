package special.collection

trait Costed[Val] {
  def builder: CostedBuilder
  def value: Val
  def cost: Int
  def dataSize: Long
}

trait CostedBuilder {
  def ConstructTupleCost: Int = 1
  def ConstructSumCost: Int = 1
  def SumTagSize: Int = 1
}


