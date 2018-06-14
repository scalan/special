package scalan.collection

trait Costed[Val] {
  def builder: CostedBuilder
  def value: Val
  def cost: Long
}

trait CostedBuilder {
}


