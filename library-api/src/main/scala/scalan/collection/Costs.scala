package scalan.collection

trait Costed[Val] {
  def builder: CostedBuilder
  def value: Val
  def cost: Int
}

trait CostedBuilder {
}


