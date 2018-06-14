package scalan.collection

trait Costed[T] {
  def builder: CostedBuilder
  def value: T
  def cost: Long
}

trait CostedBuilder {
}


