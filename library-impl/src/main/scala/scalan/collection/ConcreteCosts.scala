package scalan.collection

import scala.reflect.ClassTag

class CostedPrim[Val](val value: Val, val cost: Long) extends Costed[Val] {
  def builder = new ConcreteCostedBuilder
}

class CostedPair[L,R](val l: L, val r: R, val cost: Long) extends Costed[(L,R)] {
  def builder = new ConcreteCostedBuilder
  def value: (L,R) = (l, r)
}

//class CostedOption[T](val either: Either[Long, Costed[T]]) extends Costed[Option[T]] {
//  def value: Option[T] = either.fold(l => None, r => r.value)
//  def cost = either.fold(l => l, r => r.cost)
//}

class CostedArray[Item: ClassTag](val values: Col[Item], val costs: Col[Long]) extends Costed[Array[Item]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[Item] = values.arr
  def cost: Long = costs.sum(builder.monoidBuilder.longPlusMonoid)
}

class ConcreteCostedBuilder extends CostedBuilder {
  def monoidBuilder = new MonoidBuilderInst
}
