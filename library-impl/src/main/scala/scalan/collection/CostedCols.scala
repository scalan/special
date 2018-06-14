package scalan.collection

import scala.reflect.ClassTag

class CostedPrim[T](val value: T, val cost: Long) extends Costed[T] {
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

class CostedArray[T: ClassTag](val arr: Col[Costed[T]]) extends Costed[Array[T]] {
  def builder = new ConcreteCostedBuilder
  def value: Array[T] = arr.map(c => c.value).arr
  def cost: Long = arr.map(c => c.cost).fold(0L)((x,y) => x + y)
}

class ConcreteCostedBuilder extends CostedBuilder {
}
