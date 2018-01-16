package scalan.collection

import scala.reflect.ClassTag

class ColOverArray[A](val arr: Array[A]) extends Col[A] {
  def builder = new ColOverArrayBuilder
  def length = arr.length
  def apply(i: Int) = arr(i)
  def map[B: ClassTag](f: A => B): Col[B] = new ColOverArray(arr.map(f))
}

class PairOfCols[L,R](val ls: Col[L], val rs: Col[R]) extends PairCol[L,R] {
  def builder: ColBuilder = new ColOverArrayBuilder
  override def arr: Array[(L, R)] = ls.arr.zip(rs.arr)
  override def length: Int = ls.length
  override def apply(i: Int): (L, R) = (ls(i), rs(i))
  override def map[V: ClassTag](f: ((L, R)) => V): Col[V] = new ColOverArray(arr.map(f))
}

class ColOverArrayBuilder extends ColBuilder {
  override def apply[A, B](as: Col[A], bs: Col[B]): PairCol[A, B] = new PairOfCols(as, bs)
  def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray[T](arr)
}

//class ArrayFunctor extends Functor[Array] {
//  override def map[A, B:ClassTag](fa: Array[A])(f: (A) => B): Array[B] = fa.map(f)
//}
//  object ColOverArray {
//    def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray(arr)
//  }
//  class PairCol[A, B](val as: Col[A], val bs: Col[B]) extends Col[(A, B)] {
//    def arr: Array[(A, B)] = (as.arr zip bs.arr)
//    def length = as.length
//    def apply(i: Int) = (as(i), bs(i))
//  }

