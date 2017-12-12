package scalan.collection

import scala.reflect.ClassTag

class ColOverArray[A](val arr: Array[A]) extends Col[A] {
  def length = arr.length

  def apply(i: Int) = arr(i)

  def map[B: ClassTag](f: A => B): Col[B] = new ColOverArray(arr.map(f))
}

class ColOverArrayBuilder extends ColBuilder {
  def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray[T](arr)
}
//  object ColOverArray {
//    def fromArray[T](arr: Array[T]): Col[T] = new ColOverArray(arr)
//  }
//  class PairCol[A, B](val as: Col[A], val bs: Col[B]) extends Col[(A, B)] {
//    def arr: Array[(A, B)] = (as.arr zip bs.arr)
//    def length = as.length
//    def apply(i: Int) = (as(i), bs(i))
//  }

