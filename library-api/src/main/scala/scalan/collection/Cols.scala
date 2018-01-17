package scalan.collection

import scala.reflect.ClassTag
import scalan.macros.typeclass

trait Col[A] {
  def builder: ColBuilder
  def arr: Array[A]
  def length: Int
  def apply(i: Int): A
  def map[B: ClassTag](f: A => B): Col[B]
  def zip[B](ys: Col[B]): PairCol[A, B] = builder(this, ys)
  //    def reduce(implicit m: NumMonoid[A]): A = arr.reduce(m.append)
}

trait PairCol[L,R] extends Col[(L,R)] {
  def ls: Col[L]
  def rs: Col[R]
}

trait ColBuilder {
  def apply[A,B](as: Col[A], bs: Col[B]): PairCol[A,B]
  def fromArray[T](arr: Array[T]): Col[T]
  def ddmvm(v: Array[Double]): Int = {
    val xs = Array.fill(v.length)(0)
    val c = xs.zip(v).map(d => d)
    c.length
  }
  def functorArg(arr: Array[Double])(evF: Functor[Array]) = evF.map(arr)(x => x + 1)
//  def useFunctor(arr: Array[Double]) = evF.map(arr)(x => x + 1)
}

@typeclass trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B)(implicit tB: ClassTag[B]): F[B]
}
