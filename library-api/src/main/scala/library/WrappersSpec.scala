package library

import scala.reflect.ClassTag

class ArrayWrappersSpec {
  def zip[A,B](xs: Array[A], ys: Array[B]): Array[(A,B)] = xs.zip(ys)
  def map[A,B](xs: Array[A], f: A => B) = xs.map(f)
  def length[A](xs: Array[A]) = xs.length
  def fill[A:ClassTag](n: Int, elem: =>A): Array[A] = Array.fill(n)(elem)
  def slice[A](xs: Array[A], from: Int, until: Int): Array[A] = xs.slice(from, until)
  def foldLeft[A, B](xs: Array[A], zero: B, op: (B, A) => B): B = xs.foldLeft(zero)(op)
  def filter[A](xs: Array[A], p: A => Boolean): Array[A] = xs.filter(p)
  def forall[A](xs: Array[A], p: A => Boolean): Boolean = xs.forall(p)
  def exists[A](xs: Array[A], p: A => Boolean): Boolean = xs.exists(p)
  def foreach[A](xs: Array[A], p: A => Unit): Unit = xs.foreach(p)
  def apply[A](xs: Array[A], i: Int): A = xs.apply(i)
};

