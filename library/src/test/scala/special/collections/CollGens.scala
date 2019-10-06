package special.collections

import scala.collection.mutable.ArrayBuffer
import org.scalacheck.util.Buildable

import scala.collection.mutable
import org.scalacheck.Gen
import scalan._
import special.collection.{Coll, ReplColl}


trait CollGens extends RTypeGens { testSuite =>
  import Gen._

  val indexGen = choose(0, 100)

  val monoid = builder.Monoids.intPlusMonoid

  def hashCodeLt0[T](x: T) = x.hashCode() < 0
  def hashCodeInc[T](x: T) = x.hashCode() + 1
  def plusHashcode[T](p: (T,T)) = plus(p._1.hashCode(), p._2.hashCode())

  def eq0(x: Int) = x == 0
  def lt0(x: Int) = x < 0
  def plus(acc: Int, x: Int): Int = acc + x
  val plusF = (p: (Int,Int)) => plus(p._1, p._2)
  val predF = (p: (Int,Int)) => plus(p._1, p._2) > 0
  def inc(x: Int) = x + 1
  def dec(x: Int) = x - 1

  def collMatchRepl[B](coll: B): Boolean = coll match {
    case _ : ReplColl[_] => true
    case _ => false
  }

  implicit def buildableColl[T:RType] = new Buildable[T,Coll[T]] {
    def builder = new mutable.Builder[T,Coll[T]] {
      val al = new ArrayBuffer[T]
      def +=(x: T) = {
        al += x
        this
      }
      def clear() = al.clear()
      def result() = testSuite.builder.fromArray(al.toArray)
    }
  }

  implicit def traversableColl[T](coll: Coll[T]): mutable.Traversable[T] = coll.toArray

}
