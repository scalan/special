package special.collections

import scala.collection.mutable.ArrayBuffer
import org.scalacheck.util.Buildable

import scala.collection.mutable
import org.scalacheck.{Arbitrary, Gen}
import scalan._
import special.collection.{Coll, CollOverArrayBuilder, CollBuilder}

import scala.reflect.ClassTag

trait CollGens { testSuite =>
  import Gen._
  val builder: CollBuilder = new CollOverArrayBuilder
  val monoid = builder.Monoids.intPlusMonoid
  val valGen = choose(-100, 100)
  val indexGen = choose(0, 100)
  val replacedGen = choose(0, 100)
  val lenGen = choose(0, 100)

  val arrayGen: Gen[Array[Int]] = containerOfN[Array, Int](100, valGen)
  val indexesGen = containerOfN[Array, Int](10, indexGen).map(arr => builder.fromArray(arr.distinct.sorted))

  val collOverArrayGen = arrayGen.map(builder.fromArray(_))
  val replCollGen = for { l <- lenGen; v <- valGen } yield builder.replicate(l, v)
  val collGen = Gen.oneOf(collOverArrayGen, replCollGen)

  implicit val arbColl = Arbitrary(collGen)

  def eq0(x: Int) = x == 0
  def lt0(x: Int) = x < 0
  def plus(acc: Int, x: Int): Int = acc + x
  val plusF = (p: (Int,Int)) => plus(p._1, p._2)
  def inc(x: Int) = x + 1

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
