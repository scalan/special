package special.collections

import scalan.BaseTests
import special.collection.{Coll, CollOverArrayBuilder}
import org.scalacheck.Arbitrary._
import org.scalacheck.util.Buildable
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait CollGenerators {

}

class CollsTests extends PropSpec with PropertyChecks with Matchers with CollGenerators { testSuite =>
  import Gen._

  val builder = new CollOverArrayBuilder

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

  implicit def buildableColl[T:ClassTag] = new Buildable[T,Coll[T]] {
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

  implicit def traversableColl[T](coll: Coll[T]): mutable.Traversable[T] = coll.arr

  property("Coll.indices") {
    forAll(collGen, collGen) { (col1: Coll[Int], col2: Coll[Int]) =>
      col1.indices.arr shouldBe col1.arr.indices.toArray
// TODO     col1.zip(col2).indices.arr shouldBe col1.arr.zip(col2.arr).indices.toArray
    }
  }

  property("Coll.flatMap") {
    forAll(containerOfN[Coll, Int](3, valGen), collGen) { (zs, col) =>
      val matrix = zs.map(_ => col)
      val res = zs.zip(matrix).flatMap(_._2)
      res.arr shouldBe zs.arr.flatMap(_ => col.arr)
    }
  }

  property("Coll.segmentLength") {
    forAll(collGen, indexGen) { (col, from) =>
      col.segmentLength(lt0, from) shouldBe col.arr.segmentLength(lt0, from)
    }
  }

  property("Coll.indexWhere") {
    forAll(collGen, indexGen) { (col, from) =>
      col.indexWhere(eq0, from) shouldBe col.arr.indexWhere(eq0, from)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe col.arr.zip(col.arr).indexWhere(p2, from)
    }
  }

  property("Coll.lastIndexWhere") {
    forAll(collGen, indexGen) { (col, end) =>
      col.lastIndexWhere(eq0, end) shouldBe col.lastIndexWhere(eq0, end)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).lastIndexWhere(p2, end) shouldBe col.arr.zip(col.arr).lastIndexWhere(p2, end)
    }
  }

  property("Coll.partition") {
    forAll(collGen) { col =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.arr.partition(lt0)
      lsC.arr shouldBe ls
      rsC.arr shouldBe rs
    }
  }

  property("Coll.patch") {
    forAll(collGen, choose(-100, 100), collGen, replacedGen) { (col, from, patch, replaced) =>
      whenever(from < col.length ) {
        val patchedC = col.patch(from, patch, replaced)
        val patched = col.arr.patch(from, patch.arr, replaced)
        patchedC.arr shouldBe patched
      }
    }
  }

  property("Coll.updated") {
    forAll(collGen, indexGen, valGen) { (col, index, elem) =>
      whenever(index < col.length ) {
        val patchedC = col.updated(index, elem)
        val patched = col.arr.updated(index, elem)
        patchedC.arr shouldBe patched
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updated(col.length, elem)
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updated(-1, elem)
      }
    }
  }

  property("Coll.updateMany") {
    forAll(collGen, indexesGen) { (col, indexes) =>
      whenever(indexes.forall(_ < col.length)) {
        val updatedC = col.updateMany(indexes, indexes)
        val updated = col.arr.clone()
        for (i <- indexes)
          updated.update(i, i)
        updatedC.arr shouldBe updated
      }
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updateMany(builder.fromItems(col.length), builder.fromItems(0))
      }      
      an[IndexOutOfBoundsException] should be thrownBy {
        col.updateMany(builder.fromItems(-1), builder.fromItems(0))
      }
    }
  }
}
