package special.collections

import scalan.BaseTests
import special.collection.CollOverArrayBuilder
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

trait CollGenerators {

}

class CollsTests extends PropSpec with PropertyChecks with Matchers with CollGenerators {
  val builder = new CollOverArrayBuilder
  val xs = builder.fromItems(1, 2, 3)
  val ys = builder.fromItems(1L, 2L, 3L, 4L)
  val zs = builder.fromItems(true, false)
  val tens = builder.replicate(3, 10)

  property("Coll.indices") {
    xs.indices.arr shouldBe Array(0, 1, 2)
    tens.indices.arr shouldBe Array(0,1,2)
    ys.zip(xs).indices.arr shouldBe Array(0,1,2,3) // TODO should use min length
  }

  property("Coll.flatMap") {
    val flags = zs.map(_ => zs).arr
    flags shouldBe Array(true, false, true, false)
    val matrix = zs.map(_ => xs)
    val res = zs.zip(matrix).flatMap(_._2)
    res.arr shouldBe Array(1,2,3, 1,2,3)
  }

  property("Coll.segmentLength") {
    tens.segmentLength(_ == 10, 1) shouldBe 2
    tens.segmentLength(_ == 11, 1) shouldBe 0
    tens.segmentLength(_ == 11, 3) shouldBe 0
    ys.segmentLength(_ < 3, 0) shouldBe 2
  }

  property("Coll.indexWhere") {
    forAll { (xs: Array[Int], from: Int) =>
      val col = builder.fromArray(xs)
      def p(x: Int) = x == 0
      col.indexWhere(p, from) shouldBe xs.indexWhere(p, from)
      def p2(ab: (Int, Int)) = p(ab._1) && p(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe xs.zip(xs).indexWhere(p2, from)
    }
    forAll { (l: Int, v: Int, from: Int) => whenever(0 <= l && l < 100) {
      val col = builder.replicate(l, v)
      def p(x: Int) = x == 0
      col.indexWhere(p, from) shouldBe col.arr.indexWhere(p, from)
    }}
  }

  property("Coll.lastIndexWhere") {
    forAll { (xs: Array[Int], from: Int) =>
      val col = builder.fromArray(xs)
      def p(x: Int) = x == 0
      col.lastIndexWhere(p, from) shouldBe xs.lastIndexWhere(p, from)
      def p2(ab: (Int, Int)) = p(ab._1) && p(ab._2)
      col.zip(col).lastIndexWhere(p2, from) shouldBe xs.zip(xs).lastIndexWhere(p2, from)
    }
    forAll { (l: Int, v: Int, from: Int) => whenever(0 <= l && l < 100) {
      val col = builder.replicate(l, v)
      def p(x: Int) = x == 0
      col.lastIndexWhere(p, from) shouldBe col.arr.lastIndexWhere(p, from)
    }}
  }

  property("Coll.partition") {
    forAll { (xs: Array[Int]) =>
      val col = builder.fromArray(xs)
      def p(x: Int) = x > 0
      val (lsC, rsC) = col.partition(p)
      val (ls, rs) = xs.partition(p)
      lsC.arr shouldBe ls
      rsC.arr shouldBe rs
    }
    forAll { (l: Int, v: Int, from: Int) => whenever(0 <= l && l < 100) {
      val col = builder.replicate(l, v)
      def p(x: Int) = x > 0
      val (lsC, rsC) = col.partition(p)
      val (ls, rs) = col.arr.partition(p)
      lsC.arr shouldBe ls
      rsC.arr shouldBe rs
    }}
  }

  property("Coll.patch") {
    forAll { (xs: Array[Int], from: Int, patch: Array[Int], replaced: Int) =>
      whenever(0 <= from && from < xs.length && 0 <= replaced) {
        val col = builder.fromArray(xs)
        val patchedC = col.patch(from, builder.fromArray(patch), replaced)
        val patched = xs.patch(from, patch, replaced)
        patchedC.arr shouldBe patched
      }
    }
  }
}
