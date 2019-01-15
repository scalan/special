package special.collections

import scalan.BaseTests
import special.collection.CollOverArrayBuilder

class CollsTests extends BaseTests {
  val builder = new CollOverArrayBuilder
  val xs = builder.fromItems(1, 2, 3)
  val ys = builder.fromItems(1L, 2L, 3L, 4L)
  val zs = builder.fromItems(true, false)

  test("Coll.indices") {
    xs.indices.arr shouldBe Array(0, 1, 2)
    builder.replicate(3, 10).indices.arr shouldBe Array(0,1,2)
    ys.zip(xs).indices.arr shouldBe Array(0,1,2,3) // TODO should use min length
  }

  test("Coll.flatMap") {
    val flags = zs.map(_ => zs).arr
    flags shouldBe Array(true, false, true, false)
    val matrix = zs.map(_ => xs)
    val res = zs.zip(matrix).flatMap(_._2)
    res.arr shouldBe Array(1,2,3, 1,2,3)
  }
}
