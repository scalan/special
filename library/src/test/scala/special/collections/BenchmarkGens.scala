package special.collections

import org.scalameter.api.Gen

trait BenchmarkGens extends CollGens {
  val sizes = Gen.exponential("size")(10, 100000, 10)

  val ranges = for { size <- sizes } yield (0 until size, 100000 / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }

  val colls = arrays.map { case (arr, i) => (builder.fromArray(arr), i) }
}
