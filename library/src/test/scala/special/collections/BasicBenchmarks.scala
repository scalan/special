package special.collections

import org.scalameter.api._
import spire.syntax.all.cfor

trait BasicBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>
  performance of "Seq" in {
    var res: Seq[Int] = null
    measure method "Nil" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Nil }
      }
    }
    measure method "empty" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Seq.empty }
      }
    }
    measure method "apply" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Seq() }
      }
    }
  }

  performance of "Map" in {
    var res: Map[Int,Int] = null
    measure method "empty" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Map.empty[Int,Int] }
      }
    }
    measure method "apply" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ =>
          res = Map[Int,Int]()
        }
      }
    }
  }

  performance of "for" in {
    var cell: Int = 0
    measure method "foreach" in {
      using(arrays) in { case (xs, _) =>
        xs.foreach { x => cell = x }
      }
    }
    measure method "cfor" in {
      using(arrays) in { case (xs, _) =>
        cfor(0)(_ < xs.length, _ + 1) { i => cell = xs(i) }
      }
    }
  }
}

object FastBasicBenchmark extends Bench.LocalTime with BasicBenchmarkCases {
}

