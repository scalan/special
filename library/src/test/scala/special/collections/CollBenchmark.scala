package special.collections

import org.scalameter.api._
import special.collection.Coll

object CollBenchmark extends Bench.LocalTime with CollGens {
  val sizes = Gen.range("size")(3000, 15000, 3000)

  val ranges = for { size <- sizes } yield 0 until size
//  performance of "Range" in {
//    measure method "map" in {
//      using(ranges) in {
//        r => r.map(_ + 1)
//      }
//    }
//  }

  val arrays = ranges.map(r => r.toArray)

//  performance of "Array" in {
//    measure method "map" in {
//      using(arrays) in {
//        r => r.map(_ + 1)
//      }
//    }
//  }

  val colls = arrays.map(arr => builder.fromArray(arr))
//  performance of "Coll" in {
//    measure method "map" in {
//      using(colls) in {
//        r => r.map(_ + 1)
//      }
//    }
//  }

  performance of "PairArray" in {
    measure method "filter" in {
      using(arrays) in {
        arr => arr.zip(arr).filter(p => p._1 == p._2)
      }
    }
    measure method "map" in {
      using(arrays) in {
        arr => arr.zip(arr).map(p => p._1 + p._2)
      }
    }
    measure method "exists" in {
      using(arrays) in {
        arr => arr.zip(arr).exists(p => p._1 != p._2)
      }
    }
    measure method "fold" in {
      using(arrays) in {
        arr => arr.zip(arr).foldLeft(0)((b, p) => b + p._1 + p._2)
      }
    }
  }

  performance of "PairColl" in {
    measure method "filter" in {
      def doFilter(c: Coll[Int]) = c.zip(c).filter(p => p._1 == p._2)
      using(colls) in {
        c => doFilter(c)
      }
    }
    measure method "map" in {
      def doMap(c: Coll[Int]) = c.zip(c).map(p => (p._1 + p._2).toLong)
      using(colls) in {
        c => doMap(c)
      }
    }
    measure method "exists" in {
      using(colls) in {
        c => c.zip(c).exists(p => p._1 != p._2)
      }
    }
    measure method "fold" in {
      using(colls) in {
        c => c.zip(c).fold[Int](0, p => p._1 + p._2._1 + p._2._2)
      }
    }
  }
}

