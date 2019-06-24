package special.collections

import org.scalameter.{execution, Executor}
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scalan.util.CollectionUtil
import special.collection.Coll
import special.collection.ExtensionMethods._
import spire.syntax.all._

trait CollHashBenchmarkCases extends CollGens { suite: Bench[Double] =>
  val sizes = Gen.exponential("size")(10, 100000, 10)

  val ranges = for { size <- sizes } yield (0 until size, 100000 / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }

  val replColls = arrays.map { case (arr, i) => (builder.replicate(arr.length, (arr((arr.length - 1) / 2), 3)), i) }

  val byteColl = ranges.map { case (r, i) => (Array.fill[Byte](r.toArray.length)(Byte.MaxValue), i) }
  val shortColl = ranges.map { case (r, i) => (Array.fill[Short](r.toArray.length)(Short.MaxValue), i) }
  val intColl = ranges.map { case (r, i) => (Array.fill[Int](r.toArray.length)(Int.MaxValue), i) }
  val charColl = ranges.map { case (r, i) => (Array.fill[Char](r.toArray.length)(Char.MaxValue), i) }
  val longColl = ranges.map { case (r, i) => (Array.fill[Long](r.toArray.length)(Long.MaxValue), i) }
  val floatColl = ranges.map { case (r, i) => (Array.fill[Float](r.toArray.length)(Float.MaxValue), i) }
  val doubleColl = ranges.map { case (r, i) => (Array.fill[Double](r.toArray.length)(Double.MaxValue), i) }
  val boolColl = ranges.map { case (r, i) => (Array.fill[Boolean](r.toArray.length)(true), i) }
  val complexColl = ranges.map { case (r, i) => (builder.replicate(50, (
    1.toByte, ((100, (("string", builder.replicate(100, 2)),
    Array(1, 2, 3, 4))), (i, i)))), 100)
  }

  def oldDeepHashCode[T](arr: Array[T]): Int = {
    import java.util
    arr match {
      case arr: Array[AnyRef] => util.Arrays.deepHashCode(arr)
      case arr: Array[Byte] => util.Arrays.hashCode(arr)
      case arr: Array[Short] => util.Arrays.hashCode(arr)
      case arr: Array[Int] => util.Arrays.hashCode(arr)
      case arr: Array[Long] => util.Arrays.hashCode(arr)
      case arr: Array[Char] => util.Arrays.hashCode(arr)
      case arr: Array[Float] => util.Arrays.hashCode(arr)
      case arr: Array[Double] => util.Arrays.hashCode(arr)
      case arr: Array[Boolean] => util.Arrays.hashCode(arr)
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode" in {
      using(replColls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode" in {
      using(replColls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Byte" in {
      using(byteColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Byte" in {
      using(byteColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Short" in {
      using(shortColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Short" in {
      using(shortColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Int" in {
      using(intColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Int" in {
      using(intColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Char" in {
      using(charColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Char" in {
      using(charColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Float" in {
      using(floatColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Float" in {
      using(floatColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Double" in {
      using(doubleColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Double" in {
      using(doubleColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Boolean" in {
      using(boolColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Boolean" in {
      using(boolColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }

  performance of "hashCode" in {
    measure method "of old CReplColl.hashCode on Complex type" in {
      using(complexColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          oldDeepHashCode(c.toArray)
        }
      }
    }
    measure method "of new CReplColl.hashCode on Complex type" in {
      using(complexColl) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.hashCode()
        }
      }
    }
  }
}

object FastCollHashBenchmark extends Bench.LocalTime with CollHashBenchmarkCases {
}

object CollHashBenchmark extends Bench.OfflineRegressionReport with CollHashBenchmarkCases {
  override def executor: Executor[Double] = new execution.LocalExecutor(
    warmer,
    aggregator,
    measurer
  )
  //  lazy val executor = LocalExecutor(
  //    new Executor.Warmer.Default,
  //    Aggregator.min[Double],
  //    measurer)
  //  lazy val measurer = new Measurer.Default
  //  def reporter: Reporter[Double] = Reporter.Composite(
  //    new LoggingReporter[Double],
  ////    new RegressionReporter(
  ////      RegressionReporter.Tester.OverlapIntervals(),
  ////      RegressionReporter.Historian.ExponentialBackoff() ),
  //    HtmlReporter(true)
  //  )

  //  reports.resultDir -> "tmp1"
  //  lazy val reporter = HtmlReporter(true)
  //  lazy val reporter = new LoggingReporter[Double]

  //  lazy val persistor = Persistor.None
}

