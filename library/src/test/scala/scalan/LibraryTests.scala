package scalan

import scalan.util.BenchmarkUtil._

class Benchmark {
  def run() = {
    val (ctx, total) = measureTime {
      var ctx: Library = new Library {}
      measure(1000, false) { i =>
        ctx = new Library {}
      }
      ctx
    }
    println(s"Def count: ${ctx.defCount}, total: $total msec")
  }
}

class LibraryTests extends BaseCtxTests {
  test("Benchmark Library creation time") {
    new Benchmark().run()
  }
}

object MeasureLibraryCreate extends App {
  new Benchmark().run()
}
