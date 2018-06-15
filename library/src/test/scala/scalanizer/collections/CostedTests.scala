package scalanizer.collections

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan._

class CostedTests extends BaseCtxTests {
  class Ctx extends TestContext with Library {
    def plus(x: Rep[Int], n: Int) = {
      Range(0, n).foldLeft(x)((y, i) => y + i)
    }

    val b = ColOverArrayBuilder()

    def dataCost[T:Elem](x: Rep[T]): Rep[Costed[T]] = element[T] match {
      case ByteElement => CostedPrim(x, 1L)
      case ShortElement => CostedPrim(x, 2L)
      case IntElement => CostedPrim(x, 4L)
      case LongElement => CostedPrim(x, 8L)
      case pe: PairElem[a,b] =>
        val l = dataCost(x.asRep[(a,b)]._1)(pe.eFst)
        val r = dataCost(x.asRep[(a,b)]._2)(pe.eSnd)
        CostedPair(l.value, r.value, l.cost + r.cost)
      case ae: WArrayElem[a,_] =>
        val col = b.fromArray(x.asRep[WArray[a]])
        implicit val ea = ae.eItem
        val costed = col.map(fun(dataCost[a]))
        CostedArray(costed).asRep[Costed[T]]
    }

    def result[T](dc: Rep[Costed[T]]): Rep[(T, Long)] = Pair(dc.value, dc.cost)

    def split[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Long)] = {
      implicit val eT = f.elem.eDom
      val calc = fun { x: Rep[T] => f(x).value }
      val cost = fun { x: Rep[T] => f(x).cost }
      Pair(calc, cost)
    }
  }

  def measure[T](nIters: Int)(action: Int => Unit): Unit = {
    for (i <- 1 to nIters) {
      val start = System.currentTimeMillis()
      val res = action(i)
      val end = System.currentTimeMillis()
      val iterTime = end - start
      println(s"Iter $i: $iterTime ms")
    }
  }
  lazy val ctx = new Ctx { }
  import ctx._

  def buildGraph[T](nIters: Int, name: String)(action: Int => Rep[T]) = {
    val buf = mutable.ArrayBuilder.make[Rep[T]]()
    measure(nIters) { i =>
      buf += action(i)
    }
    ctx.emit(name, buf.result(): _*)
  }


  test("measure: plus const propagation") {
    buildGraph(10, "measure_plus_const") { i =>
      plus(i * 1000, 1000)
    }
  }

  test("plus fresh var") {
    buildGraph(10, "measure_plus_var") { i =>
      plus(fresh[Int], 1000)
    }
  }

  test("measure: dataCost") {
    buildGraph(10, "measure_dataCost") { i =>
      val data = Range(0, 20).foldLeft[Rep[Any]](toRep(i))((y, k) => Pair(y, k))
      result(dataCost(data)(data.elem))
    }
  }

  test("data cost") {
    ctx.emit("dataCost",
      result(dataCost(Pair(10, 20.toByte))),
      result(dataCost(Pair(30, Pair(40.toByte, 50L))))
    )
  }

  test("split") {
    ctx.emit("split",
      split(fun { x: Rep[(Int, Byte)] => dataCost(x) }),
      split(fun { x: Rep[Int] => dataCost(Pair(x, 20.toByte)) })
    )
  }

  test("split arrays") {
    ctx.emit("split_arrays",
      split(fun { x: Rep[WArray[Int]] => dataCost(Pair(x, 20.toByte)) })
    )
  }

}
