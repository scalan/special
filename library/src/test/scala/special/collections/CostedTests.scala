package special.collections

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.util.BenchmarkUtil._
import special.SpecialPredef

class CostedTests extends BaseCostedTests {

  class ThisCtx extends Ctx  {
  }
  lazy val ctx = new ThisCtx
  import ctx._
  import CSizePrim._
  import CSizePair._
  import Costed._
  import CCostedPair._
  import CCostedPrim._
  import CCostedColl._
  import CCostedOption._
  import CollBuilder._
  import Coll._
  import WOption._
  import WSpecialPredef._
  import Liftables._

  lazy val sizeDataRW = new PartialRewriter({ case Def(sd: SizeData[_,_]) => calcSizeFromData(sd) })

  def buildGraph[T](nIters: Int, name: String)(action: Int => Rep[T]) = {
    val buf = mutable.ArrayBuilder.make[Rep[T]]()
    measure(nIters) { i =>
      buf += action(i)
    }
    ctx.emit(name, buf.result(): _*)
  }

  lazy val l = toRep(10)
  lazy val r = toRep(10.toByte)
  lazy val lC = RCCostedPrim(l, 1, RCSizePrim[Int](4L))
  lazy val rC = RCCostedPrim(r, 1, RCSizePrim[Byte](1L))
  lazy val pC = RCCostedPair(lC, rC)
  lazy val ppC = RCCostedPair(pC, pC)

  test("dataSize of CostedPair") {
    val sizeD= pC.size
    val expected = RCSizePair(RCSizePrim[Int](4L), RCSizePrim[Int](1L))
    sizeD shouldBe expected
  }

//  test("dataSize of nested CostedPair") {
//    val sizeD= ppC.dataSize
//    val ppSize = sizeData(pC.value.elem, Pair(sizeData(l.elem, 4L), sizeData(r.elem, 1L)))
//    val expected @ Def(d: SizeData[_,_]) = sizeData(ppC.value.elem, Pair(ppSize, ppSize))
//    sizeD shouldBe expected
//    val size = ProgramGraph.transform(sizeD, sizeDataRW)
//    size shouldBe toRep(10L)
//  }
//
//  val Colls = new special.collection.CollOverArrayBuilder
//  val xs = Colls.fromItems(10, 20, 30)
//  lazy val xsSym = liftConst(xs)
//  lazy val xsSizes = liftConst(Colls.replicate(3, 4L))
//  lazy val xsC = RCCostedColl(xsSym, liftConst(Colls.replicate(3, 0)), xsSizes, 0)
//
//  test("dataSize of CostedColl") {
//    val sizeD = xsC.dataSize
//    val expected @ Def(d: SizeData[_,_]) = sizeData(xsC.value.elem, xsSizes)
//    sizeD shouldBe expected
//    val size = ProgramGraph.transform(sizeD, sizeDataRW)
//    size shouldBe toRep(12L)
//  }
//
//  val opt: Option[Int] = Some(10)
//  lazy val optSym = liftConst(opt)
//  lazy val optSize = RWSpecialPredef.some(4L)
//  lazy val optC = RCCostedOption(optSym, RWSpecialPredef.some(0), optSize, 0)
//
//  test("dataSize of CostedOption") {
//    val sizeD = optC.dataSize
//    val expected @ Def(d: SizeData[_,_]) = sizeData(optC.value.elem, optSize)
//    sizeD shouldBe expected
//    val size = ProgramGraph.transform(sizeD, sizeDataRW)
//    size shouldBe toRep(4L)
//  }
//
//  test("dataSize of CostedOption.get") {
//    val v = optC.get.dataSize
//    v shouldBe sizeData(element[Int], 4L)
//  }

//  test("measure: plus const propagation") {
//    buildGraph(10, "measure_plus_const") { i =>
//      plus(i * 1000, 1000)
//    }
//  }
//
//  test("measure: dataCost") {
//    buildGraph(10, "measure_dataCost") { i =>
//      val data = Range(0, 20).foldLeft[Rep[Any]](toRep(i))((y, k) => Pair(y, k))
//      result(dataCost(data))
//    }
//  }
//
//  test("data cost") {
//    ctx.emit("dataCost",
//      result(dataCost(Pair(10, 20.toByte))),
//      result(dataCost(Pair(30, Pair(40.toByte, 50L))))
//    )
//  }
//
//  test("split") {
//    ctx.emit("split",
//      split(fun { x: Rep[(Int, Byte)] => dataCost(x) }),
//      split(fun { x: Rep[Int] => dataCost(Pair(x, 20.toByte)) })
//    )
//  }
//
//  test("split arrays") {
//    ctx.emit("split_arrays",
//      split(fun { in: Rep[(WArray[Int], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("measure: split arrays") {
//    buildGraph(10, "measure_split_arrays") { i =>
//      var res: Rep[Any] = null
//      for (k <- 1 to 1000) {
//        res = split(fun { in: Rep[(WArray[Int], Byte)] =>
//          val Pair(x, b) = in
//          dataCost(Pair(x, b + i.toByte))
//        })
//      }
//      res
//    }
//  }
//
//  test("split pair arrays") {
//    ctx.emit("split_pair_arrays",
//      split(fun { in: Rep[(WArray[(Int, Short)], Byte)] =>
//        dataCost(in)
//      })
//    )
//    ctx.emit("split_pair_arrays2",
//      split(fun { in: Rep[(WArray[(Int, (Short, Boolean))], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("measure: split pair arrays") {
//    buildGraph(10, "measure_split_pair_arrays") { i =>
//      var res: Rep[Any] = null
//      for (k <- 1 to 10) {
//        res = split(fun { in: Rep[(WArray[(Int, (Short, Boolean))], Byte)] =>
//          val Pair(x, b) = in
//          dataCost(Pair(x, b + i.toByte))
//        })
//      }
//      res
//    }
//  }
//
//  test("split nested arrays") {
//    ctx.emit("split_nested_arrays",
//      split(fun { in: Rep[(WArray[WArray[Int]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested pair arrays") {
//    ctx.emit("split_nested_pair_arrays",
//      split(fun { in: Rep[(WArray[WArray[(Int, Short)]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested nested arrays") {
//    ctx.emit("split_nested_nested_arrays",
//      split(fun { in: Rep[(WArray[WArray[WArray[Int]]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split nested nested pair arrays") {
//    ctx.emit("split_nested_nested_pair_arrays",
//      split(fun { in: Rep[(WArray[WArray[WArray[(Int, Short)]]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split complex1") {
//    ctx.emit("split_complex1",
//      split(fun { in: Rep[(WArray[WArray[(WArray[(Int, Short)], Boolean)]], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("split complex2") {
//    ctx.emit("split_complex2",
//      split(fun { in: Rep[(WArray[(WArray[(WArray[(Int, Boolean)], Short)], Char)], Byte)] =>
//        dataCost(in)
//      })
//    )
//  }
//
//  test("measure: split complex2") {
//    buildGraph(5, "measure_split_complex2") { i =>
//      var res: Rep[Any] = null
//      for (k <- 1 to 10) {
//        res =  split(fun { in: Rep[(WArray[(WArray[(WArray[(Int, Boolean)], Short)], Char)], Byte)] =>
//          val Pair(x, b) = in
//          dataCost(Pair(x, b + i.toByte))
//        })
//      }
//      res
//    }
//    measure(2) { i =>
//      var sum = 0
//      var nDefs = 0
//      var nVars = 0
//      for (s <- ctx.allSymbols) {
//        s match {
//          case Def(d) =>
//            nDefs +=1
//          case _ =>
//            nVars += 1
//        }
//      }
//      println(s"#Defs: $nDefs, #Vars: $nVars, sum: $sum")
//    }
//  }

}
