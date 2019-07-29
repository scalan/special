package scalan

import scalan.common.{Segments, Interval, SegmentsModule}

class BasicStaginTests extends BaseCtxTests {
  lazy val ctx = new TestContext() with SegmentsModule {
    beginPass(new DefaultPass("mypass", Pass.defaultPassConfig.copy(constantPropagation = false)))
    var defCounter = 0
    var defTime: Long = 0
    override def def_unapply[T](e: Rep[T]) = {
      defCounter += 1
      val start = System.nanoTime()
      val res = super.def_unapply(e)
      val end = System.nanoTime()
      defTime += (end - start)
      res
    }
    def printCounters() =  {
      println(s"Defs: $defCounter, Time: ${defTime}")
    }
  }
  import ctx._

  test("create Scalan cake") {
    val x = toRep(10)
    assert(x != null)
  }

  test("stage and pattern match constants") {
    val x = toRep(10)
    x match {
      case Def(Const(x)) => x shouldBe 10
    }
  }

  test("stage and pattern match ops") {
    val x = toRep(10)
    val y = toRep(20)
    val z = x + y
    z match {
      case Def(ApplyBinOp(op, Def(Const(10)), Def(Const(20)))) =>
        op.isInstanceOf[NumericPlus[_]] shouldBe true
    }
    printCounters()
  }

  test("stage and pattern match lambdas") {
    val f = fun { x: Rep[Int] => x + 1 }
    f match {
      case Def(Lambda(lam, _, x, Def(ApplyBinOp(op, x1, Def(Const(1)))))) =>
        x shouldBe x1
        lam.schedule.isEmpty shouldBe false
    }
    printCounters()
  }

  test("transform() for LiftedConst") {
    import Liftables._
    import Segment._
    val seg1: scalan.common.Segment = new scalan.common.Interval(0, 10)
    val segSym1 = liftConst(seg1)
    val seg2: scalan.common.Segment = new scalan.common.Interval(0, 20)
    val segSym2 = liftConst(seg2)
    segSym1 shouldNot be(segSym2) // different constants

    // transform should preserve liftable constants
    segSym1.rhs.mirror(MapTransformer.Empty) shouldBe segSym1
    segSym1.rhs.mirror(new MapTransformer((segSym1 -> segSym2))) shouldBe segSym1
  }
}
