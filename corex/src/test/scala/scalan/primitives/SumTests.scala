package scalan.primitives

import scala.language.reflectiveCalls
import scalan.{BaseCtxTests, BaseCtxTestsEx}
import scalan.common.{SegmentsModule, CommonExamples, ViewExamples}

class SumTests extends BaseCtxTestsEx {

  test("IsSumMapLambda") {
    val ctx = new TestContextEx with SegmentsModule {
      lazy val t1 = fun { x: Ref[Int|Unit] => x.mapSum(l => l + 1, r => r) }
    }
    import ctx._
    t1 should matchPattern { case Def(IsSumMapLambda(_)) => }
  }

  test("constant propagation from If to SumFold") {
    val ctx = new TestContextEx("fromIfToSumFold") with SegmentsModule {
      lazy val t1 = fun { x: Ref[Int] =>
        val s = IF (x > 0) THEN { (x + 1).asLeft[Int] } ELSE { (x + 2).asRight[Int] }
        s.fold(l => l + 1, r => r - 2)
      }
      lazy val t2 = fun { x: Ref[Int] =>
        val s = IF (x > 0) THEN { (x + 1).asRight[Int] } ELSE { (x + 2).asLeft[Int] }
        s.fold(l => l + 1, r => r - 2)
      }
    }
    import ctx._
    emit("t1", t1)
    emit("t2", t2)
  }

  test("SumMap(Right(x)) rewriting") {
    val ctx = new TestContextEx("SumMapRightRewriting") {
      lazy val t1 = fun { x: Ref[Int] =>
        x.asRight[Int].mapSum(_ + 1, _ - 1)
      }
    }
    import ctx._

    emit("t1", t1)
    t1.getLambda.y should matchPattern { case Def(SRight(_,_)) => }
  }

  test("SumMap(Left(x)) rewriting") {
    val ctx = new TestContextEx("SumMapLeftRewriting") {
      lazy val t1 = fun { x: Ref[Int] =>
        x.asLeft[Int].mapSum(_ + 1, _ - 1)
      }
    }
    import ctx._

    emit("t1", t1)
    t1.getLambda.y should matchPattern { case Def(SLeft(_,_)) => }
  }

  test("isLeft and isRight work") {
    val ctx = new TestContextEx() {
      lazy val isLeftFun = fun { x: Ref[Int | Double] => x.isLeft }
      lazy val isRightFun = fun { x: Ref[Int | Double] => x.isRight }

      lazy val shouldBeTrue = toRep(10).asLeft[Double].isLeft
    }
    import ctx._

    isLeftFun.getLambda.y should matchPattern { case Def(IsLeft(_)) => }
    isRightFun.getLambda.y should matchPattern { case Def(IsRight(_)) => }
    shouldBeTrue shouldEqual toRep(true)
  }
}
