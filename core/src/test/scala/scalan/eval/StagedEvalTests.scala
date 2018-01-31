package scalan.eval

import scalan.{TestContexts, Scalan}
import scalan.meta.{BaseMetaTests, SName}

class StagedEvalTests extends BaseMetaTests with TestContexts {
  val ctx = new TestContext("StagedEval") with StagedEvaluation {}
  val evtr = new StatedEvaluator[ctx.type](ctx)

  def testMethod(methodDef: String): Unit = {
    implicit val parseCtx = new ParseCtx(isVirtualized = false)
    val us = parseCtx.astContext.newUnitSymbol("scalan.meta", "StagedEvalTests")
    val m = parseMethod(us, methodDef)
    val f = evtr.eval[Any => Any](m)
    ctx.emit("f", f)
  }

  describe("Staged evaluation of Scala") {
    it("methods") {
      testMethod("def f(x: Int): Int = x + 1")
    }
  }
}
