package scalan.eval

import scalan.{Scalan, TestContexts}
import scalan.meta.BaseMetaTests

class StagedEvalTests extends BaseMetaTests with TestContexts {
  val ctx = new TestContext("StagedEval") with StagedEvaluation {}
  val evtr = new StatedEvaluator[ctx.type](ctx)

  def testMethod(methodDef: String): Unit = {
    implicit val parseCtx = new ParseCtx(isVirtualized = false)
    val m = parseMethod(methodDef)
    val f = evtr.eval[Any => Any](m)
    ctx.emit("f", f)
  }

  describe("Staged evaluation of Scala") {
    it("methods") {
      testMethod("def f(x: Int): Int = x + 1")
    }
  }
}
