package scalan.eval

import com.trueaccord.lenses.Updatable
import org.scalatest.fixture

import scalan.{TestContexts, Scalan}
import scalan.meta.{SName, BaseMetaTests, AstContext}
import scalan.meta.AstLenses._

class StagedEvalTests extends BaseMetaTests with TestContexts {
  class Ctx(implicit val context: AstContext) extends TestContext("StagedEval") with StagedEvaluation {}
  val ctx = new Ctx
  val evtr = new StatedEvaluator[ctx.type](ctx)
  val testUnit = TestModule("Test",
    """ package scalan.collection
     |  trait A {
     |  }
     """.stripMargin, false)
  val un = ctx.context.addUnit(parseModule(testUnit)).unitName
  
  def testMethod(name: String, methodDef: String): Unit = {
    implicit val parseCtx = new ParseCtx(isVirtualized = false)
    val unit = parseCtx.astContext.updateUnit(un, l => {
      l.traits.find(_.name ==  "A").modify(t => {
        val m = parseMethod(t.symbol, methodDef)
        t.update(_.body :+= m)
      })
    })
    val eA = unit.getEntity("A")

//    val sA = evtr.eval[Any => Any](eA)
//    ctx.emit(name, sA)
  }

  describe("Staged evaluation of Scala") {
    it("methods") {
      testMethod("f", "def f(x: Int): Int = x + 1")
    }
  }
}
