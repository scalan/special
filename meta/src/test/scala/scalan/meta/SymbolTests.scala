package scalan.meta

import scala.tools.nsc.Global
import scalan.BaseNestedTests
import scalan.meta.Symbols.DefType

class SymbolTests extends BaseNestedTests with ScalanParsersEx[Global] {
  def getGlobal = new Global(settings, reporter)
  implicit val context = new AstContext(Nil, this)
  import context._

  describe("Symbol methods") {

    it("construct fullName"){
      val us = newUnitSymbol("scalan.collections", "Cols")
      us.fullName shouldBe "scalan.collections.Cols"
      us.toString shouldBe "sym://scalan.collections.Cols"
      val es = newEntitySymbol(us, "Col")
      es.fullName shouldBe "scalan.collections.Cols.Col"
      val ms = newEntityItemSymbol(es, "map", DefType.Def)
      ms.fullName shouldBe "scalan.collections.Cols.Col.map"
    }

  }

}
