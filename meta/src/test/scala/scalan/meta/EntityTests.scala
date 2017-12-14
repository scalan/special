package scalan.meta

class EntityTests extends BaseMetaTests with Examples {

  describe("SEntityDef ops") {
    val cols = parseModule(colsVirtModule)
    context.addModule(cols)
    it("getInheritedTypes") {
      val e = cols.getEntity("ColOverArray")
      val types = e.getInheritedTypes
      types.map(_.name) shouldBe(List("Collection", "Def"))
      e.isInherit("Def") shouldBe(true)
      e.isInherit("Def1") shouldBe(false)
      e.isInherit("Collection") shouldBe(true)
      e.isInherit("Iters") shouldBe(false)
    }
  }

}
