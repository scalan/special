package scalan.meta

class EntityTests extends BaseMetaTests with Examples {

  describe("SEntityDef ops") {
    val cols = parseModule(colsVirtModule)
    context.addModule(cols)
    it("getInheritedTypes") {
      val e = cols.getEntity("ColOverArray")
      val types = e.getInheritedTypes
      types.map(_.name) shouldBe(List("Collection", "Def"))
      e.inherits("Def") shouldBe(true)
      e.inherits("Def1") shouldBe(false)
      e.inherits("Collection") shouldBe(true)
      e.inherits("Iters") shouldBe(false)
    }
  }

}
