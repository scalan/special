package scalan.meta

class EntityTests extends BaseMetaTests with Examples {

  describe("SEntityDef ops") {
    val cols = parseModule(colsVirtModule)
    context.addModule(cols)
    val e = cols.getEntity("ColOverArray")
    it("getInheritedTypes") {
      val types = e.getInheritedTypes
      types.map(_.name) shouldBe(List("Collection", "Def"))
    }
    it("inherits") {
      val e = cols.getEntity("ColOverArray")
      e.inherits("Def") shouldBe(true)
      e.inherits("Def1") shouldBe(false)
      e.inherits("Collection") shouldBe(true)
      e.inherits("Iters") shouldBe(false)
    }
    it("collectMethodsInBody") {
      val e = cols.getEntity("PairOfCols")
      e.collectItemsInBody(_.isAbstract).map(_._2.name) shouldBe(List())
      e.collectItemsInBody(!_.isAbstract).map(_._2.name) shouldBe(List("ls", "rs", "eL", "eR", "length", "apply"))
    }
    it("collectMethodsFromAncestors") {
      val e = cols.getEntity("PairOfCols")
      e.collectMethodsFromAncestors(_.isAbstract).map(_._2.name) shouldBe(List("eL", "eR", "ls", "rs", "eA", "arr", "length", "apply"))
      e.collectMethodsFromAncestors(!_.isAbstract).map(_._2.name) shouldBe(List())
    }
  }

}
