package scalan.meta

import scalan.meta.ScalanAst.WrapperConf

class ConfTests extends BaseMetaTests with Examples {
  val warrays = parseModule(warraysModule)
  val warrays1 = parseModule(warrays1Module)
  val warrays2 = parseModule(warrays2Module)
  val baseDir = ""
  def wrapperConfigs: Map[String, WrapperConf] = List(
    WrapperConf(baseDir,
      packageName = "scalan",
      name = "SpecialPredef"
    )
  ).map(w => (w.name, w)).toMap

  val warrayModule = new SourceModuleConf("", "warrays")
      .addUnit("WArrays.scala", "scala/WArrays.scala")
  val apiModule = new SourceModuleConf("", "library-api")
      .addUnit("WrappersSpec.scala", "library/WrappersSpec.scala", wrapperConfigs)
      .addUnit("Cols.scala", "scalan/collection/Cols.scala")
      .dependsOn(warrayModule)
  val implModule = new SourceModuleConf("", "library-impl")
      .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")
      .dependsOn(apiModule, warrayModule)

  describe("Module dependencies") {
    it("collectInputModules") {
      implModule.dependsOnModules() shouldBe (Set(warrayModule, apiModule))
    }
  }

  describe("SourceModuleConf") {
    it("wrapperSpecUnit") {
      apiModule.wrapperSpecUnit("SpecialPredef").map(_.name) shouldBe Some("WrappersSpec.scala")
      apiModule.wrapperSpecUnit("SpecialPredef").map(_.packageName) shouldBe Some("library")
    }
  }

}
