package special.library.config

import scalan.meta.{TargetModuleConf, ConfMap, SourceModuleConf, LibraryConfig}

class SpecialLibraryConfig extends LibraryConfig {
  def name = "library"
  def baseDir = ""
  val ApiModule = new SourceModuleConf("", "library-api")
//      .addUnit("Cols.scala", "scalan/collection/Cols.scala")
      .addUnit("Costs.scala", "scalan/collection/Costs.scala")
  val ImplModule = new SourceModuleConf("", "library-impl")
//      .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")
      .addUnit("ConcreteCosts.scala", "scalan/collection/ConcreteCosts.scala")
      .dependsOn(ApiModule)
  val target = new TargetModuleConf("", "library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule)
  )

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(target)
}
