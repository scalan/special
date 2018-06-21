package special.library.config

import scalan.meta.{TargetModuleConf, ConfMap, SourceModuleConf, LibraryConfig}

class SpecialLibraryConfig extends LibraryConfig {
  def name = "library"
  def baseDir = ""
  
  val ApiModule: SourceModuleConf = new SourceModuleConf("", "library-api")
      .addUnit("WrappersSpec.scala", "library/WrappersSpec.scala")
      .addUnit("Monoids.scala", "scalan/collection/Monoids.scala")
      .addUnit("Cols.scala", "scalan/collection/Cols.scala")
      .addUnit("Costs.scala", "scalan/collection/Costs.scala")

//  val wrapperConfigs: Map[String, WrapperConfig] = List(
//    WrapperConfig(
//      name = "Array",
//      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
//    ),
//    WrapperConfig(
//      name = "SpecialPredef"
//    )
//  ).map(w => (w.name, w)).toMap

  val ImplModule = new SourceModuleConf("", "library-impl")
      .addUnit("MonoidInstances.scala", "scalan/collection/MonoidInstances.scala")
      .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")
      .addUnit("ConcreteCosts.scala", "scalan/collection/ConcreteCosts.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf("", "library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule)
  )

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
