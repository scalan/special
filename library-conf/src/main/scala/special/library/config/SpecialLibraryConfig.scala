package special.library.config

import scalan.{FunctorType, Liftable, ContainerType, WithMethodCallRecognizers}
import scalan.meta.ScalanAst.WrapperConf
import scalan.meta.{LibraryConfig, TargetModuleConf, ConfMap, SourceModuleConf}

class SpecialLibraryConfig extends LibraryConfig {
  def name = "library"
  def baseDir = ""

  def wrapperConfigs: Map[String, WrapperConf] = List(
    WrapperConf(baseDir,
      packageName = "scala",
      name = "Option",
      annotations = List(
        classOf[ContainerType], classOf[FunctorType], classOf[Liftable], classOf[WithMethodCallRecognizers])
        .map(_.getSimpleName)
    ),
    WrapperConf(baseDir,
      packageName = "special",
      name = "SpecialPredef",
      annotations = List(classOf[WithMethodCallRecognizers]).map(_.getSimpleName)
    ),
    WrapperConf(baseDir,
      packageName = "scalan",
      name = "RType",
      annotations = List(classOf[Liftable], classOf[WithMethodCallRecognizers]).map(_.getSimpleName),
      imports = List("scalan.RType")
    ),
  ).map(w => (w.name, w)).toMap

  val ApiModule: SourceModuleConf = new SourceModuleConf(baseDir, "library-api")
      .addUnit("special/wrappers/WrappersSpec.scala", wrapperConfigs)
      .addUnit("special/collection/Monoids.scala")
      .addUnit("special/collection/Colls.scala")
      .addUnit("special/collection/Costs.scala")
      .addUnit("special/collection/Sizes.scala")

  val ImplModule = new SourceModuleConf(baseDir, "library-impl")
      .addUnit("special/collection/ConcreteCosts.scala")
      .addUnit("special/collection/ConcreteSizes.scala")
      .addUnit("special/collection/CostedOptions.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf(baseDir, "library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule)
  )

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
