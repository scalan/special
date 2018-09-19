package special.library.config

import scalan.{ContainerType, FunctorType}
import scalan.meta.ScalanAst.WrapperConf
import scalan.meta.{LibraryConfig, TargetModuleConf, ConfMap, SourceModuleConf}

class SpecialLibraryConfig extends LibraryConfig {
  def name = "library"
  def baseDir = ""

  def wrapperConfigs: Map[String, WrapperConf] = List(
    WrapperConf(baseDir,
      packageName = "scala",
      name = "Array",
      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
    ),
    WrapperConf(baseDir,
      packageName = "scala",
      name = "Option",
      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
    ),
    WrapperConf(baseDir,
      packageName = "scala.util",
      name = "Either"
    ),
    WrapperConf(baseDir,
      packageName = "special",
      name = "SpecialPredef"
    ),
    WrapperConf(baseDir,
      packageName = "scalan.meta",
      name = "RType"
    ),
  ).map(w => (w.name, w)).toMap

  val ApiModule: SourceModuleConf = new SourceModuleConf(baseDir, "library-api")
      .addUnit("special/wrappers/WrappersSpec.scala", wrapperConfigs)
      .addUnit("special/collection/Monoids.scala")
      .addUnit("special/collection/Cols.scala")
      .addUnit("special/collection/Costs.scala")

  val ImplModule = new SourceModuleConf(baseDir, "library-impl")
      .addUnit("special/collection/MonoidInstances.scala")
      .addUnit("special/collection/ColsOverArrays.scala")
      .addUnit("special/collection/ConcreteCosts.scala")
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
