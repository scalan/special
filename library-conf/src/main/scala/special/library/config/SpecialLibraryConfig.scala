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
        packageName = "scalan",
        name = "SpecialPredef"
      )
    ).map(w => (w.name, w)).toMap

  val ApiModule: SourceModuleConf = new SourceModuleConf(baseDir, "library-api")
      .addUnit("WrappersSpec.scala", "library/WrappersSpec.scala", wrapperConfigs)
      .addUnit("Monoids.scala", "scalan/collection/Monoids.scala")
      .addUnit("Cols.scala", "scalan/collection/Cols.scala")
      .addUnit("Costs.scala", "scalan/collection/Costs.scala")

  val ImplModule = new SourceModuleConf(baseDir, "library-impl")
      .addUnit("MonoidInstances.scala", "scalan/collection/MonoidInstances.scala")
      .addUnit("ColsOverArrays.scala", "scalan/collection/ColsOverArrays.scala")
      .addUnit("ConcreteCosts.scala", "scalan/collection/ConcreteCosts.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf(baseDir, "library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule)
  )

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
