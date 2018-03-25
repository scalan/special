package scalan

import special.library.config.SpecialLibraryConfig

import scalan.meta.{ConfMap, TargetModuleConf, SourceModuleConf}
import scalan.plugin.ScalanizerPluginConfig

class SpecialScalanizerConfig extends ScalanizerPluginConfig {
  val library = new SpecialLibraryConfig

  /** Modules that contain units to be virtualized by scalan-meta. */
  override val sourceModules: ConfMap[SourceModuleConf] = ConfMap(library.sourceModules: _*)

  /** Modules that assemble virtualized units from source modules into virtualized cakes */
  override val targetModules: ConfMap[TargetModuleConf] = ConfMap(library.targetModules: _*)
}
