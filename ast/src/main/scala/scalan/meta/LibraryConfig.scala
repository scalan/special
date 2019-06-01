package scalan.meta

/** Base class for configuration of Special library.
  * Each Special library contain:
  * - Source modules(i.e. api, impl)
  * - Target modules
  * - one conf module
  * - and one scalanizer module */
abstract class LibraryConfig extends Conf {
  /** Modules that contain units to be processed by Special plugin. */
  def sourceModules: List[SourceModuleConf]

  /** Modules that assemble Special units from source modules into Special cakes */
  def targetModules: List[TargetModuleConf]
}

