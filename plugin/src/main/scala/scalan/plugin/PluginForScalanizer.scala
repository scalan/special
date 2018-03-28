package scalan.plugin

import scalan.meta.AstContext
import scalan.meta.scalanizer.ScalanizerConfig

import scala.tools.nsc.Global

/** Entry point of a plugin for Scalanizer (as opposed to ScalanizerPlugin) */
trait PluginForScalanizer {
  def apply(context: AstContext, scalanizerConfig: ScalanizerConfig, pipeline: SourceModulePipeline[Global], extraData: Any)
}
