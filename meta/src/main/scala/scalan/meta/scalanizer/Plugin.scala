package scalan.meta.scalanizer

import scalan.meta.AstContext

/** Entry point of a plugin for Scalanizer (as opposed to ScalanizerPlugin) */
trait Plugin {
  def apply(context: AstContext, scalanizerConfig: ScalanizerConfig, extraData: Any)
}
