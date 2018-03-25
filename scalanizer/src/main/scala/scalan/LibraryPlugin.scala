package scalan

import scala.tools.nsc.Global
import scalan.meta.scalanizer.ScalanizerConfig
import scalan.plugin.ScalanizerPlugin

class LibraryPlugin(g: Global) extends ScalanizerPlugin(g) { plugin =>
  override def createScalanizerConfig(): ScalanizerConfig = new SpecialScalanizerConfig
}
