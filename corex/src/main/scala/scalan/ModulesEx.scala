package scalan

import scalan.meta.ScalanAst.SUnitDef
import scalan.meta.{UnitConfig, AstContextBase}

import scala.collection.mutable

trait ModulesEx extends Modules { self: ScalanEx =>
  def configs: List[UnitConfig] = Nil

  def astContext: AstContextBase = !!!(s"AstContext is not overridden in IR cake $this")

  def getModules: mutable.Map[String, SUnitDef] = mutable.Map.empty[String, SUnitDef]

  def allEntities = getModules.values.flatMap(_.allEntities)

}
