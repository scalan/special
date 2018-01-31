package scalan.meta

import Symbols._
import scalan.meta.ScalanAst.AstContext

trait Symbols { this: AstContext =>

  def newUnitSymbol(packageName: String, name: String): SUnitSymbol = {
    SUnitSymbol(SName(packageName, name), null)
  }

  def newEntitySymbol(outer: SSymbol, entityName: String): SEntitySymbol = {
    SEntitySymbol(entityName, outer)
  }

}

object Symbols {

  abstract class SSymbol(val outer: SSymbol) {
    def outerUnit: SUnitSymbol = this match {
      case us: SUnitSymbol =>
        assert(us.outer eq null, s"UnitSymbol have outer ${us.outer}")
        us
      case _ =>
        assert(outer ne null, s"$this doesn't have outer")
        outer.outerUnit
    }
  }
  case class SUnitSymbol private(unitName: SName, override val outer: SSymbol) extends SSymbol(outer)
  case class SEntitySymbol private(entityName: String, override val outer: SSymbol) extends SSymbol(outer)

}
