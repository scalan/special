package scalan.meta

import Symbols._
import scalan.meta.ScalanAst.AstContext

trait Symbols { this: AstContext =>

  def newUnitSymbol(packageName: String, name: String): SUnitSymbol = {
    SUnitSymbol(SName(packageName, name), null)
  }

  def newNamedDefSymbol(outer: SSymbol, name: String, defType: DefType.Value): SNamedDefSymbol = {
    SNamedDefSymbol(name, defType, outer)
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

  case class SNamedDefSymbol private(
      name: String, defType: DefType.Value, override val outer: SSymbol) extends SSymbol(outer)

  object DefType extends Enumeration {
    val Entity, Method, Val, ClassArg, Type = Value
  }
}
