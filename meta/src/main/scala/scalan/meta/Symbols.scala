package scalan.meta

import java.util.Objects

import Symbols._

trait Symbols { this: AstContext =>

  def newUnitSymbol(packageName: String, name: String): SUnitSymbol = {
    SUnitSymbol(SNoSymbol, SName(packageName, name))
  }

  def newEntitySymbol(owner: SSymbol, name: String, defType: DefType.Value): SNamedDefSymbol = {
    SEntitySymbol(owner, name)
  }

  def newEntityItemSymbol(owner: SSymbol, name: String, defType: DefType.Value): SNamedDefSymbol = {
    SEntityItemSymbol(owner, name, defType)
  }

}

object Symbols {

  sealed trait SSymbol {
    def owner: SSymbol
    def outerUnit: SUnitSymbol = this match {
      case us: SUnitSymbol =>
        assert(us.owner eq SNoSymbol, s"UnitSymbol have owner ${us.owner}")
        us
      case _ =>
        assert(owner ne SNoSymbol, s"$this doesn't have owner")
        owner.outerUnit
    }
  }

  object DefType extends Enumeration {
    val Unit, Entity, Method, Val, ClassArg, Type = Value
  }

  trait SNamedDefSymbol extends SSymbol {
    def name: String
    def defType: DefType.Value
  }

  case object SNoSymbol extends SSymbol {
    override def owner: SSymbol = this
  }

  class SEntitySymbol private[Symbols](val owner: SSymbol, val name: String) extends SNamedDefSymbol {
    def defType = DefType.Entity

    override def hashCode(): Int = Objects.hash(owner, name)
    override def equals(other: Any): Boolean = other match {
      case es: SEntitySymbol => owner == es.owner && name == es.name
      case _ => false
    }
    override def toString = s"SEntitySymbol($owner,$name)"
  }
  object SEntitySymbol {
    def apply(owner: SSymbol, name: String) = new SEntitySymbol(owner, name)
    def unapply(s: SSymbol): Option[(SSymbol, String)] = s match {
      case es: SEntitySymbol => Some((es.owner, es.name))
      case _ => None
    }
  }

  case class SEntityItemSymbol private(owner: SSymbol, name: String, defType: DefType.Value)
      extends SNamedDefSymbol

  case class SUnitSymbol private(override val owner: SSymbol, unitName: SName)
      extends SEntitySymbol(owner, unitName.name) {
    override def defType = DefType.Unit
    override def toString = s"SUnitSymbol($owner,${unitName.mkFullName})"
  }

}
