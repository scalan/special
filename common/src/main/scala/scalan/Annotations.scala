package scalan

import scala.annotation.Annotation

class lang(name: String) extends Annotation

object Effect extends Enumeration {
  val NoEffects = Value(0)
  val Control = Value(1)
}

class effects(types: Effect.Value*) extends Annotation
