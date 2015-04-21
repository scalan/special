package scalan.primitives

import scalan._
import scalan.common.Default

trait AbstractStrings extends Base with BaseTypes { self: AbstractStringsDsl =>

  type RStr = Rep[String]
  trait AString extends TypeWrapper[String, AString] { self =>
    def wrappedValueOfBaseType: Rep[String]
  }
  trait AStringCompanion extends ExCompanion0[String]  {
    def defaultVal = Default.defaultVal("")
    def apply(msg: Rep[String]): Rep[String] = newObjEx(classOf[String], List(msg.asRep[AnyRef]))
  }

  abstract class SString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait SStringCompanion

  abstract class CString(val wrappedValueOfBaseType: Rep[String]) extends AString
  trait CStringCompanion
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {
}

trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {
}

trait AbstractStringsDslExp extends impl.AbstractStringsExp {
}