package scalan.compilation

import scalan.ScalanEx

abstract class ScalanCompiler[+ScalanCake <: ScalanEx, +Codegen <: FileCodegen[ScalanCake]](_scalan: ScalanCake)
    extends Compiler[ScalanCake](_scalan) {
  def codegen: Codegen
}
