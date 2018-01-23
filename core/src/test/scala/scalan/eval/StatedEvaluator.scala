package scalan.eval

import scalan.Scalan
import scalan.meta.ScalanAst.{SExpr, SMethodDef, AstContext}

class StatedEvaluator[IR <: Scalan](val IR: IR)(implicit context: AstContext) {
  import IR._
  def eval[A](expr: SExpr): Rep[A] = expr match {
    case md: SMethodDef =>
      val f = fun { x: Rep[Int] => x + 1 }
      f.asRep[A]
  }
}
