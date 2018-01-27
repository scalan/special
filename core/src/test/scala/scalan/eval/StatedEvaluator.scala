package scalan.eval

import scalan.Scalan
import scalan.meta.ScalanAst.{SExpr, AstContext, SMethodDef, SConst}

trait StagedEvaluation extends Scalan {
  sealed trait Node
  case class AddrNode(sym: Sym) extends Node
  case class ExprNode(expr: SExpr) extends Node

  sealed trait EvalContext

  type Stack = List[EvalContext]
}


class StatedEvaluator[IR <: StagedEvaluation](val IR: IR)(implicit context: AstContext) {
  import IR._
  def eval[A](expr: SExpr): Rep[A] = //evalStep(ExprNode(expr), Nil).asRep[A]
    expr match {
      case md: SMethodDef =>
        val f = fun { x: Rep[Int] => x + 1 }
        f.asRep[A]
    }

  def addGroundNode(e: SExpr): Sym = e match {
    case SConst(c, Some(tpe)) => toRep(c)(tpe.toTypeDesc().asElem[Any])
  }

  def isGroundNode(e: SExpr): Boolean = e match {
    case _: SConst | _: SMethodDef => true
    case _ => false
  }

  // letters: αβγδεζηθλμπρσφωψ
  def evalStep(expr: Node, R: Stack): Sym = expr match {
    case AddrNode(α) => R match {
      case Nil => α
    }
    case ExprNode(e) => (e, R) match {
      case (e, R) if isGroundNode(e) => addGroundNode(e)
    }
  }
}
