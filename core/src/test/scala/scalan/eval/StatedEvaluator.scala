package scalan.eval

import scalan.Scalan
import scalan.meta.AstContext
import scalan.meta.ScalanAst.{SExpr, SConst, SMethodDef}

trait StagedEvaluation extends Scalan {
  implicit val context: AstContext
  sealed trait Node
  case class AddrNode(sym: Sym) extends Node
  case class ExprNode(expr: SExpr) extends Node

  sealed trait EvalContext

  type Stack = List[EvalContext]
}


class StatedEvaluator[IR <: StagedEvaluation](val IR: IR) {
  import IR._
  def eval[A](expr: SExpr)(implicit ctx: AstContext): Rep[A] = //evalStep(ExprNode(expr), Nil).asRep[A]
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
      case _ => !!!(s"Unexpected stack", α)
    }
    case ExprNode(e) => (e, R) match {
      case (e, R) if isGroundNode(e) => addGroundNode(e)
    }
    case _ => !!!(s"Don't know how to evalStep($expr, $R)")
  }
}
