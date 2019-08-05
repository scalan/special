package scalan.primitives

import scalan.{DelayInvokeException, ScalanEx, BaseEx}
import scalan.compilation.{GraphVizConfig, GraphVizExport}

trait PatternMatching extends BaseEx with GraphVizExport { _: ScalanEx =>

//  def MATCH[A, B: Elem](selector: Rep[A])(f: PartialFunction[Rep[A], Rep[B]]): Rep[B] =
//  macro PatternMatchingMacro.patternMatchImpl[A, B]

  case class Branch[A, +B](elem: Elem[A], guard: Rep[A => Boolean], body: Rep[A => B]) {
    def transform(t: Transformer) = Branch[A,B](elem, t(guard), t(body))
  }

  object Branch {
    def apply[A, B](elem: Elem[A], body: Rep[A => B]): Branch[A, B] = Branch(elem, constFun(true)(elem), body)
  }

  // for convenient branch creation
  case class MkBranch[A]()(implicit elem: Elem[A]) {
    // unfortunately can't be used as MkBranch[A] { block } if named apply, so we write
    // MkBranch[A].make { block } instead
    def make[B: Elem](body: Rep[A] => Rep[B]) = Branch(elem, constFun(true)(elem), fun(body))
    def make[B: Elem](guard: Rep[A] => Rep[Boolean])(body: Rep[A] => Rep[B]) = Branch(elem, fun(guard), fun(body))
  }

  // TODO match branches need to be treated similarly to if branches for code motion etc.

  def patternMatchError(obj: Any): Nothing = throw new DelayInvokeException

  protected def patternMatch[A, B: Elem](selector: Rep[A])(branches: Branch[_ <: A, B]*)(default: Option[Rep[A => B]]) =
    reifyObject(Match[A, B](selector, branches.toList, default))

  case class Match[A, B](selector: Rep[A], branches: List[Branch[_ <: A, B]], default: Option[Rep[A => B]])(implicit selfType: Elem[B]) extends BaseDef[B] {
    override def transform(t: Transformer) =
      Match[A,B](t(selector), branches.map(_.transform(t)), default.map(t(_)))
  }

  private def eDom(branchBody: Rep[_ => _]) = branchBody.elem.asInstanceOf[FuncElem[_, _]].eDom

  override def rewriteDef[A](d: Def[A]) = d match {
    case Match(selector: Rep[a], branches, defaultOpt) =>
      branches match {
        case Nil => defaultOpt match {
          case Some(default) => default(selector)
          case None => super.rewriteDef(d) // TODO replace with Throw when we have better exceptions/effects support
        }
        // cheap check compared to TypeTag.<:<, we don't care about type arguments
        case Branch(elem, guard, body) :: branchesTail if selector.elem <:< elem => //elem.runtimeClass.isAssignableFrom(selectorClass) =>
          ifThenElseLazy(guard(selector), {
            body(selector).asInstanceOf[Rep[A]]
          }, {
            implicit val eA = d.resultType
            patternMatch(selector)(branchesTail.asInstanceOf[List[Branch[a, A]]]: _*)(defaultOpt.asInstanceOf[Option[Rep[a => A]]])
          })
        case _ if selector.elem.isInstanceOf[ConcreteElem[_, _]] =>
          val possibleBranches = branches.filter { case Branch(elem, _, _) =>
//            val branchClass = elem.runtimeClass
//            // could be wrong if we match on mix-ins instead of the main hierarchy; currently they aren't supported
//            branchClass.isAssignableFrom(selectorClass) || selectorClass.isAssignableFrom(branchClass)
            selector.elem <:< elem
          }
          if (possibleBranches.length < branches.length) {
            implicit val eA = d.resultType
            patternMatch(selector)(possibleBranches.asInstanceOf[List[Branch[a, A]]]: _*)(defaultOpt.asInstanceOf[Option[Rep[a => A]]])
          } else {
            super.rewriteDef(d)
          }
        case _ => super.rewriteDef(d)
      }
    case _ => super.rewriteDef(d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig) = d match {
    case Match(selector, branches, default) =>
      val branchString = branches.map { case Branch(_, guard, body) =>
        val e = eDom(body)
        val guardString = guard match {
          case Def(VeryConstantLambda(true)) => ""
          case _ => s"if $guard"
        }
        // Doesn't quite correspond to Scala code, but should be clear enough
        s"case ${e.name}$guardString => $body"
      }.mkString("; ")
      val defaultString = default match {
        case Some(f) => s"; _ => $f"
        case None => ""
      }
      s"$selector match { $branchString$defaultString }"
    case _ => super.formatDef(d)
  }
}
