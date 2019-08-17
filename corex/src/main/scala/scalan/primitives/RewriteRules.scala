package scalan.primitives

import scalan.{ScalanEx, Base, Nullable, AVHashMap}

import scala.reflect.runtime.universe._
import scalan.util.Invariant

import scala.reflect.ClassTag

trait RewriteRules extends Base { self: ScalanEx =>
  case class Rewrite[A](lhs: Ref[A], rhs: Ref[A])
  case class RewriteOp[A: Elem]() extends BinOp[A, Rewrite[A]]("==>", (x, y) => Rewrite[A](x, y))

  type RRewrite[A] = Ref[Rewrite[A]]

  case class RewriteElem[A](eA: Elem[A]) extends Elem[Rewrite[A]] {
    override def buildTypeArgs = TypeArgs("A" -> (eA -> Invariant))
  }

  def cachedElem[E <: Elem[_]](args: AnyRef*)(implicit tag: ClassTag[E]) = {
    cachedElem0(tag.runtimeClass, Nullable.None, args).asInstanceOf[E]
  }

  implicit def rewriteElement[A](implicit eA: Elem[A]): Elem[Rewrite[A]] =
    cachedElem[RewriteElem[A]](eA)

  def mkRewrite[A](x: Ref[A], y: Ref[A]): Ref[Rewrite[A]] = RewriteOp[A]()(x.elem).apply(x, y)

  implicit class PropEqualityOps[A](x: Ref[A]) {
    def <=>(y: Ref[A]): Ref[Rewrite[A]] = self.mkRewrite(x, y)
  }

  def postulate[A:Elem, R](p: Ref[A] => RRewrite[R]): RRewrite[R] = p(variable[A])
  def postulate[A:Elem, B:Elem, R](p: (Ref[A], Ref[B]) => RRewrite[R]): RRewrite[R] = p(variable[A], variable[B])
  def postulate[A:Elem, B:Elem, C:Elem, R](p: (Ref[A], Ref[B], Ref[C]) => RRewrite[R]): RRewrite[R] =
    p(variable[A], variable[B], variable[C])

  /** This is default implementation which delegates to global rewrite rules.
    * However, this can be overriden, as it is done in RewriteRules.
    * @hotspot: need to avoid allocations
    */
  def rewrite[T](s: Ref[T]): Sym = {
    var result: Sym = null
    if (rewriteRules.nonEmpty)
      result = rewriteWithRules(rewriteRules)(s)
    if (result != null) result
    else {
      if (performViewsLifting) {
        val d = s.node
        val v = rewriteViews(d)
        val res = if (v != null) v else rewriteDef(d)
        res
      } else
        rewriteDef(s.node)
    }
  }

  def rewriteWithRules[T](rules: List[RewriteRule[_]])(s: Ref[T]): Sym = {
    val eT = s.elem
    val iterator = rules.iterator
    var result: Sym = null
    while (iterator.hasNext && result == null) {
      val rule = iterator.next()
      if (eT <:< rule.eA)
        result = rule.asInstanceOf[RewriteRule[T]](s)
    }
    result
  }

  var rewriteRules = List[RewriteRule[_]]()

  def addRewriteRules(rules: RewriteRule[_]*) {
    rewriteRules ++= rules
  }
  def removeRewriteRules(rules: RewriteRule[_]*) {
    rewriteRules = rewriteRules.diff(rules)
  }

  trait RewriteRule[A] {
    def eA: Elem[A]
    def apply(x: Ref[A]): Sym
  }

  case class PatternRewriteRule[A](lhs: Ref[A], rhs: Ref[A], eA: Elem[A]) extends RewriteRule[A] {
    val g = new PGraph(rhs)
    import scalan.util.CollectionUtil._
    
    def apply(s: Ref[A]): Sym = {
      val optSubst = patternMatch(lhs, s)
      if (optSubst.isDefined) {
        val subst = optSubst.get
        val g1 = g.transform(DefaultMirror, NoRewriting, new MapTransformer(new AVHashMap(subst)))
        g1.roots.head
      } else null
    }
  }

  def patternRewriteRule[A](rewrite: RRewrite[A]): PatternRewriteRule[A] =
    rewrite match {
      case Def(ApplyBinOp(_: RewriteOp[_], lhs, rhs)) =>
        PatternRewriteRule(lhs, rhs, rewrite.elem.asInstanceOf[RewriteElem[A]].eA)
    }

  class RulesRewriter(rules: List[RewriteRule[_]]) extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] = {
      if (rules.isEmpty) return x
      val res = rewriteWithRules(rules)(x)
      if (res != null) res.asInstanceOf[Ref[T]]
      else x
    }
  }
}
