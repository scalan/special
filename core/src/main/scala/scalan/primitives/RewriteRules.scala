package scalan.primitives

import scalan.{Base, Scalan}
import scala.reflect.runtime.universe._
import scalan.util.Invariant

trait RewriteRules extends Base { self: Scalan =>
  case class Rewrite[A](lhs: Rep[A], rhs: Rep[A])
  case class RewriteOp[A: Elem]() extends BinOp[A, Rewrite[A]]("==>", (x, y) => Rewrite[A](x, y))

  type RRewrite[A] = Rep[Rewrite[A]]

  case class RewriteElem[A](eA: Elem[A]) extends Elem[Rewrite[A]] {
    protected def getDefaultRep = {
      val defA = eA.defaultRepValue
      mkRewrite(defA, defA)
    }
    lazy val tag = {
      implicit val tag1 = eA.tag
      implicitly[WeakTypeTag[Rewrite[A]]]
    }
    override def buildTypeArgs = TypeArgs("A" -> (eA -> Invariant))
  }

  implicit def rewriteElement[A](implicit eA: Elem[A]): Elem[Rewrite[A]] =
    cachedElem[RewriteElem[A]](eA)

  def mkRewrite[A](x: Rep[A], y: Rep[A]): Rep[Rewrite[A]] = RewriteOp[A]()(x.elem).apply(x, y)

  implicit class PropEqualityOps[A](x: Rep[A]) {
    def <=>(y: Rep[A]): Rep[Rewrite[A]] = self.mkRewrite(x, y)
  }

  def postulate[A:Elem, R](p: Rep[A] => RRewrite[R]): RRewrite[R] = p(variable[A])
  def postulate[A:Elem, B:Elem, R](p: (Rep[A], Rep[B]) => RRewrite[R]): RRewrite[R] = p(variable[A], variable[B])
  def postulate[A:Elem, B:Elem, C:Elem, R](p: (Rep[A], Rep[B], Rep[C]) => RRewrite[R]): RRewrite[R] =
    p(variable[A], variable[B], variable[C])

  /** This is default implementation which delegates to global rewrite rules.
    * However, this can be overriden, as it is done in RewriteRules.
    * @hotspot: need to avoid allocations
    */
  def rewrite[T](s: Rep[T]): Sym = {
    var result: Sym = null
    if (rewriteRules.nonEmpty)
      result = rewriteWithRules(rewriteRules)(s)
    if (result != null) result
    else {
      if (performViewsLifting) {
        val d = s.rhs
        val v = rewriteViews(d)
        val res = if (v != null) v else rewriteDef(d)
        res
      } else
        rewriteDef(s.rhs)
    }
  }

  def rewriteWithRules[T](rules: List[RewriteRule[_]])(s: Rep[T]): Sym = {
    val eT = s.elem
    val iterator = rules.iterator
    var result: Sym = null
    while (iterator.hasNext && result == null) {
      val rule = iterator.next()
      if (rule.eA >:> eT)
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
    def apply(x: Rep[A]): Sym
  }

  case class PatternRewriteRule[A](lhs: Rep[A], rhs: Rep[A], eA: Elem[A]) extends RewriteRule[A] {
    val g = new PGraph(rhs)
    import scalan.util.CollectionUtil._
    
    def apply(s: Rep[A]): Sym = {
      val optSubst = patternMatch(lhs, s)
      if (optSubst.isDefined) {
        val subst = optSubst.get
        val g1 = g.transform(DefaultMirror, NoRewriting, new MapTransformer(subst.toImmutableMap))
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
    def apply[T](x: Rep[T]): Rep[T] = {
      if (rules.isEmpty) return x
      val res = rewriteWithRules(rules)(x)
      if (res != null) res.asInstanceOf[Rep[T]]
      else x
    }
  }
}
