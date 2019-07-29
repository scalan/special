package scalan.primitives

import java.util

import scalan.staged.ProgramGraphs
import scalan.util.{GraphUtil, NeighbourFunc, Neighbours}
import scalan.{Lazy, Base, Nullable, Scalan}
import debox.{Set => DSet, Buffer => DBuffer}
import scala.collection.mutable
import scala.language.implicitConversions
import spire.syntax.all.cfor

trait Functions extends Base with ProgramGraphs { self: Scalan =>

  implicit class LambdaOps[A,B](f: Rep[A => B]) {
    def apply(x: Rep[A]): Rep[B] = mkApply(f, x)
    def >>[C](g: Rep[B => C]) = compose(g, f)
    def <<[C](g: Rep[C => A]) = compose(f, g)
  }

  /** Global lambda equality mode used by default. It is used in `fun` and `fun2` lambda builders.
    * If this flag is `true` then Lambda nodes are equal if they are the same up to renaming of symbols. (see Lambda.equals()).
    * Each Lambda node has independent equality mode flag which is setup in the constructor. */
  var useAlphaEquality: Boolean = true

  /** Global flag governing lambda reification in `fun` and `mkLambda`.
    * If this flag is `true` then original `f: Rep[A] => Rep[B]` function is stored in Lambda node.
    * As a consequence if `f` is not stored, then `unfoldLambda` is done by `mirrorLambda`. */
  var keepOriginalFunc: Boolean = true

  /** Turns on/off lambda unfolding using original function `f` stored in the Lambda node.
    * If this flag is `false` then this function cannot be used even if it is present in the node. */
  var unfoldWithOriginalFunc: Boolean = true

  implicit def fun[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, true, useAlphaEquality, keepOriginalFunc)
  implicit def fun2[A,B,C](f: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = mkLambda(f)
  def funGlob[A,B](f: Rep[A] => Rep[B])(implicit eA: LElem[A]): Rep[A => B] = mkLambda(f, false, true, keepOriginalFunc)

  // more convenient to call with explicit eA
  def typedfun[A, B](eA: Elem[A])(f: Rep[A] => Rep[B]): Rep[A => B] =
    fun(f)(Lazy(eA))

  // see BooleanFunctionOps for example usage
  def sameArgFun[A, B, C](sample: Rep[A => C])(f: Rep[A] => Rep[B]): Rep[A => B] =
    typedfun(sample.elem.eDom)(f)

  def composeBi[A, B, C, D](f: Rep[A => B], g: Rep[A => C])(h: (Rep[B], Rep[C]) => Rep[D]): Rep[A => D] = {
    sameArgFun(f) { x => h(f(x), g(x)) }
  }

  class Lambda[A, B](val f: Nullable[Rep[A] => Rep[B]], val x: Rep[A], val y: Rep[B], val mayInline: Boolean, val alphaEquality: Boolean = true)
    extends Def[A => B] with AstGraph with Product { thisLambda =>
    def eA = x.elem
    def eB = y.elem

    private var _selfType: Elem[A => B] = _
    def selfType: Elem[A => B] = {
      if (_selfType != null) _selfType
      else {
        val res = funcElement(eA, eB)
        if (!y.isPlaceholder) _selfType = res  // memoize once y is assigned
        res
      }
    }

    // ensure all lambdas of the same type have the same hashcode,
    // so they are tested for alpha-equivalence using equals
    private var _hashCode: Int = 0
    override def hashCode: Int = {
      if (_hashCode == 0) {
        _hashCode = if (alphaEquality)
          41 * (41 + x.elem.hashCode) + y.elem.hashCode
        else
          41 * (41 + x.hashCode) + y.hashCode
      }
      _hashCode
    }

    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) ||
      (other match {
        case other: Lambda[_,_] =>
          if (alphaEquality)
            matchLambdas(this, other, false, emptyMatchSubst).isDefined
          else
            other.x == this.x && other.y == this.y
        case _ => false
      })

    override def toString = s"Lambda(${if (f.isDefined) "f is Some" else "f is None"}, $x => $y})"
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]

    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => x
      case 1 => y
      case _ => throw new NoSuchElementException(s"Lambda.productElement($n) is undefined")
    }
    def productArity: Int = 2

    // AstGraph implementation
    val boundVars = Array(x)
    val boundVarId = x.rhs._nodeId
    val roots = y :: Nil

    override lazy val rootIds: DBuffer[Int] = super.rootIds

    override lazy val freeVars = super.freeVars

    @inline override def isBoundVar(s: Sym) = s.rhs.nodeId == boundVarId

    override lazy val  scheduleIds: DBuffer[Int] = {
      val sch = if (isIdentity)
        DBuffer.ofSize[Int](0)
      else {
        // graph g will contain all Defs reified as part of this Lambda, (due to `filterNode`)
        // BUT not all of them depend on boundVars, thus we need to filter them out
        // 1) we build g.schedule and then g.usageMap
        // 2) collect set of nodes, which depend on `x`
        val g = new PGraph(roots, filterNode = Nullable(s => s.rhs._nodeId >= boundVarId))
        val locals = GraphUtil.depthFirstSetFrom[Int](DBuffer(boundVarId))(
          new Neighbours({ id => g.usagesOf(id) })
        )
        val gschedule = g.schedule.toArray
        val len = gschedule.length
        val sch = DBuffer.ofSize[Int](len)
        cfor(0)(_ < len, _ + 1) { i =>
          val sym = gschedule(i)
          val id = sym.rhs.nodeId
          if (locals(id) && !sym.isVar)
            sch += id
        }
        val currSch = if (sch.isEmpty) g.rootIds else sch
        currSch
      }
      sch
    }

    override protected def getDeps: Array[Sym] = freeVars.toArray

    def isGlobalLambda: Boolean = {
      freeVars.forall { x =>
        val xIsGlobalLambda = x.isLambda && { val Def(lam: Lambda[_, _]) = x; lam.isGlobalLambda }
        x.isConst || xIsGlobalLambda
      }
    }
  }

  type LambdaData[A,B] = (Lambda[A,B], Nullable[Rep[A] => Rep[B]], Rep[A], Rep[B])
  object Lambda {
    def unapply[A,B](lam: Lambda[A, B]): Nullable[LambdaData[A,B]] = {
      val res: LambdaData[A,B] =
        if (lam == null) null
        else {
          (lam, lam.f, lam.x, lam.y)
        }
      Nullable(res)
    }
  }

  override def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
    case l: Lambda[a, b] =>
      val newLam = new Lambda(Nullable.None, t(l.x), t(l.y), l.mayInline, l.alphaEquality)
      val newSym = newLam.self
      asRep[A](toExp(newLam, newSym))
    case _ => super.transformDef(d, t)
  }

  /**
   * Matcher for lambdas which don't depend on their arguments
   * (but can close over other expressions, unlike VeryConstantLambda).
   */
  object ConstantLambda {
    // if lam.y depends on lam.x indirectly, lam.schedule must contain the dependency path
    // and its length will be > 1
    def unapply[A,B](lam: Lambda[A, B]): Option[Rep[B]] =
      if (lam.schedule.length <= 1 && !lam.y.rhs.deps.contains(lam.x) && lam.y != lam.x)
        Some(lam.y)
      else
        None
  }

  /**
   * Matcher for lambdas which return staging-time constants.
   * VeryConstantLambda(x) should be equivalent to ConstantLambda(Def(Const(x)))
   */
  object VeryConstantLambda {
    def unapply[A,B](lam: Lambda[A, B]): Option[B] = lam.y match {
      case Def(Const(y)) => Some(y)
      case _ => None
    }
  }

  // matcher version of Lambda.isIdentity
  object IdentityLambda {
    def unapply[A,B](lam: Lambda[A, B]): Boolean = lam.isIdentity
  }

  case class Apply[A,B](f: Rep[A => B], arg: Rep[A], mayInline: Boolean = true) extends Def[B] {
    def selfType = f.elem.eRange
    override def transform(t: Transformer) = Apply(t(f), t(arg), mayInline)
  }

  implicit class FuncExtensions[A, B](f: Rep[A=>B]) {
    implicit def eA = f.elem.eDom
    def getLambda: Lambda[A,B] = f match {
      case Def(lam: Lambda[_,_]) => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!(s"Expected symbol of Lambda node but was $f", f)
    }

    def zip[C](g: Rep[A=>C]): Rep[A=>(B,C)] = {
      implicit val eB = f.elem.eRange
      implicit val eC = g.elem.eRange
      fun { (x: Rep[A]) => Pair(f(x), g(x)) }
    }
  }

  type Subst = java.util.HashMap[Sym, Sym]
  @inline def emptyMatchSubst: Subst = new util.HashMap[Sym,Sym]()

  def alphaEqual(s1: Sym, s2: Sym): Boolean = matchExps(s1, s2, false, emptyMatchSubst).isDefined

  def patternMatch(s1: Sym, s2: Sym): Nullable[Subst] = matchExps(s1, s2, true, emptyMatchSubst)

  protected def matchExps(s1: Sym, s2: Sym, allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = s1 match {
    case _ if s1 == s2 || subst.get(s1) == s2 || subst.get(s2) == s1 =>
      Nullable(subst)
    case Def(d1) if !d1.isInstanceOf[Variable[_]] => s2 match {
      case Def(d2) =>
        val res = matchDefs(d1, d2, allowInexactMatch, subst)
        if (res.isDefined) {
          res.get.put(s1, s2)
        }
        res
      case _ => Nullable.None
    }
    case _ =>
      if (allowInexactMatch && !subst.containsKey(s1)) {
        subst.put(s1, s2)
        Nullable(subst)
      } else {
        Nullable.None
      }
  }

  @inline
  private def matchLambdas(lam1: Lambda[_, _], lam2: Lambda[_, _], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] =
    if (lam1.x.elem == lam2.x.elem) {
      subst.put(lam1.x, lam2.x)
      matchExps(lam1.y, lam2.y, allowInexactMatch, subst)
    }
    else
      Nullable.None

  protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = d1 match {
    case lam1: Lambda[_, _] => d2 match {
      case lam2: Lambda[_, _] =>
        matchLambdas(lam1, lam2, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case _ =>
      if (d1.getClass == d2.getClass && d1.productArity == d2.productArity && d1.selfType.name == d2.selfType.name) {
        matchIterators(d1.productIterator, d2.productIterator, allowInexactMatch, subst)
      } else
        Nullable.None
  }

  // generalize to Seq or Iterable if we get nodes with deps of these types
  protected def matchIterators(i1: Iterator[_], i2: Iterator[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] =
    if (i1.hasNext) {
      if (i2.hasNext) {
        var res = matchAny(i1.next(), i2.next(), allowInexactMatch, subst)
        if (res.isDefined)
          res = matchIterators(i1, i2, allowInexactMatch, res.get)
        res
      } else Nullable.None
    } else {
      if (i2.hasNext) Nullable.None else Nullable(subst)
    }

  protected def matchAny(a1: Any, a2: Any, allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = a1 match {
    case s1: Sym => a2 match {
      case s2: Sym =>
        matchExps(s1, s2, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case l1: Iterable[_] => a2 match {
      case l2: Iterable[_] =>
        matchIterators(l1.iterator, l2.iterator, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case _ => if (a1 == a2) Nullable(subst) else Nullable.None
  }

  //=====================================================================================
  //   Function application

  def mkApply[A,B](f: Rep[A => B], x: Rep[A]): Rep[B] = {
    val d = f.rhs
    if (d.isInstanceOf[Lambda[_, _]]) {
      val lam = d.asInstanceOf[Lambda[A, B]]
      if (lam.mayInline) {
        return unfoldLambda(lam, x)
      }
    }
    Apply(f, x, mayInline = false)
  }

  def unfoldLambda[A,B](lam: Lambda[A,B], x: Rep[A]): Rep[B] = {
    lam.f match {
      case Nullable(g) if unfoldWithOriginalFunc => g(x) // unfold initial non-recursive function
      case _ => mirrorApply(lam, x)  // f is mirrored, unfold it by mirroring
    }
  }

  def unfoldLambda[A,B](f: Rep[A=>B], x: Rep[A]): Rep[B] = {
    val lam = f.getLambda
    unfoldLambda(lam, x)
  }

  def mirrorApply[A,B](lam: Lambda[A, B], s: Rep[A]): Rep[B] = {
    val body = lam.scheduleIds
    val t = DefaultMirror.mirrorSymbols(new MapTransformer(lam.x -> s), NoRewriting, lam, body)
    t(lam.y).asInstanceOf[Rep[B]]
  }

  //=====================================================================================
  //   Function reification

  def mkLambda[A,B](f: Rep[A] => Rep[B],
                    mayInline: Boolean,
                    alphaEquality: Boolean,
                    keepOriginalFunc: Boolean)(implicit eA: LElem[A]): Rep[A=>B] = {
    val x = variable[A]
    lambda(x)(f, mayInline, alphaEquality, keepOriginalFunc)
  }

  def mkLambda[A,B,C](f: Rep[A]=>Rep[B]=>Rep[C])
                     (implicit eA: LElem[A], eB: Elem[B]): Rep[A=>B=>C] = {
    val y = variable[B]
    mkLambda(
      (a: Rep[A]) => lambda(y)((b:Rep[B]) => f(a)(b), mayInline = true, useAlphaEquality, keepOriginalFunc),
      mayInline = true,
      alphaEquality = useAlphaEquality,
      keepOriginalFunc = keepOriginalFunc)
  }

  def mkLambda[A,B,C](f: (Rep[A], Rep[B])=>Rep[C])(implicit eA: LElem[A], eB: LElem[B]): Rep[((A,B))=>C] = {
    implicit val leAB = Lazy(pairElement(eA.value, eB.value))
    mkLambda({ (p: Rep[(A, B)]) =>
      val (x, y) = unzipPair(p)
      f(x, y)
    }, true, useAlphaEquality, keepOriginalFunc)
  }

  var lambdaStack: List[Lambda[_,_]] = Nil

  private def lambda[A,B](x: Rep[A])(f: Rep[A] => Rep[B],
                                     mayInline: Boolean,
                                     alphaEquality: Boolean,
                                     keepOriginalFunc: Boolean)(implicit leA: LElem[A]): Rep[A=>B] = {
//    implicit val eA = leA.value

    // ySym will be assigned after f is executed
    val ySym = placeholder(LazyAnyElement).asInstanceOf[Rep[B]]

    val orig = if (keepOriginalFunc) Nullable(f) else Nullable.None
    val lam = new Lambda(orig, x, ySym, mayInline, alphaEquality)
    val lamSym = lam.self

    val oldStack = lambdaStack
    try {
      lambdaStack = lam :: lambdaStack
      val y = reifyEffects(f(x))
      ySym.assignDefFrom(y)
    }
    finally {
      lambdaStack = oldStack
    }

    findOrCreateDefinition(lam, lamSym)
  }

  class LambdaStack {
    var stack = List[Sym]()
    def top: Option[Sym] = stack.isEmpty match { case true => None case _ => Some(stack.head) }
    def push(e: Sym): this.type = { stack = e :: stack; this }
    def pop: Sym = {
      val res = stack.head;
      stack = stack.tail;
      res
    }
  }

  def identityFun[A](implicit e: Elem[A]) = fun[A, A](x => x)

  def upcastFun[A: Elem, B >: A]: Rep[A => B] = fun[A,B](x => x)

  def constFun[A, B](x: Rep[B])(implicit e: Elem[A]) = {
    implicit val eB = x.elem
    fun[A, B](_ => x)
  }

  def compose[A, B, C](f: Rep[B => C], g: Rep[A => B]): Rep[A => C] = {
    implicit val eA = g.elem.eDom
    implicit val eC = f.elem.eRange
    fun { x => f(g(x)) }
  }

}
