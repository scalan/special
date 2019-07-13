package scalan.primitives

import scalan.compilation.{GraphVizConfig, GraphVizExport}
import scalan.{Liftable => _, _}
import debox.{Set => DSet}
import scala.reflect.runtime.universe._
import scalan.util.Covariant

trait Thunks extends Functions with ViewsModule with GraphVizExport { self: Scalan =>
  import IsoUR._
  
  type Th[+T] = Rep[Thunk[T]]
  trait Thunk[+A] { def value: A }
  class ThunkCompanion {
    def apply[T](block: => Rep[T]) = thunk_create(block)
    def forced[T](block: => Rep[T]) = thunk_create(block).force
  }
  val Thunk: ThunkCompanion = new ThunkCompanion

  implicit class RepThunkOps[T](t: Th[T]) {
    def force() = thunk_force(t)
    def map[R](f: Rep[T => R]): Th[R] = thunk_map(t, f)
    def map[R](f: Rep[T] => Rep[R]): Th[R] = thunk_map1(t, f)
  }

  implicit val thunkCont: Cont[Thunk] = new Cont[Thunk] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Thunk[T]]
    def lift[T](implicit eT: Elem[T]) = element[Thunk[T]]
    def unlift[T](implicit eFT: Elem[Thunk[T]]) = eFT.eItem
    def getElem[T](fa: Rep[Thunk[T]]) = !!!("Operation is not supported by Thunk container " + fa)
    def unapply[T](e: Elem[_]) = e match {
      case e: ThunkElem[_] => Some(e.asElem[Thunk[T]])
      case _ => None
    }
  }

  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SThunk[T] = () => T

  case class ThunkConst[ST, T](constValue: SThunk[ST], lT: Liftable[ST, T])
      extends BaseDef[Thunk[T]]()(thunkElement(lT.eW))
         with LiftedConst[SThunk[ST], Thunk[T]] {
    val liftable: Liftable[SThunk[ST], Thunk[T]] = liftableThunk(lT)
  }

  case class LiftableThunk[ST, T](lT: Liftable[ST, T]) extends Liftable[SThunk[ST], Thunk[T]] {
    import RType._
    def eW: Elem[Thunk[T]] = thunkElement(lT.eW)
    def sourceType: RType[SThunk[ST]] = {
      implicit val tST = lT.sourceType
      RType[SThunk[ST]]
    }
    def lift(x: SThunk[ST]): Rep[Thunk[T]] = ThunkConst(x, lT)
    def unlift(w: Rep[Thunk[T]]): SThunk[ST] = w match {
      case Def(ThunkConst(x: SThunk[_], l)) if l == lT => x.asInstanceOf[SThunk[ST]]
      case _ => unliftError(w)
    }
  }

  implicit def liftableThunk[ST,T](implicit lT: Liftable[ST,T]): Liftable[SThunk[ST], Thunk[T]] =
    LiftableThunk(lT)


  case class ThunkElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, Thunk[A], Thunk](eItem, container[Thunk]) {
    override lazy val liftable = asLiftable[SThunk[_], Thunk[A]](liftableThunk(eItem.liftable))
    def parent: Option[Elem[_]] = None
    override lazy val typeArgs = TypeArgs("A" -> (eItem -> Covariant))
    lazy val tag = {
      implicit val rt = eItem.tag
      weakTypeTag[Thunk[A]]
    }
    protected def getDefaultRep = Thunk(eItem.defaultRepValue)
  }

  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] =
    cachedElemByClass(eItem)(classOf[ThunkElem[T]])
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  class ThunkDef[A](val root: Rep[A], _schedule: =>Schedule)
    extends Def[Thunk[A]] with AstGraph with Product {

    implicit def eA: Elem[A] = root.elem
    private var _selfType: Elem[Thunk[A]] = _
    def selfType: Elem[Thunk[A]] =
      if (_selfType != null) _selfType
      else {
        val res = thunkElement(eA)
        if (!root.isPlaceholder) _selfType = res  // memoize once root is assigned
        res
      }

    override lazy val schedule: Schedule = _schedule
    // structural equality pattern implementation
    override lazy val hashCode: Int = _nodeId //41 * (41 + root.hashCode) + schedule.hashCode
    override def equals(other: Any) =
      other match {
        case that: ThunkDef[_] => _nodeId == that._nodeId
//            (this.root equals that.root) &&
//            (this.schedule equals that.schedule)
        case _ => false
      }
    override def toString = s"Th($root, [${scheduleSyms.mkString(",")}])"
    def canEqual(other: Any) = other.isInstanceOf[ThunkDef[_]]

    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => root
      case _ => throw new NoSuchElementException(s"ThunkDef.productElement($n) is undefined")
    }
    def productArity: Int = 1

    override def boundVars = Nil
    override lazy val freeVars = if (schedule.isEmpty) Set(root) else super.freeVars
    val roots = new scala.collection.immutable.::(root, Nil) // optimization of hot spot
  }
  object ThunkDef {
    def unapply(d: ThunkDef[_]): Option[(Rep[T], Schedule) forSome {type T}] = d match {
      case th: ThunkDef[_] => Some((th.root, th.schedule))
      case _ => None
    }
  }

  override def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
    case thunk: ThunkDef[a] =>
      sys.error(s"Thunk should be transformed in mirrorThunk")
    case _ => super.transformDef(d, t)
  }

  case class ThunkView[A, B](source: Rep[Thunk[A]])(innerIso: Iso[A, B])
    extends View1[A, B, Thunk](thunkIso(innerIso)) {
  }

  override def unapplyViews[T](s: Rep[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ThunkView[_,_]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]


  class ThunkScope(val parent: ThunkScope, val thunkSym: Rep[Any]) {
    private val bodyIds: DSet[Int] = DSet.ofSize(16)
    private val bodyDefs: AVHashMap[Def[_], Def[_]] = AVHashMap(32)

    @inline final def isEmptyBody: Boolean = bodyIds.isEmpty

    def +=(sym: Sym): Unit = {
      val d = sym.rhs
      bodyIds += d.nodeId
      bodyDefs.put(d, d)
    }

    def scheduleForResult(root: Rep[Any]): Schedule = {
      buildScheduleForResult(Array(root), _.getDeps.filter(s => bodyIds(s.rhs.nodeId) && !s.isVar))
    }

    // TODO optimize: this is performance hotspot (use ArrayBuilder instead of ListBuffer)
    def findDef[T](d: Def[T]): Rep[T] = {
      val existingOpt = bodyDefs.get(d)
      if (existingOpt.isDefined) return existingOpt.get.self.asInstanceOf[Rep[T]]
      if (parent == null)
        findGlobalDefinition(d)
      else
        parent.findDef(d)
    }
  }

  class ThunkStack {
    var stack = List[ThunkScope]()
    @inline def top: Nullable[ThunkScope] = if (stack.isEmpty) Nullable.None else Nullable(stack.head)
    def push(e: ThunkScope): this.type = { stack = e :: stack; this }
    @inline def pop: ThunkScope = {
      val res = stack.head
      stack = stack.tail
      res
    }
    def beginScope(thunkSym: Rep[Any]): ThunkScope = {
      val parent = if (stack.isEmpty) null else stack.head
      val scope = new ThunkScope(parent, thunkSym)
      this.push(scope)
      scope
    }
    @inline def endScope(): Unit = { this.pop }
  }
  protected val thunkStack = new ThunkStack

  protected def currentThunkSym = thunkStack.top match {
    case Nullable(scope) => scope.thunkSym
    case _ => globalThunkSym
  }

  implicit def repToThunk[A](block: Rep[A]): Rep[Thunk[A]] = thunk_create(block)

  def thunk_create[A](block: => Rep[A]): Rep[Thunk[A]] = {
    var schedule: Schedule = null
    val resPH = placeholder(Lazy(AnyElement)).asInstanceOf[Rep[A]] // will be known after block is evaluated
    val newThunk = new ThunkDef(resPH, { assert(schedule != null); schedule })
    val newThunkSym = newThunk.self

    val newScope = thunkStack.beginScope(newThunkSym)
    // execute block and add all new definitions to the top scope (see createDefinition)
    // reify all the effects during block execution
    val res = reifyEffects(block)
    resPH.assignDefFrom(res)
    schedule =
      if (res.isVar) Nil
      else if (newScope.isEmptyBody)  Nil
      else newScope.scheduleForResult(res)

    val sh = newThunk.schedule  // force lazy value in newThunk (see ThunkDef._schedule argument above)
    thunkStack.endScope()
    toExp(newThunk, newThunkSym)
  }

  def thunk_map[A, B](t: Th[A], f: Rep[A => B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }
  def thunk_map1[A, B](t: Th[A], f: Rep[A] => Rep[B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }

  var isInlineThunksOnForce = false

  def forceThunkByMirror[A](thunk: Th[A], subst: MapTransformer = MapTransformer.Empty): Rep[A] = {
    val Def(th: ThunkDef[A]) = thunk
    forceThunkDefByMirror(th, subst)
  }
  def forceThunkDefByMirror[A](th: ThunkDef[A], subst: MapTransformer = MapTransformer.Empty): Rep[A] = {
    val body = th.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(subst, NoRewriting, th, body)
    t(th.root).asInstanceOf[Rep[A]]
  }

  def thunk_force[A](t: Th[A]): Rep[A] =
    if (isInlineThunksOnForce)
      t match {
        case Def(th@ThunkDef(_, _)) =>
          forceThunkByMirror(t)
        case _ => ThunkForce(t)
      }
    else
      ThunkForce(t)

  case class ThunkForce[A](thunk: Rep[Thunk[A]]) extends Def[A] {
    implicit def selfType = thunk.elem.eItem
    override def transform(t: Transformer) = ThunkForce(t(thunk))
  }

  override protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = d1 match {
    case ThunkDef(root1, sch1) => d2 match {
      case ThunkDef(root2, sch2) =>
        var res = matchIterators(sch1.iterator, sch2.iterator, allowInexactMatch, subst)
        if (res.isDefined)
          res = matchExps(root1, root2, allowInexactMatch, res.get)
        res
      case _ => Nullable.None
    }
    case _ =>
      super.matchDefs(d1, d2, allowInexactMatch, subst)
  }

  object ConstantThunk {
    def unapply(d: Def[_]): Option[Rep[_]] = d match {
      case ThunkDef(root @ Def(Const(_)), sch) if sch == Seq(root) => Some(root)
      case _ => None
    }
    def unapply(s: Sym): Option[Rep[_]] = s match { case Def(d) => unapply(d) case _ => None }
  }

  override def rewriteViews[T](d: Def[T]) = d match {
    case th @ ThunkDef(HasViews(srcRes, iso: Iso[a,b]), _) => {
      implicit val eA = iso.eFrom
      implicit val eB = iso.eTo
      val newTh = Thunk { iso.from(forceThunkDefByMirror(th.asInstanceOf[ThunkDef[b]])) }   // execute original th as part of new thunk
      ThunkView(newTh)(iso)
    }
    case ThunkForce(HasViews(srcTh, Def(iso: ThunkIso[a, b]))) => {
      val innerIso = iso.innerIso
      implicit val eA = innerIso.eFrom
      innerIso.to(srcTh.asInstanceOf[Rep[Thunk[a]]].force)
    }
    case _ => super.rewriteViews(d)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case ThunkForce(ConstantThunk(root)) => root
    case ApplyBinOpLazy(op, l, ConstantThunk(root)) => op.apply(l, root)
    case _ => super.rewriteDef(d)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case ThunkDef(r, sch) => s"Thunk($r, [${sch.mkString(",")}])"
    case _ => super.formatDef(d)
  }

  override protected def nodeColor(td: TypeDesc, d: Def[_])(implicit config: GraphVizConfig) = td match {
    case _: ThunkElem[_] => "red"
    case _ => super.nodeColor(td, d)
  }
}
