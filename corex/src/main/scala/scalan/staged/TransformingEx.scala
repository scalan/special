package scalan.staged

import scalan.RType.SingletonType

import scala.collection.{Seq, mutable}
import scalan.{ScalanEx, RType, DelayInvokeException}

import scala.reflect.{ClassTag, classTag}

trait TransformingEx { self: ScalanEx =>

  case class SingletonElem[T: ClassTag](value: T)
      extends BaseElemLiftable[T](value, SingletonType(value, classTag[T]))

  sealed abstract class KeyPath {
    def isNone = this == KeyPath.None
    def isAll = this == KeyPath.All
  }
  object KeyPath {
    case object Root extends KeyPath
    case object This extends KeyPath
    case object All extends KeyPath
    case object None extends KeyPath
    case object First extends KeyPath
    case object Second extends KeyPath
    case class Field(name: String) extends KeyPath
  }

  def keyPathElem(kp: KeyPath): Elem[KeyPath] = SingletonElem(kp)

  implicit class KeyPathElemOps(eKeyPath: Elem[KeyPath]) {
    def keyPath = eKeyPath.asInstanceOf[SingletonElem[KeyPath]].value
  }

  object DecomposeRewriter extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] = x match {
      case Def(d) => decompose(d) match {
        case None => x
        case Some(y) => y
      }
      case _ => x
    }
  }

  abstract class MirrorEx extends Mirror {

    protected def mirrorMetadata[A, B](t: Transformer, old: Ref[A], mirrored: Ref[B]) =
      (t, allMetadataOf(old))

    private def setMirroredMetadata(t1: Transformer, node: Sym, mirrored: Sym): (Transformer, Sym) = {
      val (t2, mirroredMetadata) = mirrorMetadata(t1, node, mirrored)
      setAllMetadata(mirrored, mirroredMetadata.filterSinglePass)
      (t2, mirrored)
    }

  }

  //  sealed abstract class TupleStep(val name: String)
  //  case object GoLeft extends TupleStep("L")
  //  case object GoRight extends TupleStep("R")
  type TuplePath = List[Int]

  def projectPath(x:Ref[Any], path: TuplePath) = {
    val res = path.foldLeft(x)((y,i) => TupleProjection(y.asInstanceOf[Ref[(Any,Any)]], i))
    res
  }

  // build projection from the root taking projection structure from the tree
  // assert(result.root == root)
  // NOTE: tree.root is not used
  def projectTree(root:Ref[Any], tree: ProjectionTree): ProjectionTree = {
    val newChildren = tree.children.map(child => {
      val i = projectionIndex(child.root)
      val newChildRoot = TupleProjection(root.asInstanceOf[Ref[(Any,Any)]], i)
      projectTree(newChildRoot, child)
    })
    ProjectionTree(root, newChildren)
  }

  def pairMany(env: List[Sym]): Sym =
    env.reduceRight(Pair(_, _))

  abstract class SymbolTree {
    def root: Sym
    def children: List[SymbolTree]
    def mirror(leafSubst: Sym => Sym): SymbolTree
    def paths: List[(TuplePath, Sym)]
    def isLeaf = children.isEmpty
  }

  class ProjectionTree(val root: Sym, val children: List[ProjectionTree]) extends SymbolTree {
    override def toString = s"""ProjTree(\n${paths.mkString("\n")})"""

    lazy val paths: List[(TuplePath, Sym)] =
      if (isLeaf) List((Nil, root))
      else{
        for {
          ch <- children
          (p, s) <- ch.paths
        } yield {
          val i = projectionIndex(ch.root)
          (i :: p, s)
        }
      }

    def mkNewTree(r: Sym, cs: List[ProjectionTree]) = ProjectionTree(r, cs)
    def mirror(subst: Sym => Sym): ProjectionTree = {
      val newRoot = subst(root)
      projectTree(newRoot, this)
    }
  }
  object ProjectionTree {
    def apply(root: Sym, children: List[ProjectionTree]) = new ProjectionTree(root, children)
    def apply(root: Sym, unfoldChildren: Sym => List[Sym]): ProjectionTree =
      ProjectionTree(root, unfoldChildren(root) map (apply(_, unfoldChildren)))
  }

  class TupleTree(val root: Sym, val children: List[TupleTree]) extends SymbolTree {
    override def toString =
      if (isLeaf) root.toString
      else "Tup(%s)".format(children.mkString(","))

    lazy val paths: List[(TuplePath, Sym)] = children match {
      case Nil => List((Nil, root))
      case _ =>
        for {
          (i,ch) <- children.indices.toList zip children
          (p, s) <- ch.paths
        } yield (i + 1 :: p, s)
    }

    def mirror(leafSubst: Sym => Sym): TupleTree =
      if (isLeaf)
        TupleTree(leafSubst(root), Nil)
      else {
        val newChildren = children map (_.mirror(leafSubst))
        val newRoot = pairMany(newChildren map (_.root))
        TupleTree(newRoot, newChildren)
      }
  }

  object TupleTree {
    def apply(root: Sym, children: List[TupleTree]) = new TupleTree(root, children)

    // require ptree to be sorted by projectionIndex
    def fromProjectionTree(ptree: ProjectionTree, subst: Sym => Sym): TupleTree =
      if (ptree.isLeaf)
        TupleTree(subst(ptree.root), Nil)
      else {
        val newChildren = ptree.children map (fromProjectionTree(_, subst))
        val newRoot = pairMany(newChildren map (_.root))
        TupleTree(newRoot, newChildren)
      }

    def unapply[T](s: Ref[T]): Option[TupleTree] = {
      s match {
        case Def(Tup(TupleTree(l),TupleTree(r))) =>
          Some(TupleTree(s, List(l, r)))
        case _ => Some(TupleTree(s, Nil))
      }
    }
  }

  /** Rewriter of the MethodCall nodes that can be invoked.
    * For such nodes has an effect of inlining the method body in place of MethodCall node. */
  object InvokeRewriter extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] = x.node match {
      case call: MethodCall =>
        call.tryInvoke match {
          case InvokeSuccess(res) =>
            res.asInstanceOf[Ref[T]]
          case InvokeFailure(e) =>
            if (e.isInstanceOf[DelayInvokeException])
              x
            else
              !!!(s"Failed to invoke $call", e, x)
          case _ => x
        }
      case _ => x
    }
  }

  abstract class Analyzer {
    def name: String
    override def toString = s"Analysis($name)"
  }

  trait Lattice[M[_]] {
    def maximal[T:Elem]: Option[M[T]]
    def minimal[T:Elem]: Option[M[T]]
    def join[T](a: M[T], b: M[T]): M[T]
  }

  trait BackwardAnalyzer[M[_]] extends Analyzer {
    type MarkedSym = (Ref[T], M[T]) forSome {type T}
    type MarkedSyms = Seq[MarkedSym]
    def keyPrefix: String = name

    def lattice: Lattice[M]
    def defaultMarking[T:Elem]: M[T]

    def updateMark[T](s: Ref[T], other: M[T]): (Ref[T], M[T]) = {
      s -> lattice.join(getMark(s), other)
    }

    def beforeAnalyze[A,B](l: Lambda[A,B]): Unit = {}

    def getInboundMarkings[T](thisSym: Ref[T], outMark: M[T]): MarkedSyms

    def getLambdaMarking[A,B](lam: Lambda[A,B], mDom: M[A], mRange: M[B]): M[A => B]

    def getMarkingKey[T](implicit eT:Elem[T]): MetaKey[M[T]] = markingKey[T](keyPrefix).asInstanceOf[MetaKey[M[T]]]

    def clearMark[T](s: Ref[T]): Unit = {
      implicit val eT = s.elem
      s.removeMetadata(getMarkingKey[T])
    }

    def getMark[T](s: Ref[T]): M[T] = {
      implicit val eT = s.elem
      val mark = s.getMetadata(getMarkingKey[T]).getOrElse(defaultMarking[T])
      mark
    }

    def hasMark[T](s: Ref[T]): Boolean = {
      implicit val eT = s.elem
      s.getMetadata(getMarkingKey[T]).isDefined
    }

    def updateOutboundMarking[T](s: Ref[T], mark: M[T]): Unit = {
      implicit val eT = s.elem
      val current = getMark(s)
      val updated = lattice.join(current, mark)
      val key = getMarkingKey[T]
      s.setMetadata(key)(updated, Some(true))
    }

    def backwardAnalyzeRec(g: AstGraph): Unit = {
      val revSchedule = g.schedule.reverseIterator
      for (sym <- revSchedule) sym match { case s: Ref[t] =>
        val d = s.node
        // back-propagate analysis information (including from Lambda to Lambda.y, see LevelAnalyzer)
        val outMark = getMark(s)
        val inMarks = getInboundMarkings[t](s, outMark)
        for ((s, mark) <- inMarks) {
          updateOutboundMarking(s, mark)
        }
        d match {
          // additionally if it is Lambda
          case l: Lambda[a,b] =>
            // analyze lambda after the markings were assigned to the l.y during previous propagation step
            backwardAnalyzeRec(l)
            // markings were propagated up to the lambda variable
            val mDom = getMark(l.x)
            val mRange = getMark(l.y)

            // update markings attached to l
            val lMark = getLambdaMarking(l, mDom, mRange)
            updateOutboundMarking(l.self, lMark)
          case _ =>
        }
      }
    }
  }

  trait Marking[T] {
    def elem: Elem[T]
    def basePath: KeyPath = KeyPath.Root
    def nonEmpty: Boolean
  }

  class EmptyMarking[T](val elem: Elem[T]) extends Marking[T] {
    def nonEmpty = false
  }

  type MarkedSym = (Ref[T], Marking[T]) forSome {type T}
  type MarkedSyms = Seq[MarkedSym]

  implicit def markingRType[A](implicit tA: RType[A]): RType[Marking[A]] = MarkingType(tA)

  case class MarkingType[A](tA: RType[A]) extends RType[Marking[A]] {
    val classTag: ClassTag[Marking[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Marking[A]]
    }
    override def name: String = s"Marking[${tA.name}]"
    override def isConstantSize: Boolean = false
  }

  class MarkingElem[T](implicit eT: Elem[T])
    extends BaseElemLiftable[Marking[T]](
      new EmptyMarking[T](element[T]), markingRType(eT.sourceType.asInstanceOf[RType[T]]))

  implicit def markingElem[T:Elem] = new MarkingElem[T]

  private val markingKeys = mutable.Map.empty[(String, Elem[_]), MetaKey[_]]

  def markingKey[T](prefix: String)(implicit eT:Elem[T]): MetaKey[Marking[T]] = {
    val key = markingKeys.getOrElseUpdate((prefix, eT), MetaKey[Marking[T]](s"${prefix}_marking[${eT.name}]"))
    key.asInstanceOf[MetaKey[Marking[T]]]
  }
}
