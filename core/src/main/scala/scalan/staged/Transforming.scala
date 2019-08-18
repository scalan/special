package scalan.staged

import java.lang.reflect.Method
import java.util

import scalan.{Nullable, DelayInvokeException, Lazy, Scalan, AVHashMap}
import debox.{Buffer => DBuffer}
import spire.syntax.all.cfor

trait Transforming { self: Scalan =>

  abstract class Pass {
    def name: String
    def config: PassConfig = Pass.defaultPassConfig
    def doFinalization(): Unit = {}
    /**
      * Pass specific optional decision.
      * @param d receiver of the method
      * @param m method to invoke
      * @return Some(decision) if some this Pass defines some logic, None - then core behavior is used
      */
    def isInvokeEnabled(d: Def[_], m: Method): Option[Boolean] = None
  }
  object Pass {
    val defaultPassName = "default"
    val defaultPass = new DefaultPass(defaultPassName)
    val defaultPassConfig = defaultPass.config
  }

  case class PassConfig(
                         shouldUnpackTuples: Boolean = false,
                         shouldExtractFields: Boolean = true,
                         constantPropagation: Boolean = true,
                         shouldSlice: Boolean = false
                       )
  {
    def withConstantPropagation(value: Boolean) = this.copy(constantPropagation = value)
  }

  class DefaultPass(val name: String, override val config: PassConfig = PassConfig()) extends Pass

  //TODO Current design doesn't allow to run through passes in two Compilers in parallel
  var _currentPass: Pass = Pass.defaultPass
  def currentPass = _currentPass

  def beginPass(pass: Pass): Unit = {
    _currentPass = pass
  }
  def endPass(pass: Pass): Unit = {
    _currentPass = Pass.defaultPass
  }


  case class MapTransformer(private val subst: util.HashMap[Sym, Sym]) extends Transformer {
    def this(substPairs: (Sym, Sym)*) {
      this({
        val map = new util.HashMap[Sym, Sym](1000)
        val len = substPairs.length
        cfor(0)(_ < len, _ + 1) { i =>
          val kv = substPairs(i)
          map.put(kv._1, kv._2)
        }
        map
      })
    }
    def apply[A](x: Ref[A]): Ref[A] = {
      val y = subst.get(x)
      if (y == null || y == x) return x
      apply(y.asInstanceOf[Ref[A]]) // transitive closure
    }
    def isDefinedAt(x: Ref[_]) = subst.containsKey(x)
    def domain: Seq[Ref[_]] = subst.keySet.toArray(new Array[Sym](0))

    def +[A](key: Sym, value: Sym): Transformer = {
      subst.put(key, value)
      this
    }
    def merge(other: Transformer): Transformer =
      other.domain.foldLeft[Transformer](this) {
        case (t, s: Sym) => t + (s, other(s))
      }

    override def toString = if (subst.isEmpty) "MapTransformer.Empty" else s"MapTransformer($subst)"
  }

  object MapTransformer {
    def empty(initialCapacity: Int = 100) = new MapTransformer(new util.HashMap[Sym, Sym](initialCapacity))
  }

  implicit class PartialRewriter(pf: PartialFunction[Sym, Sym]) extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] =
      if (pf.isDefinedAt(x))
        pf(x).asInstanceOf[Ref[T]]
      else
        x
  }

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

  abstract class Rewriter { self =>
    def apply[T](x: Ref[T]): Ref[T]

    def orElse(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Ref[T]) = {
        val y = self(x)
        (x == y) match { case true => other(x) case _ => y }
      }
    }
    def andThen(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Ref[T]) = {
        val y = self(x)
        val res = other(y)
        res
      }
    }

    def |(other: Rewriter) = orElse(other)
    def ~(other: Rewriter) = andThen(other)
  }

  val NoRewriting: Rewriter = new Rewriter {
    def apply[T](x: Ref[T]) = x
  }

  abstract class Mirror {
    def apply[A](t: Transformer, rewriter: Rewriter, node: Ref[A], d: Def[A]): Sym = d.mirror(t)

    protected def mirrorElem(node: Sym): Elem[_] = node.elem

    // every mirrorXXX method should return a pair (t + (v -> v1), v1)
    protected def mirrorVar[A](t: Transformer, rewriter: Rewriter, v: Ref[A]): Transformer = {
      val newVar = variable(Lazy(mirrorElem(v)))
      t + (v, newVar)
    }

    protected def mirrorDef[A](t: Transformer, rewriter: Rewriter, node: Ref[A], d: Def[A]): Transformer = {
      val res = apply(t, rewriter, node, d)
      t + (node, res)
    }

    protected def getMirroredLambdaSym[A, B](node: Ref[A => B]): Sym = placeholder(Lazy(mirrorElem(node)))

    // require: should be called after oldlam.schedule is mirrored
    private def getMirroredLambdaDef(t: Transformer, oldLam: Lambda[_,_], newRoot: Sym): Lambda[_,_] = {
      val newVar = t(oldLam.x)
      val newLambdaDef = new Lambda(Nullable.None, newVar, newRoot, oldLam.mayInline, oldLam.alphaEquality)
      newLambdaDef
    }

    protected def mirrorLambda[A, B](t: Transformer, rewriter: Rewriter, node: Ref[A => B], lam: Lambda[A, B]): Transformer = {
      var tRes: Transformer = t
      val t1 = mirrorNode(t, rewriter, lam, lam.x)

      // original root
      val originalRoot = lam.y

      // ySym will be assigned after f is executed
      val ySym = placeholder(Lazy(lam.y.elem))
      val newLambdaCandidate = getMirroredLambdaDef(t1, lam, ySym)
      val newLambdaSym = newLambdaCandidate.self

      // new effects may appear during body mirroring
      // thus we need to forget original Reify node and create a new one
      val oldStack = lambdaStack
      try {
        lambdaStack = newLambdaCandidate :: lambdaStack
        val newRoot = { // reifyEffects block
          val schedule = lam.scheduleIds
          val t2 = mirrorSymbols(t1, rewriter, lam, schedule)
          tRes = t2
          tRes(originalRoot) // this will be a new root
        }
        ySym.assignDefFrom(newRoot)
      }
      finally {
        lambdaStack = oldStack
      }

      // we don't use toExp here to avoid rewriting pass for new Lambda
      val resLam = findOrCreateDefinition(newLambdaCandidate, newLambdaSym)

// TODO metadata is not processed (for performance, since we don't need it yet)
//      val (tRes2, mirroredMetadata) = mirrorMetadata(tRes, node, newLambdaExp)
//      val resLam = rewriteUntilFixPoint(newLambdaExp, mirroredMetadata, rewriter)

      tRes + (node, resLam)
    }

    protected def mirrorThunk[A](t: Transformer, rewriter: Rewriter, node: Ref[Thunk[A]], thunk: ThunkDef[A]): Transformer = {
      var scheduleIdsPH: ScheduleIds = null
      val newRootPH = placeholder(Lazy(node.elem.eItem))
      val newThunk = new ThunkDef(newRootPH, { assert(scheduleIdsPH != null); scheduleIdsPH })
      val newThunkSym = newThunk.self

      val newScope = thunkStack.beginScope(newThunkSym)
      val schedule = thunk.scheduleIds
      val t1 = mirrorSymbols(t, rewriter, thunk, schedule)
      thunkStack.endScope()

      val newRoot = t1(thunk.root)
      newRootPH.assignDefFrom(newRoot)
      scheduleIdsPH =
          if (newRoot.isVar) DBuffer.ofSize(0)
          else if (newScope.isEmptyBody) DBuffer.ofSize(0)
          else newScope.scheduleForResult(newRoot)

      createDefinition(thunkStack.top, newThunkSym, newThunk)
      t1 + (node, newThunkSym)
    }

    protected def isMirrored(t: Transformer, node: Sym): Boolean = t.isDefinedAt(node)

    def mirrorNode(t: Transformer, rewriter: Rewriter, g: AstGraph, node: Sym): Transformer = {
      if (isMirrored(t, node)) t
      else {
        node.node match {
          case v: Variable[_] =>
            mirrorVar(t, rewriter, node)
          case lam: Lambda[a, b] =>
            mirrorLambda(t, rewriter, node.asInstanceOf[Ref[a => b]], lam)
          case th: ThunkDef[a] =>
            mirrorThunk(t, rewriter, node.asInstanceOf[Ref[Thunk[a]]], th)
          case d =>
            mirrorDef(t, rewriter, node, d)
        }
      }
    }

    /** @hotspot */
    def mirrorSymbols(t0: Transformer, rewriter: Rewriter, g: AstGraph, nodes: DBuffer[Int]) = {
      var t: Transformer = t0
      cfor(0)(_ < nodes.length, _ + 1) { i =>
        val n = nodes(i)
        val s = getSym(n)
        t = mirrorNode(t, rewriter, g, s)
      }
      t
    }
  }

  val DefaultMirror = new Mirror {}

}

