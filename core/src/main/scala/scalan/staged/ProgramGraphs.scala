package scalan.staged

import scalan.{Nullable, Scalan, DFunc}
import debox.{Buffer => DBuffer}
import spire.syntax.all.cfor

trait ProgramGraphs extends AstGraphs { self: Scalan =>

  type PGraph = ProgramGraph

  // immutable program graph
  case class ProgramGraph(roots: Seq[Sym], mapping: Nullable[Transformer], filterNode: Nullable[Sym => Boolean])
  	  extends AstGraph {
    def this(roots: Seq[Sym], filterNode: Nullable[Sym => Boolean] = Nullable.None) { this(roots, Nullable.None, filterNode) }
    def this(root: Sym) { this(List(root)) }

    override lazy val rootIds: DBuffer[Int] = super.rootIds

    override def boundVars = Nil
    override def freeVars = Set()
    override lazy val scheduleIds = {
      val neighbours: DFunc[Int, DBuffer[Int]] = filterNode match {
        case Nullable(pred) =>
          { (id: Int) =>
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (pred(sym) && !sym.isVar)
                res += sym.node.nodeId
            }
            res
          }
        case _ =>
          { (id: Int) =>
            val deps = getSym(id).node.deps
            val len = deps.length
            val res = DBuffer.ofSize[Int](len)
            cfor(0)(_ < len, _ + 1) { i =>
              val sym = deps(i)
              if (!sym.isVar)
                res += sym.node.nodeId
            }
            res
          }
      }
      val sch = buildScheduleForResult(rootIds, neighbours)
      sch
    }

    def transform(m: Mirror, rw: Rewriter, t: Transformer): ProgramGraph = {
      val t0 = mapping match {
        case Nullable(mapping) => t merge mapping
        case _ => t
      }
      val t1 = m.mirrorSymbols(t0, rw, this, scheduleIds)
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, Nullable(t1), filterNode)
    }

    def transformOne(oldExp: Sym, newExp: Sym): ProgramGraph = {
      val newRoots = roots map (x => x match {case v: Sym if v == oldExp => newExp; case t => t }  )
      new ProgramGraph(newRoots, mapping, filterNode)
    }

    def withoutContext = ProgramGraph(roots, Nullable.None, filterNode)

    override def toString: String = {
      val mappingStr = if (mapping.isEmpty) "None" else mapping.toString
      val filterNodeStr = if (filterNode.isDefined) filterNode.toString else "None"
      s"ProgramGraph($roots, $mappingStr, $filterNodeStr)"
    }
  }

  object ProgramGraph {
    def transform[A](s: Ref[A], rw: Rewriter = NoRewriting, t: MapTransformer = MapTransformer.empty()): Ref[A] = {
      val g = ProgramGraph(List(s), Nullable.None, Nullable.None)
      val g1 = g.transform(DefaultMirror, rw, t)
      g1.roots(0).asInstanceOf[Ref[A]]
    }
  }
}
