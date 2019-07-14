package scalan.staged

import scalan.{Nullable, Scalan, DFunc}

trait ProgramGraphs extends AstGraphs { self: Scalan =>

  type PGraph = ProgramGraph[MapTransformer]

  // immutable program graph
  case class ProgramGraph[Ctx <: Transformer : TransformerOps](roots: Seq[Sym], mapping: Ctx, filterNode: Nullable[Sym => Boolean])
  	  extends AstGraph {
    def this(roots: Seq[Sym], filterNode: Nullable[Sym => Boolean] = Nullable.None) { this(roots, implicitly[TransformerOps[Ctx]].empty, filterNode) }
    def this(root: Sym) { this(List(root)) }

    override def boundVars = Nil
    override def freeVars = Set()
    override lazy val schedule = {
      val neighbours: DFunc[Int, Array[Int]] = filterNode match {
        case Nullable(pred) =>
          (id: Int) => getSym(id).rhs.deps.collect { case sym if pred(sym) && !sym.isVar => sym.rhs.nodeId }.toArray
        case _ =>
          (id: Int) => getSym(id).rhs.deps.collect { case sym if !sym.isVar => sym.rhs.nodeId }.toArray
      }
      buildScheduleForResult(roots.map(_.rhs.nodeId).toArray, neighbours).map(getSym(_))
    }


    def transform(m: Mirror[Ctx], rw: Rewriter, t: Ctx): ProgramGraph[Ctx] = {
      val t0 = t merge mapping
      val (t1, _) = m.mirrorSymbols(t0, rw, this, scheduleSyms)
      val newRoots = roots map { t1(_) }
      new ProgramGraph(newRoots, t1, filterNode)
    }

    def transformOne(oldExp:Sym, newExp:Sym): ProgramGraph[Ctx] = {
      val newRoots = roots map (x => x match {case v: Sym if v == oldExp => newExp; case t => t }  )
      new ProgramGraph(newRoots, mapping, filterNode)
    }

    def withoutContext = ProgramGraph(roots, implicitly[TransformerOps[Ctx]].empty, filterNode)

    override def toString: String = s"ProgramGraph($roots, $mapping, ${if(filterNode.isDefined) filterNode.toString else "None"})"
  }

  object ProgramGraph {
    def transform[A](s: Rep[A], rw: Rewriter = NoRewriting, t: MapTransformer = MapTransformer.Empty): Rep[A] = {
      val g = ProgramGraph(List(s), t, Nullable.None)
      val g1 = g.transform(DefaultMirror, rw, t)
      g1.roots(0).asInstanceOf[Rep[A]]
    }
  }
}
