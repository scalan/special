package scalan.staged

import scala.collection.{mutable, _}
import scalan.{Nullable, Scalan, DFunc}
import scalan.compilation.GraphVizConfig
import scalan.util.GraphUtil
import spire.syntax.all.cfor
import debox.{Set => DSet, Buffer => DBuffer}

trait AstGraphs extends Transforming { self: Scalan =>

  /**
   * GraphNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  case class GraphNode(
          graph: AstGraph,
          sym: Sym, // this symbol
          definition: Nullable[Def[_]], // definition
          usages: List[Sym]) {
    def inputSyms: Seq[Sym] = if (definition.isDefined) definition.get.deps else Nil
    def outSyms: List[Sym] = usages
    def addUsage(usage: Sym): GraphNode = copy(usages = usage :: this.usages)
  }

  type Schedule = Seq[Sym]

  trait AstGraph { thisGraph =>
    def boundVars: List[Sym]
    def roots: Seq[Sym]

    /** @hotspot */
    def freeVars: Set[Sym] = {
      val res = mutable.HashSet.empty[Sym]
      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        val deps = sym.rhs.deps
        cfor(0)(_ < deps.length, _ + 1) { j =>
          val s = deps(j)
          if (!res.contains(s)) {
            if (!(isLocalDef(s) || isBoundVar(s))) {
              res += s
            }
          }
        }
      }
      res
    }

    def getRootsIfEmpty(sch: Schedule) =
      if (sch.isEmpty) {
        roots // the case when body is consists of consts
      }
      else sch

    def schedule: Schedule

    lazy val scheduleSyms: Seq[Int] = {
      val len = schedule.length
      val res = new Array[Int](len)
      cfor(0)(_ < len, _ + 1) { i =>
        res(i) = schedule(i).rhs.nodeId
      }
      res
    }

    @inline def isIdentity: Boolean = boundVars == roots
    @inline def isBoundVar(s: Sym) = boundVars.contains(s)

    // TODO use DSet for scheduleSyms
    @inline def isLocalDef(s: Sym): Boolean = scheduleSyms contains (s.rhs.nodeId)

    @inline def isRoot(s: Sym): Boolean = roots contains s

    lazy val scheduleAll: Schedule =
      schedule.flatMap {
        case sym if sym.rhs.isInstanceOf[AstGraph] =>
          sym.rhs.asInstanceOf[AstGraph].scheduleAll :+ sym
        case tp =>
          Array(tp)
      }

    /**
     * Symbol Usage information for this graph
     */
    lazy val nodes: Map[Sym, GraphNode] = {
      var defMap: Map[Sym, GraphNode] = (schedule.map { sym =>
        (sym, GraphNode(this, sym, Nullable(sym.rhs), Nil))
      }).toMap

      def addUsage(usedSym: Sym, referencingSym: Sym) = {
        val newNode = defMap.getOrElse(usedSym, GraphNode(this, usedSym, Nullable.None, Nil)).addUsage(referencingSym)
        defMap += usedSym -> newNode
      }


      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        val usedSymbols = sym.rhs.deps
        cfor(0)(_ < usedSymbols.length, _ + 1) { j =>
          val us = usedSymbols(j)
          addUsage(us, sym)
        }
      }
      defMap
    }

    lazy val allNodes: Map[Sym, GraphNode] = {
      var defMap: Map[Sym, GraphNode] = (scheduleAll.map { sym =>
        (sym, GraphNode(this, sym, Nullable(sym.rhs), List.empty[Sym]))
      }).toMap

      def addUsage(usedSym: Sym, referencingSym: Sym) = {
        val newNode = defMap.getOrElse(usedSym, GraphNode(this, usedSym, Nullable.None, List.empty)).addUsage(referencingSym)
        defMap += usedSym -> newNode
      }

      for (sym <- scheduleAll) {
        val usedSymbols = syms(sym.rhs)
        usedSymbols.foreach(us => addUsage(us, sym))
      }
      defMap
    }

    lazy val domain: Set[Sym] = scheduleSyms.map(getSym).toSet

    def node(s: Sym): Option[GraphNode] = nodes.get(s)

    def globalUsagesOf(s: Sym) = allNodes.get(s) match {
      case Some(node) => node.outSyms
      case None => Nil
    }

    def hasManyUsagesGlobal(s: Sym): Boolean = globalUsagesOf(s).lengthCompare(1) > 0

    def usagesOf(s: Sym) = node(s) match {
      case Some(node) => node.outSyms
      case None => Nil
    }

    def hasManyUsages(s: Sym): Boolean = usagesOf(s).lengthCompare(1) > 0

    def show(): Unit = show(defaultGraphVizConfig)
    def show(emitMetadata: Boolean): Unit = show(defaultGraphVizConfig.copy(emitMetadata = emitMetadata))
    def show(config: GraphVizConfig): Unit = showGraphs(this)(config)

  } // AstGraph


  def buildScheduleForResult(startNodes: Array[Int], neighbours: DFunc[Int, Array[Int]]): Array[Int] = {
    val components = GraphUtil.stronglyConnectedComponents(startNodes, neighbours)
    val nComponents = components.length
    if (nComponents == 1) {
      components(0).toArray()
    } else {
      val res = DBuffer.ofSize[Int](components(0).length)
      cfor(0)(_ < nComponents, _ + 1) { i =>
        res ++= components(i)
      }
      res.toArray()
    }
  }
}
