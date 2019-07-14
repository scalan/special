package scalan.staged

import scala.collection.{mutable, _}
import scala.collection.mutable.ArrayBuffer
import scalan.{Nullable, Scalan, DFunc}
import scalan.compilation.GraphVizConfig
import scalan.util.GraphUtil
import spire.syntax.all.cfor
import debox.{Set => DSet, Buffer => DBuffer}

trait AstGraphs extends Transforming { self: Scalan =>

  /**
   * AstNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  abstract class AstNode(val graph: AstGraph) {
    def sym: Sym
    def inputSyms: List[Sym]
    def outSyms: List[Sym]
  }

  case class GraphNode(
          override val graph: AstGraph,
          sym: Sym, // this symbol
          definition: Nullable[Def[_]], // definition
          usages: List[Sym]) extends AstNode(graph) {
    def inputSyms: List[Sym] = definition.toList.flatMap(_.getDeps)
    def outSyms = usages
    def addUsage(usage: Sym) = copy(usages = usage :: this.usages)
  }

  type Schedule = Seq[Sym]

  trait AstGraph { thisGraph =>
    def boundVars: List[Sym]
    def roots: Seq[Sym]

    /** @hotspot */
    def freeVars: Set[Sym] = {
      val res = mutable.HashSet.empty[Sym]
      schedule.foreach { tp =>
        for (s <- getDeps(tp.rhs)) {
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
     * also contains lambda vars with definition = None
     */
    lazy val nodes: Map[Sym, GraphNode] = {
      var defMap: Map[Sym, GraphNode] = (schedule.map { sym =>
        (sym, GraphNode(this, sym, Nullable(sym.rhs), Nil))
      }).toMap

      def addUsage(usedSym: Sym, referencingSym: Sym) = {
        val newNode = defMap.getOrElse(usedSym, GraphNode(this, usedSym, Nullable.None, Nil)).addUsage(referencingSym)
        defMap += usedSym -> newNode
      }

      for (sym <- schedule) {
        val usedSymbols = sym.rhs.getDeps
        usedSymbols.foreach(us => addUsage(us, sym))
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

    def node(s: Sym): Option[AstNode] = nodes.get(s)

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

    /** Builds a schedule starting from symbol `sym`  which consists only of local definitions.
      *  @param syms   the roots of the schedule, it can be non-local itself
      *  @param deps  dependence relation between a definition and symbols
      *  @return      a `Seq` of local definitions on which `sym` depends or empty if `sym` is itself non-local
      */
    def buildLocalScheduleFrom(syms: Seq[Sym], deps: Sym => List[Sym]): Schedule =
      for {
        s <- syms if isLocalDef(s)
        // TODO ensure deps return DBuffer
        tp <- buildScheduleForResult(Array(s.rhs.nodeId),
               id => deps(getSym(id)).collect { case dsym if isLocalDef(dsym) && !dsym.isVar => dsym.rhs.nodeId }.toArray)
      }
      yield getSym(tp)

    def buildLocalScheduleFrom(sym: Sym, deps: Sym => List[Sym]): Schedule =
      buildLocalScheduleFrom(List(sym), deps)

    def buildLocalScheduleFrom(sym: Sym): Schedule = buildLocalScheduleFrom(sym, (_: Sym).getDeps)

    def show(): Unit = show(defaultGraphVizConfig)
    def show(emitMetadata: Boolean): Unit = show(defaultGraphVizConfig.copy(emitMetadata = emitMetadata))
    def show(config: GraphVizConfig): Unit = showGraphs(this)(config)
  }

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
