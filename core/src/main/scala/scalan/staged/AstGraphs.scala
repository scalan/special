package scalan.staged

import scala.collection.{mutable, _}
import scalan.{DFunc, Nullable, Scalan}
import scalan.compilation.GraphVizConfig
import scalan.util.GraphUtil
import spire.syntax.all.cfor
import debox.{Set => DSet, Buffer => DBuffer, Map => DMap}

import scala.annotation.tailrec

trait AstGraphs extends Transforming { self: Scalan =>

  /**
   * GraphNode is created for each symbol of the AstGraph and represents graph linking structure
   */
  case class GraphNode(
          sym: Sym, // this symbol
          usages: DBuffer[Int]) {
    def inputSyms: Seq[Sym] = sym.rhs.deps
    def outSyms: DBuffer[Sym] = {
      usages.map(getSym)
    }
  }

  type Schedule = Seq[Sym]

  trait AstGraph { thisGraph =>
    def boundVars: Seq[Sym]
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

    def schedule: Schedule

    lazy val scheduleSyms: Seq[Int] = {
      val len = schedule.length
      val res = new Array[Int](len)
      cfor(0)(_ < len, _ + 1) { i =>
        res(i) = schedule(i).rhs.nodeId
      }
      res
    }

    /** Set of scheduleSyms */
    lazy val domain: DSet[Int] = {
      val res = DSet.ofSize[Int](scheduleSyms.length)
      res ++= scheduleSyms.toArray
      res
    }

    @inline final def isIdentity: Boolean = boundVars == roots
    @inline def isBoundVar(s: Sym) = boundVars.contains(s)

    @inline def isLocalDef(s: Sym): Boolean = domain(s.rhs.nodeId)

    @inline def isRoot(s: Sym): Boolean = roots contains s

    /** Flatten the given schedule into single sequence of non-AstGraph definitions.
      * All scope forming definitions like Lambda and ThunkDef are recursively unfolded in the given buffer `flatBuf`.
      * NOTE: The symbols of AstGraph-like definitions are added to `flatBuf` AFTER the unfolded body.
      */
    final def buildFlatSchedule(schedule: Schedule, flatBuf: DBuffer[Sym]): Unit = {
      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        if (sym.rhs.isInstanceOf[AstGraph]) {
          buildFlatSchedule(sym.rhs.asInstanceOf[AstGraph].schedule, flatBuf)
        }
        flatBuf += sym
      }
    }

    lazy val flatSchedule: Schedule = {
      val flatBuf = DBuffer.ofSize[Sym](schedule.length)
      buildFlatSchedule(schedule, flatBuf)
      flatBuf.toArray
    }

    def buildUsageMap(schedule: Schedule, usingDeps: Boolean): DMap[Int, GraphNode] = {
      val len = schedule.length
      val nodeMap = DMap.ofSize[Int, GraphNode](len)
      cfor(0)(_ < len, _ + 1) { i =>
        val sym = schedule(i)
        val symId = sym.rhs.nodeId
        nodeMap.update(symId, GraphNode(sym, DBuffer.empty[Int]))

        val deps = if (usingDeps) sym.rhs.deps else sym.rhs.syms
        cfor(0)(_ < deps.length, _ + 1) { j =>
          val us = deps(j)             // used symbol
          val usId = us.rhs.nodeId     // used symbol id
          var node = nodeMap.getOrElse(usId, null)
          if (null == node) {
            node = GraphNode(us, DBuffer.empty[Int])
            nodeMap.update(usId, node)
          }
          node.usages += symId
        }

      }
      nodeMap
    }

    /**
     * Symbol Usage information for this graph
     */
    lazy val usageMap: DMap[Int, GraphNode] = {
      buildUsageMap(schedule, usingDeps = true)
    }

    lazy val allNodes: DMap[Int, GraphNode] = {
      buildUsageMap(flatSchedule, usingDeps = false) // using rhs.syms instead of rhs.deps
    }

    def globalUsagesOf(s: Sym): DBuffer[Sym] = allNodes.get(s.rhs.nodeId) match {
      case Some(node) => node.outSyms
      case None => DBuffer.empty[Sym]
    }

    def hasManyUsagesGlobal(s: Sym): Boolean = globalUsagesOf(s).length > 1

    def usagesOf(s: Sym): DBuffer[Sym] = usageMap.get(s.rhs.nodeId) match {
      case Some(node) => node.outSyms
      case None => DBuffer.empty[Sym]
    }

    def hasManyUsages(s: Sym): Boolean = usagesOf(s).length > 1

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
