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
    def inputSyms: Seq[Sym] = sym.node.deps
    def outSyms: DBuffer[Sym] = {
      usages.map(getSym)
    }
  }

  type Schedule = Seq[Sym]
  type ScheduleIds = DBuffer[Int]

  /** Base class for all compound nodes with schedule (e.g. Lambda, ThunkDef) */
  abstract class AstGraph extends Node { thisGraph =>
    def boundVars: Seq[Sym]
    def roots: Seq[Sym]

    def rootIds: DBuffer[Int] = {
      val rs = roots.toArray
      val len = rs.length
      val res = new Array[Int](len)
      cfor(0)(_ < len, _ + 1) { i => res(i) = rs(i).node.nodeId }
      DBuffer.unsafe(res)
    }

    /** @hotspot */
    def freeVars: Set[Sym] = {
      val res = mutable.HashSet.empty[Sym]
      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        val deps = sym.node.deps
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

    def scheduleIds: DBuffer[Int]

    lazy val schedule: Schedule = {
      val len = scheduleIds.length
      val res = new Array[Sym](len)
      cfor(0)(_ < len, _ + 1) { i =>
        res(i) = getSym(scheduleIds(i))
      }
      res
    }


    /** Set of scheduleSyms */
    lazy val domain: DSet[Int] = {
      val res = DSet.ofSize[Int](scheduleIds.length)
      res ++= scheduleIds.toArray
      res
    }

    @inline final def isIdentity: Boolean = boundVars == roots
    @inline def isBoundVar(s: Sym) = boundVars.contains(s)

    @inline final def isLocalDef(s: Sym): Boolean = domain(s.node.nodeId)
    @inline final def isLocalDefId(id: Int): Boolean = domain(id)

    @inline final def isRoot(s: Sym): Boolean = roots.contains(s)

    /** Flatten the given schedule into single sequence of non-AstGraph definitions.
      * All scope forming definitions like Lambda and ThunkDef are recursively unfolded in the given buffer `flatBuf`.
      * NOTE: The symbols of AstGraph-like definitions are added to `flatBuf` AFTER the unfolded body.
      */
    final def buildFlatSchedule(schedule: Schedule, flatBuf: DBuffer[Sym]): Unit = {
      cfor(0)(_ < schedule.length, _ + 1) { i =>
        val sym = schedule(i)
        if (sym.node.isInstanceOf[AstGraph]) {
          buildFlatSchedule(sym.node.asInstanceOf[AstGraph].schedule, flatBuf)
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
        val symId = sym.node.nodeId
        nodeMap.update(symId, GraphNode(sym, DBuffer.empty[Int]))

        val deps = if (usingDeps) sym.node.deps else sym.node.syms
        cfor(0)(_ < deps.length, _ + 1) { j =>
          val us = deps(j)             // used symbol
          val usId = us.node.nodeId     // used symbol id
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

    def globalUsagesOf(s: Sym): DBuffer[Sym] = allNodes.get(s.node.nodeId) match {
      case Some(node) => node.outSyms
      case None => DBuffer.empty[Sym]
    }

    def hasManyUsagesGlobal(s: Sym): Boolean = globalUsagesOf(s).length > 1

    def usagesOf(id: Int): DBuffer[Int] = {
      val node = usageMap.getOrElse(id, null)
      if (node == null) return DBuffer.empty[Int]
      node.usages
    }

    def hasManyUsages(s: Sym): Boolean = usagesOf(s.node.nodeId).length > 1

    def show(): Unit = show(defaultGraphVizConfig)
    def show(emitMetadata: Boolean): Unit = show(defaultGraphVizConfig.copy(emitMetadata = emitMetadata))
    def show(config: GraphVizConfig): Unit = showGraphs(this)(config)

  } // AstGraph


  def buildScheduleForResult(startNodes: DBuffer[Int], neighbours: DFunc[Int, DBuffer[Int]]): DBuffer[Int] = {
    val components = GraphUtil.stronglyConnectedComponents(startNodes, neighbours)
    val nComponents = components.length
    if (nComponents == 1) {
      components(0)
    } else {
      val res = DBuffer.ofSize[Int](components(0).length)
      cfor(0)(_ < nComponents, _ + 1) { i =>
        res ++= components(i)
      }
      res
    }
  }
}
