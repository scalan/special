package scalan.util

import scalan.{DFunc, DFuncAdapter}
import debox.{Buffer => DBuffer}

import scala.reflect.ClassTag
import spire.syntax.all.cfor

object GraphUtilEx {
  /**
    * Returns the strongly connected components
    * of the graph rooted at the startNodes argument,
    * whose edges are given by the function argument `succ`.
    *
    * @see Tarjan, Robert E.. “Depth-First Search and Linear Graph Algorithms.” SIAM J. Comput. 1 (1972): 146-160.
    */
  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](startNodes: DBuffer[T], succ: DFunc[T, DBuffer[T]]): DBuffer[DBuffer[T]] = {
    val tarjan = new Tarjan[T](succ)

    // top level loop of the algorithm
    cfor(0)(_ < startNodes.length, _ + 1) { i =>
      val node = startNodes(i)
      tarjan.visit(node)
    }

    tarjan.res
  }

  def stronglyConnectedComponents[@specialized(Int) T: ClassTag](start: Array[T])(succ: T => DBuffer[T]): DBuffer[DBuffer[T]] = {
    stronglyConnectedComponents(DBuffer.fromArray(start), new DFuncAdapter(succ))
  }

  def buildScheduleForResult(startNodes: DBuffer[Int], neighbours: DFunc[Int, DBuffer[Int]]): DBuffer[Int] = {
    val components = GraphUtilEx.stronglyConnectedComponents(startNodes, neighbours)
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

  val initTarjan = new Tarjan[Int](new DFuncAdapter((i: Int) => DBuffer(i)))
}

