package scalan.util

import scala.collection.mutable.{Buffer, ArrayBuffer}
import scalan.{AVHashMap, DFunc, Nullable, DFuncAdapter}
import debox.{Set => DSet, Buffer => DBuffer}

import scala.reflect.ClassTag

trait NeighbourFunc[@specialized(Int) A] {
  def get(x: A, res: DBuffer[A]): Unit
}
case class Neighbours[A](f: A => TraversableOnce[A]) extends NeighbourFunc[A] {
  override def get(x: A, res: DBuffer[A]): Unit = {
    val ns = f(x)
    ns foreach (res.+=)
  }
}

object GraphUtil {

  def depthFirstSetFrom[@specialized(Int) A: ClassTag](starts: DBuffer[A])(neighbours: NeighbourFunc[A]): DSet[A] = {
    val visited = DSet.ofSize[A](starts.length)

    def visit(s: A): Unit = {
      if (!(visited(s))) {
        visited += s
        val ns = DBuffer.ofSize[A](16)
        neighbours.get(s, ns)
        ns foreach visit
      }
    }

   starts foreach visit
   visited
 }

  /**
   * Returns the strongly connected components
   * of the graph rooted at the first argument,
   * whose edges are given by the function argument.
   *
   * The scc are returned in _reverse_ topological order.
   * Tarjan's algorithm (linear).
   */
  def stronglyConnectedComponents[@specialized(Int) T](start: Seq[T], succ: DFunc[T, Seq[T]]): Seq[Seq[T]] = {
    val tarjan = new Tarjan[T](succ)

    for (node <- start)
      tarjan.visit(node)

    tarjan.res
  }

  def stronglyConnectedComponents[@specialized(Int) T](start: Seq[T])(succ: T => Seq[T]): Seq[Seq[T]] = {
    stronglyConnectedComponents(start, new DFuncAdapter(succ))
  }

  private final class IntRef(init: Int) {
    var value: Int = init
  }

}


final class Tarjan[@specialized(Int) T](neighbours: DFunc[T, Seq[T]]) {
  private var id = 0
  private var stack: List[T] = Nil
  private val mark = AVHashMap[T,Int](127)

  val res: Buffer[Seq[T]] = new ArrayBuffer()

  def visit(node: T): Int = {
    mark.get(node) match {
      case Nullable(n) => n
      case _ =>
        id += 1

        mark.put(node, id)
        stack = node :: stack
        //    println("push " + node)

        var min: Int = id
        for (child <- neighbours(node)) {
          val m = visit(child)

          if (m < min)
            min = m
        }

        if (min == mark(node)) {
          val scc: Buffer[T] = new ArrayBuffer()

          var loop: Boolean = true
          do {
            val element = stack.head
            stack = stack.tail
            //        println("appending " + element)
            scc.append(element)
            mark.put(element, Integer.MAX_VALUE)
            loop = element != node
          } while (loop)

          res.append(scc.toSeq)
        }
        min
    }
  }
}
