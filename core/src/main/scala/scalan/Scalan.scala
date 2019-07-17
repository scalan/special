package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.staged.{Transforming, TransformingEx}

class Scalan
  extends Base
  with TypeDescs
  with Proxy
  with Tuples
  with NumericOps
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with MathOps
  with Equal
  with UniversalOps
  with Functions
  with IfThenElse
  with Transforming
  with GraphVizExport
  with ViewsModule
  with Thunks
  with Structs
  with ConvertersModule
  with Modules

class ScalanEx extends Scalan
  with Blocks
  with StringOps
  with Metadata
  with RewriteRules
  with TransformingEx {

  override def resetContext() = {
    super.resetContext()
    metadataPool = Map.empty[Sym, MetaNode]
  }

  override protected def formatMetadata(s: Sym): List[String] = {
    val metadata = s.allMetadata.meta
    if (metadata.nonEmpty)
      "Metadata:" :: metadata.map { case (k, v) => s"$k:${formatConst(v.value)}" }.toList
    else
      Nil
  }

  protected def rewriteUntilFixPoint[T](start: Rep[T], mn: MetaNode, rw: Rewriter): Rep[T] = {
    var res = start
    var curr: Rep[T] = res
    do {
      curr = res
      setAllMetadata(curr, mn)
      res = rw(curr)
    } while (res != curr)
    res
  }

  override protected[scalan] def toExp[T](d: Def[T], newSym: => Rep[T]): Rep[T] = {
    var res = findOrCreateDefinition(d, newSym)
    var currSym = res
    var currDef = d
    do {
      currSym = res
      val ns = rewrite(currSym).asInstanceOf[Rep[T]]
      ns match {
        case null =>
          currDef = null
        case Def(someOtherD) =>
          res = ns
          currDef = someOtherD
        case _ =>
          res = ns
          currDef = null
      }
    } while (res != currSym && currDef != null)
    res
  }
}
