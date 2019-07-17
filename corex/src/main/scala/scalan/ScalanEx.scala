package scalan

import scalan.primitives._
import scalan.staged.{TransformingEx}

trait BaseEx extends Base { self: ScalanEx =>
  def decompose[T](d: Def[T]): Option[Rep[T]] = None

  val performViewsLifting: Boolean = true

  def rewriteViews[T](d: Def[T]): Rep[_] = null

  def unapplyViews[T](s: Rep[T]): Option[Unpacked[T]] = None
}

class ScalanEx extends Scalan
  with Blocks
  with StringOps
  with Metadata
  with RewriteRules
  with BaseEx
  with TransformingEx
  with ViewsModuleEx
  with ThunksEx
  with StructsEx
  with ConvertersModuleEx
{

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
