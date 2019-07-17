package scalan

import scalan.primitives.TypeSumEx

trait ConvertersModuleEx extends ConvertersModule with impl.ConvertersDefsEx with TypeSumEx { self: ScalanEx =>
  import IsoUR._

  override def rewriteViews[T](d: Def[T]) = d match {
    case Convert(eFrom: Elem[from], eTo: Elem[to], HasViews(_x, _iso: Iso[Def[_], _] @unchecked),  _conv) =>
      val iso = _iso.asInstanceOf[Iso[Def[_], from]]
      val conv = asRep[from => to](_conv)
      val x = asRep[Def[_]](_x)
      tryConvert(x.elem, eTo, x, iso.toFun >> conv)

    case _ => super.rewriteViews(d)
  }

}
