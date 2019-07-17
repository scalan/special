package scalan.primitives

import scalan.{ScalanEx, BaseEx}

trait StructsEx extends Structs with BaseEx { self: ScalanEx =>

  override def unapplyViews[T](s: Rep[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewStruct[a, b]) =>
      Some((view.source, view.iso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

}
