package scalan

trait TestLibrary extends Library {
  import CollBuilder._
  import CCostedBuilder._
  import CostedBuilder._
  import MonoidBuilder._

  lazy val colBuilder: Rep[CollBuilder] = variable[CollBuilder]
  lazy val costedBuilder: Rep[CostedBuilder] = RCCostedBuilder()
  lazy val intPlusMonoid: Rep[Monoid[Int]] = costedBuilder.monoidBuilder.intPlusMonoid
  lazy val longPlusMonoid: Rep[Monoid[Long]] = costedBuilder.monoidBuilder.longPlusMonoid
}
