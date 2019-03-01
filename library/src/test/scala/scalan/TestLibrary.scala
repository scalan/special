package scalan

trait TestLibrary extends Library {
  import CollOverArrayBuilder._
  import CCostedBuilder._
  import CostedBuilder._
  import MonoidBuilder._

  lazy val colBuilder: Rep[CollBuilder] = RCollOverArrayBuilder()
  lazy val costedBuilder: Rep[CostedBuilder] = RCCostedBuilder()
  lazy val intPlusMonoid: Rep[Monoid[Int]] = costedBuilder.monoidBuilder.intPlusMonoid
  lazy val longPlusMonoid: Rep[Monoid[Long]] = costedBuilder.monoidBuilder.longPlusMonoid
}
