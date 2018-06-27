package scala.wrappers {
  import scalan._
  import scalan.WSpecialPredefsModule
  import scala.WArraysModule
  import scala.util.WEithersModule

  trait WrappersModule
    extends WSpecialPredefsModule
    with WArraysModule
    with WOptionsModule
    with WEithersModule

}