package special.wrappers

import wrappers.scala.{WArraysModule, WOptionsModule}
import wrappers.scala.util.WEithersModule
import wrappers.scalan.WRTypesModule
import wrappers.special.WSpecialPredefsModule


trait WrappersModule
  extends WSpecialPredefsModule
  with WArraysModule
  with WOptionsModule
  with WEithersModule
  with WRTypesModule