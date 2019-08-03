package scalan

trait Modules extends Base { self: Scalan =>
  def okRegisterModules: Boolean = false

  def registerModule(moduleInfo: ModuleInfo) = {
    if (okRegisterModules) {
      !!!(s"Cannot register module $moduleInfo: registerModule method is not overridden in IR cake $this. ")
    }
  }

}
