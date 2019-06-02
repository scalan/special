package scalan

import scalan.meta.SSymName

case class ModuleInfo(packageName: String, moduleName: String, extension: String = ".scalan") {
  val name = SSymName(packageName, moduleName)
  def getKey = name.mkFullName
  def sourceFileName = packageName.replace('.', '/') + s"/$moduleName$extension"
}
