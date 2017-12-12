package scalan

import scalan.meta.SName

case class ModuleInfo(packageName: String, moduleName: String, extension: String = ".scalan") {
  val name = SName(packageName, moduleName)
  def getKey = name.mkFullName
  def sourceFileName = packageName.replace('.', '/') + s"/$moduleName$extension"
}

