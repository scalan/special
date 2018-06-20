package scalan.meta

import java.io.File

import com.trueaccord.lenses.Updatable

import scalan.util.{FileUtil, GraphUtil}
import scalan.util.StringUtil._
import scala.collection.mutable.{Map => MMap}

trait Conf {
  /** Module name, which is also name of the directory */
  def name: String

  /** Directory prefix relative to project root.
    * Allows to put modules in sub-directories of the project. */
  def baseDir: String
}

class ConfMap[C <: Conf] private(val table: MMap[String, C]) extends (String => C) {
  private def this() = this(MMap())

  def add(conf: C): this.type = {
    table += (conf.name -> conf)
    return this
  }

  def keySet = table.keySet

  def values = table.values

  def contains(name: String) = table.contains(name)

  def apply(name: String): C = table.getOrElse(name, sys.error(s"Cannot find config $name in collection $table"))

  def get(name: String) = table.get(name)

  def find(p: C => Boolean): Option[C] = table.find { case (_, c) => p(c) }.map(_._2)
}

object ConfMap {
  def apply[C <: Conf](): ConfMap[C] = new ConfMap[C]()

  def apply[C <: Conf](cs: C*): ConfMap[C] = {
    val res = ConfMap[C]()
    cs.foldLeft(res) { (res, c) => res.add(c) }
  }
}

abstract class ModuleConf extends Conf {
  def units: ConfMap[UnitConfig]

  def dependencies: ConfMap[ModuleConf]

  def dependsOnModules(): Set[ModuleConf] =
    GraphUtil.depthFirstSetFrom(Set(this.dependencies.values.toSeq: _*))(m => m.dependencies.values)

  def getSourcesRootDir = s"${baseDir.opt(_ + "/")}$name/${ModuleConf.SourcesDir }"

  def getResourcesRootDir = s"${baseDir.opt(_ + "/")}$name/${ModuleConf.ResourcesDir }"

  def getWrappersRootDir = getResourcesRootDir + "/wrappers"

  private def unitConfigTemplate(baseDir: String, name: String, entityFile: String) =
    UnitConfig(
      baseDir = baseDir,
      name = name, srcPath = "", resourcePath = "", entityFile = entityFile,
      baseContextTrait = "scalan.Scalan", // used like this: trait ${module.name}Defs extends ${config.baseContextTrait.opt(t => s"$t with ")}${module.name} {
      extraImports = List(
        "scala.reflect.runtime.universe._",
        "scala.reflect._"
      ),
      isVirtualized = false
    )

  def mkUnit(unitName: String, unitFile: String, isVirtualized: Boolean, definesWrappers: Boolean = false) =
    unitConfigTemplate(baseDir, unitName, unitFile).copy(
      srcPath = s"${baseDir.opt(_ + "/")}$name/${ModuleConf.SourcesDir }",
      resourcePath = s"${baseDir.opt(_ + "/")}$name/${ModuleConf.ResourcesDir }",
      isVirtualized = isVirtualized
    )
}

object ModuleConf {
  val SourcesDir = "src/main/scala"
  val TestsDir = "src/test/scala"
  val ResourcesDir = "src/main/resources"
  val ResourceFileExtension = ".scalan"
}

class TargetModuleConf(
    val baseDir: String,
    val name: String,
    val sourceModules: ConfMap[SourceModuleConf]
) extends ModuleConf {
  override def units: ConfMap[UnitConfig] = ConfMap()

  override def dependencies: ConfMap[ModuleConf] = ConfMap(sourceModules.values.map(v => v: ModuleConf).toSeq: _*)
}

class SourceModuleConf(
    val baseDir: String,
    val name: String,
    val units: ConfMap[UnitConfig] = ConfMap(),
    val deps: ConfMap[SourceModuleConf] = ConfMap(),
    val libraryDeps: ConfMap[LibraryConfig] = ConfMap(),
    val moduleDeps: ConfMap[ModuleConf] = ConfMap()
) extends ModuleConf {
  override def dependencies: ConfMap[ModuleConf] = deps.asInstanceOf[ConfMap[ModuleConf]]

  def hasUnit(unitName: String) = units.contains(unitName)

  def addUnit(unitName: String, unitFile: String, definesWrappers: Boolean = false): this.type = {
    units.add(mkUnit(unitName, unitFile, isVirtualized = false, definesWrappers))
    this
  }

  def dependsOn(us: SourceModuleConf*): this.type = {
    for ( u <- us ) deps.add(u)
    this
  }

  def libraryDependencies(lds: LibraryConfig*): this.type = {
    for ( ld <- lds ) libraryDeps.add(ld)
    this
  }

  def moduleDependencies(mds: ModuleConf*): this.type = {
    for ( md <- mds ) moduleDeps.add(md)
    this
  }

  def listWrapperFiles: Array[File] = {
    import FileUtil._
    listFilesRecursive(file(getWrappersRootDir))
  }
}

case class UnitConfig(
    baseDir: String,  // path relative to the project root dir
    name: String,
    srcPath: String, // the base path to where root package is located (example: <module>/<ModuleConf.SourceDir>)
    resourcePath: String, // example: <module>/<ModuleConf.ResourcesDir>
    entityFile: String, // the package path and file name (example: scalan/collection/Col.scala)
    baseContextTrait: String = "scalan.Scalan",
    extraImports: List[String] = List("scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}", "scalan.meta.ScalanAst._"),
    isVirtualized: Boolean = true) extends Conf with Updatable[UnitConfig] {
  val entityResource = entityFile.replaceSuffix(".scala", ModuleConf.ResourceFileExtension)
  def getFile: File = FileUtil.file(srcPath, entityFile)
  def getResourceFile: File = {
    FileUtil.file(resourcePath, entityResource)
  }
  def packageName: String = entityFile.stripSuffix("/" + entityFile).replace('/', '.')
  def unitName: String = name.stripSuffix(".scala")
  @inline def unitKey: String = SSymName.fullNameString(packageName, unitName)
}

object UnitConfig {
  def getImplFile(file: File, suffix: String, extension: String) = {
    val fileName = file.getName.split('.')(0)
    val folder = file.getParentFile
    val implFile = FileUtil.file(folder, "impl", s"$fileName$suffix.$extension")
    implFile
  }
}

/** <c>classesPath</c> is a path to JAR or directory containing scalanizer-plugin.properties and a plugin for Scalanizer which implements [scalan.meta.scalanizer.Plugin]. */
case class PluginConfig(classesPath: String, extraData: Any = null)

