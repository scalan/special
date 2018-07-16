/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
package scalan.meta

import java.io.File

import com.typesafe.scalalogging.LazyLogging

import scalan.util.FileUtil
import ScalanAst._
import scala.tools.nsc.Global

class Parsers(val configs: List[UnitConfig]) extends ScalanParsersEx[Global] {
  def getGlobal: Global = new Global(settings, reporter)
  implicit val context = new AstContext(configs, this)
  initCompiler()
}

class EntityManagement[+G <: Global](val parsers: ScalanParsers[G]) extends LazyLogging {
  import parsers._
  def configs = parsers.context.unitConfigs.values
  implicit def context = parsers.context
   
  case class EntityManager(name: String, file: File, resourceFile: File, module: SUnitDef, config: UnitConfig)

  protected val entities = (for(conf <- configs) yield {
    val file = conf.getFile
    val resourceFile = conf.getResourceFile
    try {
      val unit = parseUnitFile(file)(new ParseCtx(conf.isVirtualized))
      inform(s"Adding unit parsed from ${file} (relative to ${FileUtil.currentWorkingDir })")
      context.addUnit(unit)
      Some((conf.name, new EntityManager(unit.name, file, resourceFile, unit, conf)))
    } catch {
      case e: Exception =>
        val msg = s"Failed to parse file at $file (relative to ${FileUtil.currentWorkingDir })"
        inform(msg)
        logger.error(msg, e)
        None
    }
  }).flatten.toMap

  def createFileGenerator(codegen: MetaCodegen, unit: SUnitDef, config: UnitConfig) =
    new ModuleFileGenerator(codegen, unit, config)

  val enrichPipeline = new ScalanAstTransformers.EnrichPipeline()

  def generate(configName: String) = {
    entities.get(configName) match {
      case Some(man) =>
        println(s"  generating ${man.file}")
        val enrichedUnit = enrichPipeline(man.module)
        val g = createFileGenerator(ScalanCodegen, enrichedUnit, man.config)

        val implCode = g.emitImplFile
        val implFile = UnitConfig.getImplFile(man.file, "Impl", "scala")
        FileUtil.write(implFile, implCode)
        FileUtil.copy(man.file, man.resourceFile)
      case None =>
        logger.error(s"Cannot generate code for config '$configName' because it is not found.")
    }
  }
}
