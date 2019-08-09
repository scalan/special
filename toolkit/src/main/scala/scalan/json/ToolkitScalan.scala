package scalan.json

import scalan.meta.{Parsers, AstContextBase}
import scalan.meta.ScalanAst.SUnitDef
import scalan.primitives.StringOps

import scala.collection.mutable
import scalan.{ScalanEx, ModuleInfo, ModulesEx}

trait ParsedModules extends ModulesEx { scalan: ScalanEx =>
  lazy val parsers = {
    val parsers = new Parsers(configs)
    implicit val parseCtx = new parsers.context.parsers.ParseCtx(true)(parsers.context)
    parsers.context.loadUnitsFromResources()
    parsers
  }

  protected lazy val modules = parsers.context.units

  override def astContext: AstContextBase = parsers.context

  override def getModules: mutable.Map[String, SUnitDef] = modules

  override protected def registerModule(moduleInfo: ModuleInfo) = {
    if (okRegisterModules) {
      val pack = moduleInfo.packageName
      val name = moduleInfo.moduleName
      if (!parsers.context.hasUnit(pack, name)) {
        val m = parsers.loadUnitDefFromResource(moduleInfo.sourceFileName)(new parsers.ParseCtx(true)(parsers.context))
        parsers.context.addUnit(m)
        //      println(s"WARNING: module $pack.$name added by registerModule")
      }
    }
  }
}

class ToolkitScalan extends ScalanEx with ParsedModules with StringOps
