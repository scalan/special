package scalan

import scalan.meta.ScalanAst.{SUnitDef, SEntityDef}
import scala.collection.mutable
import scala.reflect.internal.util.BatchSourceFile
import scalan.meta.{Parsers, BoilerplateToolRun, SSymName}
import scalan.util.{ReflectionUtil, FileUtil}

trait Modules extends Base { self: Scalan =>
  private[scalan] lazy val parsers = {
    val parsers = new Parsers(BoilerplateToolRun.allConfigs)
    implicit val parseCtx = new parsers.context.parsers.ParseCtx(true)(parsers.context)
    parsers.context.loadUnitsFromResources()
    parsers
  }

  private[this] lazy val modules = parsers.context.units

  def getModules: mutable.Map[String, SUnitDef] = modules

  def allEntities = getModules.values.flatMap(_.allEntities)

  def registerModule(moduleInfo: ModuleInfo) = {
    val pack = moduleInfo.packageName
    val name = moduleInfo.moduleName
    if (!parsers.context.hasUnit(pack, name)) {
      val m = parsers.loadUnitDefFromResource(moduleInfo.sourceFileName)(new parsers.ParseCtx(true)(parsers.context))
      parsers.context.addUnit(m)
//      println(s"WARNING: module $pack.$name added by registerModule")
    }
  }

  def entityDef(e: EntityElem[_]): SEntityDef = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val moduleName = elemClassSymbol.owner.name.toString.stripSuffix("Defs")
    val packageName = e.getClass.getPackage.getName.stripSuffix(".impl")
    val key = SSymName.fullNameString(packageName, moduleName)
    val module = modules.getOrElse(key, !!!(s"Module $key not found"))
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

}
