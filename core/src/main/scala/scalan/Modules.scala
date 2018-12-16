package scalan

import scalan.meta.ScalanAst.{SUnitDef, SEntityDef}
import scala.collection.mutable
import scalan.meta.{SSymName, Parsers, BoilerplateToolRun, UnitConfig}
import scalan.util.ReflectionUtil

trait Modules extends Base { self: Scalan =>
  def configs: List[UnitConfig] = Nil
  
  private[scalan] lazy val parsers = {
    val parsers = new Parsers(configs)
    implicit val parseCtx = new parsers.context.parsers.ParseCtx(true)(parsers.context)
    parsers.context.loadUnitsFromResources()
    parsers
  }

  private[this] lazy val modules = parsers.context.units

  def getModules: mutable.Map[String, SUnitDef] = modules

  def allEntities = getModules.values.flatMap(_.allEntities)

  def okRegisterModules: Boolean = false

  def registerModule(moduleInfo: ModuleInfo) = {
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

  def entityDef(e: EntityElem[_]): SEntityDef = {
    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
    val owner = if(getEntityObject(entityName).isDefined)
        elemClassSymbol.owner.owner
      else
        elemClassSymbol.owner
    val moduleName = owner.name.toString.stripSuffix("Defs")
    val packageName = e.getClass.getPackage.getName.stripSuffix(".impl")
    val key = SSymName.fullNameString(packageName, moduleName)
    val module = modules.getOrElse(key, !!!(s"Module $key not found"))

    module.allEntities.find(_.name == entityName).getOrElse {
      !!!(s"Entity $entityName not found in module $moduleName")
    }
  }

}
