package scalan

import scalan.meta.ScalanAst.{SUnitDef, SEntityDef}
import scalan.meta.{SSymName, UnitConfig, AstContextBase}

import scala.collection.mutable

trait Modules extends Base { self: Scalan =>
  def configs: List[UnitConfig] = Nil

  def astContext: AstContextBase = !!!(s"AstContext is not overridden in IR cake $this")

  def getModules: mutable.Map[String, SUnitDef] = mutable.Map.empty[String, SUnitDef]

  def allEntities = getModules.values.flatMap(_.allEntities)

  def okRegisterModules: Boolean = false

  def registerModule(moduleInfo: ModuleInfo) = {
    if (okRegisterModules) {
      !!!(s"Cannot register module $moduleInfo: registerModule method is not overridden in IR cake $this. ")
    }
  }

//  def entityDef(e: EntityElem[_]): SEntityDef = {
//    val elemClassSymbol = ReflectionUtil.classToSymbol(e.getClass)
//    val entityName = elemClassSymbol.name.toString.stripSuffix("Elem")
//    val owner = if(getEntityObject(entityName).isDefined)
//        elemClassSymbol.owner.owner
//      else
//        elemClassSymbol.owner
//    val moduleName = owner.name.toString.stripSuffix("Defs")
//    val packageName = e.getClass.getPackage.getName.stripSuffix(".impl")
//    val key = SSymName.fullNameString(packageName, moduleName)
//    val module = modules.getOrElse(key, !!!(s"Module $key not found"))
//
//    module.allEntities.find(_.name == entityName).getOrElse {
//      !!!(s"Entity $entityName not found in module $moduleName")
//    }
//  }
}
