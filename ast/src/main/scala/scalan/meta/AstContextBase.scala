package scalan.meta

import com.trueaccord.lenses.{Mutation, Lens}
import scalan.meta.ScalanAst.{STraitCall, STpeDef, SUnitDef, STpeExpr, STpeFunc, SExpr, SEntityAnnotation, createSubst, Entity, SConst, ExternalAnnotation, SEmpty, WrapperDescr, SEntityDef, Module, SSelect}

import scala.collection.mutable.{Map => MMap}

trait AstContextBase extends Symbols {
  def configs: List[UnitConfig]
  def okLoadModules: Boolean

  /** Mapping of external type names to their wrappers ("Array" -> WrapperDescr). */
  private[scalan] val wrappers = MMap[String, WrapperDescr]()

  /** Mapping of W-entities to the corresponding wrapped type name (e.g. "WArray" -> "Array") */
  private[scalan] val entityToWrapper = MMap[String, String]()

  /** Mapping of <packageName>.<moduleName> to unit configuration.
    * Initial set of configs is taken from configs argument and later new configs can be added. */
  private[scalan] val unitConfigs: MMap[String, UnitConfig] = MMap(configs.map(c => c.unitKey -> c): _*)

  /** Mapping of <packageName>.<moduleName> to definition.
    * Initial set of units is loaded from the configs and later new units can be added. */
  private[scalan] val units = MMap[String, SUnitDef]()

  def updateWrapper(typeName: String, descr: WrapperDescr) = {
    wrappers(typeName) = descr
    val entityName = descr.unit.traits(0).name
    entityToWrapper(entityName) = typeName
  }

  def externalTypes = wrappers.keySet

  def hasWrapper(typeName: String) = wrappers.contains(typeName)
  def getWrapper(typeName: String) = wrappers.get(typeName)

  def forEachWrapper(action: ((String, WrapperDescr)) => Unit) = {
    wrappers.foreach(action)
  }

  def transformWrappers(transformer: ((String, WrapperDescr)) => WrapperDescr) = {
    wrappers.transform(scala.Function.untupled(transformer))
  }

  def isEntity(name: String): Boolean = {
    val res = for ( m <- units.values; e <- m.traits if e.name == name) yield ()
    res.nonEmpty
  }
  def isEntityCompanion(name: String): Boolean = {
    val res = for ( m <- units.values; e <- m.traits; c <- e.companion if c.name == name) yield ()
    res.nonEmpty
  }
  def isClass(name: String): Boolean = {
    val res = for ( m <- units.values; c <- m.classes if c.name == name) yield ()
    res.nonEmpty
  }
  def isClassCompanion(name: String): Boolean = {
    val res = for ( m <- units.values; c <- m.classes; comp <- c.companion if comp.name == name) yield ()
    res.nonEmpty
  }
  def isModule(name: String): Boolean = {
    units.valuesIterator.map(_.name).toSet.contains(name)
  }

  private[this] val highOrderTpes = Set("Thunk")

  def getKind(name: String): Int = {
    if (highOrderTpes.contains(name)) 1
    else {
      findModuleEntity(name).map { case (m, e) => e.tpeArgs.length }.getOrElse(0)
    }
  }

  def allModules: Iterator[SUnitDef] = wrappers.valuesIterator.map(_.unit) ++ units.valuesIterator

  //TODO refactor to use Name for more precise ModuleEntity search
  def findModuleEntity(entityName: String): Option[(Module, Entity)] = {
    def isEqualName(m: SUnitDef, shortName: String, fullName: String): Boolean =
      fullName == shortName || fullName == s"${m.packageName}.$entityName.$shortName"

    def findByName(m: SUnitDef, es: List[SEntityDef]) =
      es.find(e => isEqualName(m, e.name, entityName))

    val res = allModules collectFirst scala.Function.unlift { m =>
      findByName(m, m.traits)
          .orElse(findByName(m, m.classes))
          .map((m, _))
    }
    res
  }

  def typeDefs: Map[String, STpeDef] = {
    val defs = for {
      m <- allModules
      t <- m.typeDefs
    }
      yield t.name -> t
    defs.toMap
  }

  @inline def unitKey(packageName: String, unitName: String): String = {
    SSymName.fullNameString(packageName, unitName)
  }

  @inline def hasUnit(packageName: String, unitName: String): Boolean = {
    val key = unitKey(packageName, unitName)
    units.contains(key)
  }

  def getUnit(packageName: String, unitName: String): SUnitDef = {
    val key = unitKey(packageName, unitName)
    units(key)
  }
  def getUnit(name: SSymName): SUnitDef = getUnit(name.packageName, name.name)

  def addUnit(unit: SUnitDef): SUnitDef = {
    val key = unit.getUnitKey
    units(key) = unit
    unit
  }
  def addUnit(unit: SUnitDef, unitConf: UnitConfig): SUnitDef = {
    addUnitConfig(unitConf)
    addUnit(unit)
  }

  def removeUnit(key: String) = {
    units.remove(key)
  }

  def updateUnit(name: SSymName, by: Lens[SUnitDef, SUnitDef] => Mutation[SUnitDef]): SUnitDef = {
    val u = getUnit(name)
    val key = name.mkFullName
    val newUnit = u.update(by)
    units(key) = newUnit
    newUnit
  }

  def addUnitConfig(conf: UnitConfig): UnitConfig = {
    val key = conf.unitKey
    unitConfigs(key) = conf
    conf
  }

  def getUnitConfig(packageName: String, unitName: String): UnitConfig = {
    val key = unitKey(packageName, unitName)
    unitConfigs(key)
  }

  def getUnitConfig(name: SSymName): UnitConfig = getUnitConfig(name.packageName, name.name)

  @inline def hasUnitConfig(packageName: String, unitName: String): Boolean = {
    val key = unitKey(packageName, unitName)
    unitConfigs.contains(key)
  }

  def updateUnitConfig(name: SSymName, by: Lens[UnitConfig, UnitConfig] => Mutation[UnitConfig]): UnitConfig = {
    val c = getUnitConfig(name)
    val key = name.mkFullName
    val newConf = c.update(by)
    unitConfigs(key) = newConf
    newConf
  }

  def removeUnitConfig(key: String) = {
    unitConfigs.remove(key)
  }

  object TypeDef {
    /** Recognizes usage of STpeDef and substitutes args to rhs */
    def unapply(tpe: STpeExpr): Option[(STpeDef, STpeExpr)] = tpe match {
      case STraitCall(n, args) =>
        typeDefs.get(n).map { td =>
          val subst = createSubst(td.tpeArgs, args)
          (td, td.tpe.applySubst(subst))
        }
      case _ => None
    }
  }

  object RepTypeOf {
    def unapply(tpe: STpeExpr): Option[STpeExpr] = tpe match {
      case STraitCall("Rep", Seq(t)) =>   // Rep[t] --> t
        Some(t)
      case STraitCall("RFunc", Seq(a, b)) =>  // RFunc[a,b] --> a => b
        Some(STpeFunc(a, b))
      case TypeDef(td, RepTypeOf(t)) => // type RepCol[args] = Rep[Col[args]] then RepCol[args] --> Col[args]
        Some(t)
      case _ => None
    }
  }

  object Entity {
    def unapply(name: String): Option[(Module, Entity)] =
      findModuleEntity(name)
  }

  object ExternalType {
    def unapply(name: String): Option[(Module, Entity)] = getWrapper(name) match {
      case Some(wd) =>
        Some((wd.unit, wd.unit.traits(0)))
      case None => None
    }
  }

  object WrapperEntity {
    def unapply(name: String): Option[(Module, Entity, String)] = name match {
      case Entity(m, e) =>
        e.getAnnotation(ExternalAnnotation) match {
          case Some(SEntityAnnotation(_, _, List(SConst(externalName: String, _)))) => Some((m, e, externalName))
          case _ => None
        }
      case _ => None
    }
  }

  object R {
    def unapply(s: String): Option[String] =
      if (s.startsWith("R")) Some(s.substring(1)) else None
  }

  object IsGlobalObject {
    def unapply(expr: SExpr): Option[String] = expr match {
      case SSelect(SEmpty(None), Entity(_,e), None) => Some(e.name)
      case SSelect(SEmpty(None), WrapperEntity(_,e,_), None) => Some(e.name)
      case SSelect(SEmpty(None), ExternalType(_,e), None) => Some(e.name)
      case SSelect(SEmpty(None), R(Entity(_,e)), None) => Some(e.name)
      case SSelect(SEmpty(None), R(WrapperEntity(_,e,_)), None) => Some(e.name)
      case SSelect(SEmpty(None), R(ExternalType(_,e)), None) => Some(e.name)
      case _ => None
    }
  }

}
