package scalan.plugin

import java.io.File

import scala.reflect.io.{PlainFile, Path}
import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta._
import scalan.util.FileUtil
import scalan.util.CollectionUtil._
import scala.collection.mutable.{Map => MMap}

class TargetModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._

  val name = "target-assembler"
  val runAfter = List("parser")
  val preparedUnits = MMap[SSymName, (SUnitDef, UnitConfig)]()

  override def isEnabled: Boolean = {
    val moduleName = s.moduleName
    s.snConfig.targetModules.get(moduleName).isDefined
  }

  def copyFile(sourceFile: File, targetFile: File): Boolean = {
    val isNewFile = !targetFile.exists
    scalanizer.inform(s"Copying from $sourceFile to $targetFile ...")
    FileUtil.copy(sourceFile, targetFile)
    isNewFile
  }

  val moduleBuilder = new SModuleBuilder()(context)

  def prepareSourceUnit(unitConf: UnitConfig, target: TargetModuleConf) = {
    val sourceFile = unitConf.getResourceFile
    implicit val parseCtx = new ParseCtx(isVirtualized = true)(context)

    val sourceUnit = parseUnitFile(sourceFile)
    val selfTypeName = target.cakeName
    val preparedUnit = moduleBuilder.setSelfType(selfTypeName)(sourceUnit)
    preparedUnit
  }

  def copyScalanizedUnit(preparedUnit: SUnitDef, unitConf: UnitConfig, target: TargetModuleConf): Unit = {
    val targetSrcRoot = target.getSourcesRootDir
    val targetSrcFile = FileUtil.file(targetSrcRoot, unitConf.entityFile)
    val isNewTargetFile = !targetSrcFile.exists

    implicit val genCtx = new GenCtx(context, isVirtualized = true, toRep = true)
    val preparedTree = genPackageDef(preparedUnit)
    val code = showCode(preparedTree)
//    saveCode(target.getResourcesRootDir,
//        preparedUnit.packageName, preparedUnit.name, ".scalan", code)
    saveCode(targetSrcRoot, preparedUnit.packageName, preparedUnit.name, ".scala", code)

    /** produce boilerplate code using ModuleFileGenerator
      * NOTE: we need a unit with all implicits argument for classes and methods for correct boilerplate generation */
    val withImplicits = moduleBuilder.genClassesImplicits(moduleBuilder.genMethodsImplicits(preparedUnit))
    val boilerplateText = genUnitBoilerplateText(target, withImplicits, isVirtualized = true)
    val targetImpl = saveImplCode(targetSrcRoot, withImplicits.packageName, withImplicits.name, ".scala", boilerplateText)
    val isNewImpl = !targetImpl.exists

    if (isNewTargetFile)
      global.currentRun.compileLate(new PlainFile(Path(targetSrcFile)))
    if (isNewImpl)
      global.currentRun.compileLate(new PlainFile(Path(targetImpl)))
  }

//  def mergeWrapperUnit(unit: SUnitDef) = {
//    val wName = SSymName(unit.packageName, unit.name)
//    wrappers.get(wName) match {
//      case Some(existingUnit) =>
//        val merger = new SUnitMerger(existingUnit)(scalanizer.context)
//        val newUnit = merger.merge(unit)
//        wrappers(wName) = newUnit
//      case None =>
//        wrappers(wName) = unit
//    }
//  }

  def saveWrappersCake(sourceRoot: String, cake: WrapperCake) = {
    implicit val ctx = GenCtx(context = scalanizer.context, isVirtualized = true, toRep = false)
    val imports =
      genImport(SImportStat("scalan._")) ::
      cake.wrappers.map(w => genImport(SImportStat(SUnitDef.moduleTraitName(w.packageAndName))))
    val cakeTree = genTrait(cake.traitDef)
    val pkgStats = imports :+ cakeTree
    val wrappersPackage = PackageDef(RefTree(Ident(TermName("scala")), TermName("wrappers")), pkgStats)
    val code = showCode(wrappersPackage)
    saveCode(sourceRoot, "scala.wrappers", cake.traitDef.name, ".scala", code)
  }

  def collectWrappers(step: PipelineStep, target: TargetModuleConf): Map[SSymName, SUnitDef] = {
    // collect all wrappers loaded into context from dependencies
    val loadedWrappers = context.externalTypes.map { extName =>
      val wUnit = context.getWrapper(extName).get.unit
      extName -> wUnit
    }
    // collect this project's wrappers
    val projectWrappers = (for {
      sm <- target.sourceModules.values
      u <- sm.units.values
      (wName, wConf) <- u.wrappers
    } yield wName).toSet

    // output loaded wrappers belonging to current project
    loadedWrappers.filterMap {
      case (n, wUnit) if projectWrappers.contains(n) =>
        Some((wUnit.unitName -> wUnit))
      case _ => None
    }.toMap
  }

  val steps: List[PipelineStep] = List(
    RunStep("dependencies") { step =>
      implicit val parseCtx: ParseCtx = new ParseCtx(isVirtualized = true)(context)
      val target = scalanizer.getTargetModule
      loadProjectModuleDeps(step, target)
    },
    RunStep("assembler") { step =>
      scalanizer.inform(s"Processing target module '${scalanizer.moduleName }'")
      val target = scalanizer.getTargetModule
      val sourceRoot = target.getSourcesRootDir

      // collect all wrappers from source modules
      val wrappers = collectWrappers(step, target)

      // 1) gen boilerplate and save for all merged wrappers
      // 2) build wrappers cake
//      var wrappersCake = initWrapperCake()
      for (wUnit <- wrappers.values) {
        val wPackage = genPackageDef(wUnit, isVirtualized = true)(context)
//        val resourcesRoot = target.getResourcesRootDir
//        saveCode(resourcesRoot, wUnit.packageName, wUnit.name, ".scalan", showCode(wPackage))

        val wOptiUnit = optimizeModuleImplicits(wUnit)
        val optiPackage = genPackageDef(wOptiUnit, isVirtualized = true)(context)
        saveCode(sourceRoot, wOptiUnit.packageName, wOptiUnit.name, ".scala", showCode(optiPackage))

        // NOTE: we use original UnitDef with all implicits (non optimized)
        val boilerplateText = genUnitBoilerplateText(target, wUnit, isVirtualized = true)
        saveImplCode(sourceRoot, wUnit.packageName, wUnit.name, ".scala", boilerplateText)

//        wrappersCake = updateWrappersCake(wrappersCake, wUnit)
      }

      // generate WrappersModule cake
//      saveWrappersCake(sourceRoot, wrappersCake)

      // prepare units from source modules
      for (srcModule <- target.sourceModules.values) {
        for (srcUnitConf <- srcModule.units.values) {
          val prepared = prepareSourceUnit(srcUnitConf, target)
          context.addUnit(prepared)
          val name = SSymName(prepared.packageName, prepared.name)
          preparedUnits += ((name, (prepared, srcUnitConf)))
        }
      }

      // generate CompilationUnit for each prepared unit from all source modules
      for ((_, (unit, conf)) <- preparedUnits) {
        copyScalanizedUnit(unit, conf, target)
      }
      ()
    }
  )
}
