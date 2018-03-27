package scalan.plugin

import java.io.File
import java.net.URLClassLoader
import java.util.Properties

import scalan.meta.scalanizer.{Plugin, Scalanizer}
import scala.tools.nsc.Global
import scala.collection.mutable.{Map => MMap}
import scalan.meta._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.Symbols.{SEntityDefSymbol, SEntitySymbol, SUnitDefSymbol, SSymbol}

class SourceModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._
  val name = "scalanizer"
  val runAfter = List("typer")
  val virtPipeline = new ModuleVirtualizationPipeline()(context)

  override def isEnabled: Boolean = {
    val moduleName = s.moduleName
    s.snConfig.sourceModules.get(moduleName).isDefined
  }

  val symbolMap: MMap[SSymbol, Symbol] = MMap()

  class SourcePipelineParseCtx(override val isVirtualized: Boolean)
                             (implicit astContext: AstContext)
      extends ParseCtx(isVirtualized) {
    override def onParsed(symTree: SymTreeApi, sdef: NamedDef) = {
      val ssym = sdef.symbol
      if (!symbolMap.contains(ssym)) {
        symbolMap += (ssym -> symTree.symbol)
      }
    }
  }

  class CatchWrappersTraverser(us: SUnitDefSymbol, f: (SEntitySymbol,Tree) => Unit) extends Traverser {
    var stack: List[SEntitySymbol] = List(us)   // stack of potentially nested entities
    def currentEntity = stack.head

    override def traverse(t: Tree) {
      t match {
        case cd: ClassDef =>
          val es = SEntityDefSymbol(currentEntity, cd.name)
          f(currentEntity, t)
          stack = es :: stack  // push
          super.traverse(t)
          stack = stack.tail  // pop
        case _ =>
          f(currentEntity, t)
          super.traverse(t)
      }
    }
  }

  def loadModuleUnits(step: PipelineStep, source: ModuleConf)(implicit ctx: ParseCtx): Unit = {
    for (depModule <- source.dependsOnModules()) {
      loadModuleUnits(step, depModule)
    }
    for (unitConf <- source.units.values) {
      if(!scalanizer.context.hasUnit(unitConf.packageName, unitConf.unitName)) {
        val unit = scalanizer.loadUnitDefFromResource(unitConf.entityResource)
        snState.addUnit(unit, unitConf)
        scalanizer.inform(
          s"Step(${step.name}): Adding unit ${unit.packageAndName} form module '${source.name}' " +
              s"(parsed from resource ${unitConf.entityFile})")
      }
    }
  }

  def loadLibraryDeps(step: PipelineStep, lib: LibraryConfig)(implicit ctx: ParseCtx): Unit = {
    for (source <- lib.sourceModules) {
      loadModuleUnits(step, source)
    }
  }

  val steps: List[PipelineStep] = List(
    RunStep("dependencies") { step =>
      implicit val parseCtx: ParseCtx = new ParseCtx(isVirtualized = true)(context)
      val module = scalanizer.getSourceModule
      // add Special units from dependencies in the current project
      for (depModule <- module.dependsOnModules()) {
        for (unitConf <- depModule.units.values) {
          val unit = parseUnitFile(unitConf.getResourceFile)
          scalanizer.inform(s"Step(${step.name}): Adding dependency ${unit.packageAndName} parsed from ${unitConf.getResourceFile}")
          snState.addUnit(unit, unitConf)
        }
      }
      // add Special units from libraries we depend on
      for (lib <- module.libraryDeps.values) {
        loadLibraryDeps(step, lib)
      }

      // add Special units from modules we depend on
      for (module <- module.moduleDeps.values) {
        loadModuleUnits(step, module)
      }

      // add not yet virtualized units from the current module
      // because we are running after typer, all the names has been resolved by the compiler
      // we need to ensure visibility of all the names by scalanizer as well
      for (unitConf <- module.units.values) {
        val parseCtx = new ParseCtx(isVirtualized = false)(context)
        val unit = parseUnitFile(unitConf.getFile)(parseCtx)
        scalanizer.inform(s"Step(${step.name}): Adding unit ${unit.packageAndName} form module '${s.moduleName}' (parsed from ${unitConf.getFile})")
        snState.addUnit(unit, unitConf)
      }
    },
    ForEachUnitStep("wrapfrontend") { context => import context._;
      val unitFileName = unit.source.file.name
      if (isModuleUnit(unitFileName)) {
        implicit val ctx = new SourcePipelineParseCtx(isVirtualized = false)(scalanizer.context)
        val unitDef = unitDefFromTree(unitFileName, unit.body)
        val t = new CatchWrappersTraverser(unitDef.unitSym, catchWrapperUsage)
        t.traverse(unit.body)
      }
    },
    RunStep("enricher") { _ =>
      import virtPipeline._
      import moduleBuilder._
      implicit val context = virtPipeline.context

      snState.transformWrappers { case (name, wrapperDescr) =>
        /** Transformations of Wrappers by adding of Elem, Cont and other things. */
        val pipeline = scala.Function.chain(Seq(
          preventNameConflict _,
          addBaseToAncestors _,
          addDefAncestorToAllEntities _,
          updateSelf _,
          checkEntityCompanion _,
          constr2apply _,
          removeClassTagsFromSignatures _,
          preventNameConflict _,
          genEntityImplicits _,
          genMethodsImplicits _,
          replaceExternalTypeByWrapper _,
          /** Currently, inheritance of type wrappers is not supported.
            * Print warnings and remove ancestors. */
          filterAncestors _
        ))
        val enrichedModule = pipeline(wrapperDescr.module)

        wrapperDescr.copy(module = enrichedModule)
      }
      ()
    },
    RunStep("wrapbackend") { _ =>
      snState.forEachWrapper { case (_, WrapperDescr(u, _, config)) =>
        val wUnit = u.copy(imports = u.imports :+ SImportStat("scala.wrappers.WrappersModule"))(scalanizer.context)
        val moduleConf = getSourceModule

        /** Build source code of the wrapper unit and store it in a file */
        val wUnitWithoutImpl = wUnit.copy(classes = Nil)(context)
        val optImplicits = optimizeModuleImplicits(wUnitWithoutImpl)
        val wrapperPackage = genPackageDef(optImplicits, isVirtualized = false)(scalanizer.context)
        saveWrapperCode(moduleConf,
          optImplicits.packageName,
          optImplicits.name,
          showCode(wrapperPackage))
      }
    },
    ForEachUnitStep("virtfrontend") { context => import context._;
      withUnitModule(unit) { (module, unitConf) =>
        val unitFileName = unitConf.name
        // this unit has been added in 'dependencies' step
        val existingUnit = context.getUnit

        // now it can be replaced with the body which has passed namer and typer
        implicit val ctx = new ParseCtx(isVirtualized = false)(scalanizer.context)
        val unitDef = unitDefFromTree(unitFileName, unit.body)

        scalanizer.inform(
            s"Step(virtfrontend): Updating source unit ${existingUnit.packageAndName} " +
            s"with version from CompilationUnit(${unit.source.file})")
        snState.addUnit(unitDef, unitConf)
      }
    },
    ForEachUnitStep("virtfinal") { context => import context._
      withUnitModule(unit) { (module, conf) =>
        val unitFileName = conf.name
        val unitDef = context.getUnit

        /** Generates a virtualized version of original Scala AST, wraps types by Rep[] and etc. */
        val virtUnitDef = virtPipeline(unitDef)

        /** Scala AST of virtualized module */
        implicit val ctx = GenCtx(scalanizer.context, isVirtualized = false, toRep = true)
        val virtAst = genPackageDef(virtUnitDef)
        
        saveCodeToResources(module, virtUnitDef.packageName, virtUnitDef.name, showCode(virtAst))
      }
    },

    RunStep("plugins") { _ =>
      for (pluginConfig <- snConfig.pluginConfigs) {
        val jarOrDir = pluginConfig.classesPath
        val url = new File(jarOrDir).toURI.toURL
        val classLoader = new URLClassLoader(Array(url), getClass.getClassLoader)
        try {
          val stream = classLoader.getResourceAsStream("scalanizer-plugin.properties")
          val properties = new Properties()
          try {
            properties.load(stream)
          } finally {
            stream.close()
          }
          val className = properties.get("className").asInstanceOf[String]
          val clazz = classLoader.loadClass(className)
          val pluginInstance = clazz.newInstance().asInstanceOf[Plugin]
          pluginInstance(scalanizer.context, snConfig, pluginConfig.extraData)
          scalanizer.inform(s"Step(plugins): plugin ${pluginConfig.classesPath} run successfully")
        } catch {
          case e: Exception =>
            throw new Exception(s"Failed to run the plugin ${pluginConfig.classesPath} for Scalanizer", e)
        }
      }
    }
  )
}
