package scalan.plugin

import java.io.File
import java.net.URLClassLoader
import java.util.Properties

import scalan.meta.scalanizer.Scalanizer
import scala.tools.nsc.Global
import scala.collection.mutable.{Map => MMap}
import scalan.meta._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.Symbols.{SEntityDefSymbol, SSymbol, SEntitySymbol, SUnitDefSymbol}
import scala.tools.nsc.doc.ScaladocSyntaxAnalyzer
import AstLenses._

class SourceModulePipeline[+G <: Global](s: Scalanizer[G]) extends ScalanizerPipeline[G](s) {
  import scalanizer._
  import scalanizer.global._
  val name = "scalanizer"
  val runAfter = List("typer")
  val virtPipeline = new SourceUnitVirtualization()(context)

  override def isEnabled: Boolean = {
    val moduleName = s.moduleName
    s.snConfig.sourceModules.get(moduleName).isDefined
  }

  val symbolMap: MMap[SSymbol, Symbol] = MMap()

  class SourcePipelineParseCtx(override val isVirtualized: Boolean)
                              (implicit astContext: AstContext)
      extends ParseCtx(isVirtualized) {
    override def onParsed(symTree: SymTreeApi, sdef: NamedDef): Unit = {
      if (symTree.symbol == null || symTree.symbol == NoSymbol) return
      val ssym = sdef.symbol
      if (!symbolMap.contains(ssym)) {
        symbolMap += (ssym -> symTree.symbol)
      }
    }
  }

  class CatchWrappersTraverser(us: SUnitDefSymbol, catcher: (SEntitySymbol,Tree) => Unit) extends Traverser {
    var stack: List[SEntitySymbol] = List(us)   // stack of potentially nested entities
    def currentEntity = stack.head

    override def traverse(t: Tree) {
      t match {
        case cd: ClassDef =>
          val es = SEntityDefSymbol(currentEntity, cd.name)
          catcher(currentEntity, t)
          stack = es :: stack  // push
          super.traverse(t)
          stack = stack.tail  // pop
        case _ =>
          catcher(currentEntity, t)
          super.traverse(t)
      }
    }
  }

  object FindCommentsTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = tree match {
      case DocDef(comment, definition) =>
        super.traverse(tree)
        // FIXME definition.symbol is NoSymbol, we need to link with other symbols somehow
//        fillDocComment(definition.symbol, comment)

        // For now work around by searching for name
        // TODO check for owner name too?
        val name = definition match {
          case named: NameTree => named.name.toString
        }
        symbolMap.valuesIterator.find {
          sym => sym.nameString == name
        }.foreach {
          sym => fillDocComment(sym, comment)
        }
      case _ => super.traverse(tree)
    }
  }


  val steps: List[PipelineStep] = List(
    RunStep("dependencies") { step =>
      implicit val parseCtx: ParseCtx = new ParseCtx(isVirtualized = true)(context)
      val module = scalanizer.getSourceModule
      loadProjectModuleDeps(step, module)

      // add not yet virtualized units from the current module
      // because we are running after typer, all the names has been resolved by the compiler
      // we need to ensure visibility of all the names by scalanizer as well
      for (unitConf <- module.units.values) {
        val parseCtx = new ParseCtx(isVirtualized = false)(context)
        val unit = parseUnitFile(unitConf.getFile)(parseCtx)
        if (!context.hasUnit(unit.packageName, unit.name))
          scalanizer.inform(s"Step(${step.name}): Adding unit ${unit.packageAndName} form module '${s.moduleName}' (parsed from ${unitConf.getFile})")
        context.addUnit(unit, unitConf)
      }
    },
    ForEachUnitStep("wrapfrontend") { context => import context._;
      val unitFileName = unit.source.file.name
      findUnitModule(unitFileName) match {
        case Some((_, unitConf)) if unitConf.definesWrappers =>
          implicit val ctx = new SourcePipelineParseCtx(isVirtualized = false)(scalanizer.context)
          val unitDef = unitDefFromTree(unitFileName, unit.body)
          val t = new CatchWrappersTraverser(unitDef.unitSym, catchWrapperUsage)
          t.traverse(unit.body)
        case _ =>
      }
    },
    RunStep("enricher") { _ =>
      import virtPipeline._
      import moduleBuilder._
      implicit val context = virtPipeline.context

      context.transformWrappers {
        case (name, wDescr) if !wDescr.isImported =>
          /** Transformations of Wrappers by adding of Elem, Cont and other things. */
          val pipeline = scala.Function.chain(Seq(
            preventNameConflict _,
            addBaseToAncestors _,
            addDefAncestorToAllEntities _,
            updateSelf _,
            checkEntityCompanion _,
            constr2apply _,
            replaceImplicitDescriptorsWithElems _,
            preventNameConflict _,
            moduleBuilder.externalTypeToWrapper _,
            genEntityImplicits _,
            genMethodsImplicits _,
            /** Currently, inheritance of type wrappers is not supported.
              * Print warnings and remove ancestors. */
            filterAncestors _
          ))
          val enrichedUnit = pipeline(wDescr.unit)
          wDescr.copy(unit = enrichedUnit)

        case (_,wDescr) => wDescr
      }
      ()
    },
    RunStep("wrapbackend") { _ =>
      context.forEachWrapper {
        case (_, WrapperDescr(u, config, false)) =>
          implicit val context = scalanizer.context
          val moduleConf = getSourceModule
          val wSpecPackageName = moduleConf
              .wrapperSpecUnit(config.name)
              .map(_.packageName)
              .getOrElse("wrappers")
          val wUnit = u.copy(imports = u.imports :+ SImportStat(s"$wSpecPackageName.WrappersModule"))

          /** Build source code of the wrapper unit and store it in a file */
          val wUnitWithoutImpl = wUnit.copy(classes = Nil)(context)
          val optImplicits = optimizeUnitImplicits(wUnitWithoutImpl)
          val withImports = optImplicits.addInCakeImports
          val wrapperPackage = genPackageDef(withImports, isVirtualized = false)
          saveWrapperCode(moduleConf,
            withImports.packageName,
            withImports.name,
            showCode(wrapperPackage))
        case _ => // do nothing
      }
    },
    ForEachUnitStep("virtfrontend") { context => import context._;
      withUnitModule(unit) { (module, unitConf) =>
        val unitFileName = unitConf.name
        // this unit has been added in 'dependencies' step
        val existingUnit = context.getUnit

        // now it can be merged with the unit which has passed namer and typer
        implicit val ctx = new ParseCtx(isVirtualized = false)(scalanizer.context)
        val typedUnitDef = unitDefFromTree(unitFileName, unit.body)

        val merger = new SUnitMerger(existingUnit)(scalanizer.context)
        val mergedUnit = merger.merge(typedUnitDef)

        scalanizer.inform(
            s"Step(virtfrontend): Updating source unit ${existingUnit.packageAndName} " +
            s"with version from CompilationUnit(${unit.source.file})")
        scalanizer.context.addUnit(mergedUnit, unitConf)
      }
    },
    ForEachUnitStep("virtfinal") { context => import context._
      withUnitModule(unit) { (module, conf) =>
       val unitDef = context.getUnit

        /** Generates a virtualized version of original Scala AST, wraps types by Rep[] and etc. */
        val virtUnitDef = virtPipeline(unitDef)
        var optUnitDef = optimizeUnitImplicits(virtUnitDef)
        optUnitDef = optUnitDef.addInCakeImports

        /** Scala AST of virtualized module */
        implicit val ctx = GenCtx(scalanizer.context, isVirtualized = false, toRep = true)
        val virtAst = genPackageDef(optUnitDef)
        
        saveCodeToResources(module, optUnitDef.packageName, optUnitDef.name, showCode(virtAst))
      }
    },

    ForEachUnitStep("collectDocs") { context =>
      val analyzer = new ScaladocSyntaxAnalyzer[global.type](global) {
        override val runsAfter: List[String] = Nil
        override val runsRightAfter: Option[String] = None
      }
      val parser = new analyzer.ScaladocUnitParser(context.unit, Nil)
      val treeWithDocs = parser.parse()
      FindCommentsTraverser.traverse(treeWithDocs)
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
          val pluginInstance = clazz.newInstance().asInstanceOf[PluginForScalanizer]
          pluginInstance(scalanizer.context, snConfig, SourceModulePipeline.this, pluginConfig.extraData)
          scalanizer.inform(s"Step(plugins): plugin ${pluginConfig.classesPath} run successfully")
        } catch {
          case e: Exception =>
            throw new Exception(s"Failed to run the plugin ${pluginConfig.classesPath} for Scalanizer", e)
        }
      }
    }
  )
}
