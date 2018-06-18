package scalan.meta

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scalan.meta.ScalanAst.SUnitDef

trait ScalanParsersEx[G <: Global]
  extends ScalanParsers[G] with ScalanGens[G] {
  val settings = new Settings
  settings.embeddedDefaults(getClass.getClassLoader)
  settings.usejavacp.value = true
  val reporter = new StoreReporter

  // Credit due to Li Haoyi in Ammonite:
  // Initialize scalac to the parser phase immediately, so we can start
  // using Compiler#parse even if we haven't compiled any compilation
  // units yet due to caching
  def initCompiler() = {
    val run = new compiler.Run()
    compiler.phase = run.parserPhase
    run.cancel()
  }

  import compiler._

  case class TestModule(moduleName: String, text: String, isVirt: Boolean)

  def parseModule(module: TestModule): SUnitDef = {
    implicit val ctx = new ParseCtx(module.isVirt)(context)
    val pkg = parseString(TopLevel, module.text).asInstanceOf[PackageDef]
    val m = unitDefFromPackageDef(module.moduleName, pkg)
    m
  }
}
