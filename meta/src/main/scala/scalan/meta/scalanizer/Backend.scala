package scalan.meta.scalanizer

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstTransformers._
import scalan.meta.{ScalanCodegen, ModuleFileGenerator}
import scalan.util.ScalaNameUtil.PackageAndName

trait Backend[+G <: Global] extends ScalanizerBase[G] {
  import global._

  /** Generate boilerplate text for virtualized user-defined module */
  def genUDModuleBoilerplateText(unitName: String, unit: SUnitDef): String = {
    val unitConf = snConfig.getUnitConfig(unitName)
    val unitSym = context.newUnitSymbol(unit.packageName, unit.name)
    val gen = new ModuleFileGenerator(
      ScalanCodegen,
      unit.copy(
        origModuleTrait = Some(createModuleTrait(unitSym)),
        okEmitOrigModuleTrait = true
      )(context),
      unitConf.copy(
        extraImports = unitConf.extraImports :+ "scala.wrappers.WrappersModule"
      )
    )
    gen.emitImplFile
  }

}
