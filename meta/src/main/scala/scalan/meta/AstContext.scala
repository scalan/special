package scalan.meta

import scala.tools.nsc.Global
import scalan.util.FileUtil
import java.io.File

import com.trueaccord.lenses.{Mutation, Lens}

import scala.collection.mutable.{Map => MMap}
import scalan.meta.ScalanAst.{STraitCall, STpeDef, SUnitDef, STpeExpr, STpeFunc, SExpr, SEntityAnnotation, createSubst, Entity, SConst, ExternalAnnotation, SEmpty, WrapperDescr, SEntityDef, Module, SSelect}

class AstContext(val configs: List[UnitConfig], val parsers: ScalanParsers[Global], val okLoadModules: Boolean = false)
    extends AstContextBase {

  def loadUnitsFromResources()(implicit ctx: parsers.ParseCtx): Unit = {
    for (c <- unitConfigs.values) {
      val m = parsers.loadUnitDefFromResource(c.entityResource)
      addUnit(m)
    }
  }

  def loadModulesFromFolders(): Unit = {
    for (c <- unitConfigs.values) {
      val file = c.getFile
      try {
        val m = parsers.parseUnitFile(file)(new parsers.ParseCtx(c.isVirtualized)(this))
        addUnit(m)
      } catch {
        case t: Throwable =>
          val fullName = new File(FileUtil.currentWorkingDir, file.getPath)
          throw new RuntimeException(s"Error loading module from $fullName", t)
      }
    }
  }


  /** The types that shouldn't be Rep[].
    * For example List("Elem", "Cont", "ClassTag") */
  val typeClasses = Set("Elem", "Cont", "ClassTag", "Functor")


}
