package scalan.meta

import scalan.meta.ScalanAst.{SUnitDef, Module}

class SourceUnitVirtualization(implicit val context: AstContext) extends (SUnitDef => SUnitDef) {
  val moduleBuilder = new SModuleBuilder()
  import moduleBuilder._

  private val chain = scala.Function.chain(Seq(
    fixExistentialType _,
    transConstr _,
    replaceImplicitDescriptorsWithElems _,
    externalTypeToWrapper _,
    //      composeParentWithExt _,
    addBaseToAncestors _,
    addDefAncestorToAllEntities _,
    updateSelf _,
    //      addEntityRepSynonym _,
    addImports _,
    checkEntityCompanion _,
    checkClassCompanion _,
    genEntityImplicits _,

    eliminateClassTagApply _,
    // genClassesImplicits _, genMethodsImplicits _,
    fixEntityCompanionName _,
    fixEvidences _,
    transConstr _
//    optimizeModuleImplicits _
  ))
  override def apply(module: Module): Module = chain(module)
}
