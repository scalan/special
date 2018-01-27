package scalan.meta

import scalan.meta.ScalanAstTransformers._
import scalan.meta.ScalanAstUtils._
import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.util.CollectionUtil._

class SModuleBuilder(implicit val context: AstContext) {

  // Pipeline Step
  def externalTypeToWrapper(unit: SUnitDef) = {
    val wrapped = context.externalTypes.foldLeft(unit){(acc, externalTypeName) =>
      new External2WrapperTypeTransformer(externalTypeName).moduleTransform(acc)
    }
    wrapped
  }

  // Pipeline Step
  def fixExistentialType(module: SUnitDef) = {
    new AstTransformer {
      def containsExistential(tpe: STpeExpr): Boolean = {
        var hasExistential = false
        val trans = new TypeTransformer {
          override def existTypeTransform(existType: STpeExistential): STpeExistential = {
            hasExistential = true
            super.existTypeTransform(existType)
          }
        }
        trans(tpe)

        hasExistential
      }

      override def applyTransform(apply: SApply): SApply = {
        val hasExistential = apply.argss exists (_.exists(arg =>
          containsExistential(arg.exprType.getOrElse(STpeEmpty()))
        ))
        def castToUniversal(targs: List[STpeExpr]) = {
          val newArgss = apply.argss map(sec => SArgSection(sec.map(arg =>
            SApply(SSelect(arg, "asRep"),targs, Nil)
          )))
          apply.copy(argss = newArgss)
        }

        if (hasExistential) {
          apply.fun.exprType match {
            case Some(methodType: STpeMethod) => castToUniversal(methodType.params)
            case _ => super.applyTransform(apply)
          }
        } else super.applyTransform(apply)
      }
    }.moduleTransform(module)
  }

  // Pipeline Step
  def tupleToPair(module: SUnitDef) = {
    new AstTransformer {
      override def constrTransform(constr: SConstr): SConstr = constr match {
        case SConstr("Tuple2", args, tpe) => SConstr("Pair", args, tpe)
        case _ => constr
      }
    }.moduleTransform(module)
  }

  /** Make the module inherit from Base trait from Scalan */
  def addBaseToAncestors(module: SUnitDef) = {
    val newAncestors = STraitCall(name = "Base", args = List()).toTypeApply :: module.ancestors
    module.copy(ancestors = newAncestors)
  }

  /** Pipeline Step
    * Make the trait Col[T] extends Def[Col[T] ] */
  def addDefAncestor(e: SEntityDef): SEntityDef = {
    val newAncestors = STraitCall(
      name = "Def",
      args = List(STraitCall(e.name, e.tpeArgs.map(arg => STraitCall(arg.name, List()))))
    ).toTypeApply :: e.ancestors
    e match {
      case t: STraitDef =>
        t.copy(ancestors = newAncestors)
      case c: SClassDef =>
        c.copy(ancestors = newAncestors)
    }
  }

  /** Pipeline Step
    * Make all traits in a given unit extend Def directly or indirectly (i.e. Col[T] extends Def[Col[T] ]) */
  def addDefAncestorToAllEntities(unit: SUnitDef): SUnitDef = {
    var extended = List[SEntityDef]()
    for (entity <- unit.allEntitiesSorted) {
      val alreadyInherit = extended.exists(ext => entity.inherits(ext.name)) || entity.inherits("Def")
      val newEntity =
        if (alreadyInherit) entity
        else {
          addDefAncestor(entity)
        }
      extended ::= newEntity
    }
    val (ts, cs) = extended.reverse.partitionByType[STraitDef, SClassDef]
    unit.copy(traits = ts, classes = cs)
  }

  /** Puts the module to the cake. For example, trait Segments is transformed to
    * trait Segments {self: SegmentsDsl => ... }
    * Pipeline Step*/
  def updateSelf(module: SUnitDef) = {
    module.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = selfModuleComponents(module, "Module")
    )))
  }

  def setSelfType(selfTypeName: String)(unit: SUnitDef): SUnitDef = {
    unit.copy(selfType = Some(SSelfTypeDef(
      name = "self",
      components = List(STraitCall(selfTypeName, List()))
    )))
  }

//  /** Introduces a synonym for each entity. If name of the entity is Matr, the method adds:
//    *   type RepMatr[T] = Rep[Matr[T]]
//    * */
//  def addEntityRepSynonym(module: SModuleDef) = {
//    val entity = module.entityOps
//    def synDef(entity: STraitDef) = STpeDef(
//      name = "Rep" + entityName,
//      tpeArgs = entity.tpeArgs,
//      rhs = STraitCall("Rep", List(STraitCall(entity.name, entity.tpeArgs.map(_.toTraitCall))))
//    )
//    module.copy(entityRepSynonym = Some(synDef))
//  }

  /** Checks that the entity has a companion. If the entity doesn't have it
    * then the method adds the companion. */
  def checkEntityCompanion(module: SUnitDef) = {
    val newTraits = module.traits.map { e =>
      val newCompanion = e.companion match {
        case Some(comp) => Some(convertCompanion(comp))
        case None => Some(createCompanion(e.name))
      }
      e.copy(companion = newCompanion)
    }
    module.copy(traits = newTraits)
  }

  /** Checks that concrete classes have their companions and adds them. */
  def checkClassCompanion(module: SUnitDef) = {
    val newClasses = module.classes.map{ clazz =>
      val newCompanion = clazz.companion match {
        case Some(comp) => Some(convertCompanion(comp))
        case None => Some(createCompanion(clazz.name))
      }
      clazz.copy(companion = newCompanion)
    }
    module.copy(classes = newClasses)
  }

  /** ClassTags are removed because in virtualized code they can be extracted from Elems. */
  def removeClassTagsFromSignatures(unit: SUnitDef) = {
    new RemoveClassTagFromSignatures().moduleTransform(unit)
  }

  def replaceClassTagByElem(unit: SUnitDef) = {
    new AstReplacer("ClassTag", (_:String) => "Elem") {
      override def selectTransform(select: SSelect): SExpr = {
        val type2Elem = Map(
          "AnyRef" -> "AnyRefElement",
          "Boolean" -> "BoolElement",
          "Byte" -> "ByteElement",
          "Short" -> "ShortElement",
          "Int" -> "IntElement",
          "Long" -> "LongElement",
          "Float" -> "FloatElement",
          "Double" -> "DoubleElement",
          "Unit" -> "UnitElement",
          "String" -> "StringElement",
          "Char" -> "CharElement"
        )
        select match {
          case SSelect(SIdent("ClassTag",_), tname,_) if type2Elem.contains(tname) =>
            SSelect(SIdent("self"), type2Elem(tname))
          case _ => super.selectTransform(select)
        }
      }
    }.moduleTransform(unit)
  }

  def eliminateClassTagApply(module: SUnitDef) = {
    new AstTransformer {
      override def applyTransform(apply: SApply): SApply = {
        val newArgss = apply.argss.filterMap { sec =>
          val newArgs = sec.args.filterNot { a =>
            val t = a.exprType.flatMap(ClassTagTpe.unapply)
            t.isDefined
          }
          if (newArgs.isEmpty) None
          else Some(sec.copy(args = newArgs))
        }
        apply.copy(argss = newArgss)
      }
    }.moduleTransform(module)
  }

  /** Adds descriptor methods (def eA, def cF, etc) to the body of the first entity. */
  def genEntityImplicits(unit: SUnitDef) = {
    val newTraits = unit.traits.map { t =>
      val newBody = genDescMethodsByTypeArgs(t.tpeArgs) ++ t.body
      t.copy(body = newBody)
    }
    unit.copy(traits = newTraits)
  }

  /** Add implicit Elem arguments and implicit descriptor methods. */
  def genClassesImplicits(unit: SUnitDef): SUnitDef = {
    def unpackElem(classArg: SClassArg): Option[STpeExpr] = classArg.tpe match {
      case STraitCall("Elem", List(prim @ STpePrimitive(_,_))) => Some(prim)
      case _ => None
    }
    /** The function checks that the Elem is already defined somewhere in scope. */
    def isElemAlreadyDefined(classArg: SClassArg): Boolean = unpackElem(classArg) match {
      case Some(_) => true
      case None => false
    }
    def convertElemValToMethod(classArg: SClassArg): SMethodDef = {
      SMethodDef(name = classArg.name, tpeArgs = Nil, argSections = Nil,
        tpeRes = Some(classArg.tpe),
        isImplicit = false, isOverride = false, overloadId = None, annotations = Nil,
        body = Some(SExprApply(SIdent("element"), unpackElem(classArg).toList)),
        isTypeDesc = true)
    }
    val newClasses = unit.classes.map { c =>
      val (definedElems, elemArgs) = genImplicitArgsForClass(c)(unit.context) partition isElemAlreadyDefined
      val newArgs = (c.implicitArgs.args ++ elemArgs).distinctBy(_.tpe match {
        case TypeDescTpe(_,ty) => ty
        case t => t
      })
      val newImplicitArgs = SClassArgs(newArgs)
      val newBody = definedElems.map(convertElemValToMethod) ++ c.body

      c.copy(implicitArgs = newImplicitArgs, body = newBody)
    }

    unit.copy(classes = newClasses)
  }

  def genMethodsImplicits(module: SUnitDef) = {
    def genBodyItem(item: SBodyItem): SBodyItem = item match {
      case m: SMethodDef => genImplicitMethodArgs(module, m)
      case _ => item
    }
    def genCompanion(companion: Option[SEntityDef]) = companion match {
      case Some(t : STraitDef) => Some(t.copy(body = t.body.map(genBodyItem)))
      case Some(c : SClassDef) => Some(c.copy(body = c.body.map(genBodyItem)))
      case Some(unsupported) => throw new NotImplementedError(s"genCompanion: $unsupported")
      case None => None
    }
    def genEntity(entity: STraitDef): STraitDef = {
      val newBodyItems = entity.body.map(genBodyItem)
      entity.copy(body = newBodyItems, companion = genCompanion(entity.companion))
    }
    def genEntities(entities: List[STraitDef]): List[STraitDef] = {
      entities.map(genEntity)
    }
    def genClass(clazz: SClassDef): SClassDef = {
      val newBodyItems = clazz.body.map(genBodyItem)
      clazz.copy(body = newBodyItems, companion = genCompanion(clazz.companion))
    }
    def genClasses(classes: List[SClassDef]): List[SClassDef] = {
      classes.map(genClass)
    }

    module.copy(
      traits = genEntities(module.traits),
      classes = genClasses(module.classes)
    )
  }

  def fixEntityCompanionName(module: SUnitDef) = {
    class ECompanionTransformer extends AstTransformer {
      override def applyTransform(apply: SApply): SApply = {
        apply match {
          case SApply(sel @ SSelect(SThis(clazz,_),_,_),_,_,_) if context.isEntity(clazz) =>
            apply.copy(fun = sel.copy(expr = SThis(clazz + "Companion")))
          case _ => super.applyTransform(apply)
        }
      }
    }
    new ECompanionTransformer().moduleTransform(module)
  }

  def fixEvidences(module: SUnitDef) = {
    new AstTransformer {
      def implicitElem(tpeSExprs: List[STpeExpr]) = {
        SExprApply(
          SSelect(
            SIdent("Predef"),
            "implicitly"
          ),
          tpeSExprs map (tpe => STraitCall("Elem", List(tpe)))
        )
      }

      override def identTransform(ident: SIdent): SExpr = ident match {
        case SIdent(name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
          super.exprApplyTransform(implicitElem(targs))
        case _ => super.identTransform(ident)
      }
      override def selectTransform(select: SSelect): SExpr = select match {
        case SSelect(_, name, Some(STraitCall("ClassTag", targs))) if name.startsWith("evidence$") =>
          super.exprApplyTransform(implicitElem(targs))
        case _ => super.selectTransform(select)
      }
    }.moduleTransform(module)
  }

  /** Converts constructors (methods with name "<init>") to the apply method of companions. */
  def filterConstructor(module: SUnitDef): SUnitDef = {
    new AstTransformer {
      override def bodyTransform(body: List[SBodyItem]): List[SBodyItem] = body.filter {
        case m: SMethodDef if m.name == "<init>" => false
        case _ => true
      }
    }.moduleTransform(module)
  }

  def constr2apply(module: SUnitDef): SUnitDef = module.updateFirstEntity { e =>
    val (constrs, entityBody) = e.body partition {
      case m: SMethodDef if m.name == "<init>" => true
      case _ => false
    }
    val applies = constrs collect {
      case c: SMethodDef => c.copy(
        name = "apply",
        tpeArgs = (e.tpeArgs ++ c.tpeArgs).distinct,
        // This is an internal annotation. And it should be ignored during in the backend.
        annotations = List(SMethodAnnotation("Constructor", List(SAssign(SIdent("original"), c))))
      )
    }
    val newEntityCompanion = e.companion match {
      case Some(companion: STraitDef) => Some(companion.copy(body = applies ++ companion.body))
      case other => other
    }
    e.copy(body = entityBody, companion = newEntityCompanion)
  }


  /** Discards all ancestors of the entity except TypeWrapperDef. It could be used as temporary solution
    * if inheritance of type wrappers is not supported. */
  def filterAncestors(module: SUnitDef): SUnitDef = {
    class filterAncestorTransformer extends AstTransformer {
      override def entityAncestorsTransform(ancestors: List[STypeApply]): List[STypeApply] = {
        ancestors.filter(_.tpe.isDef)
      }
    }

    new filterAncestorTransformer().moduleTransform(module)
  }

  /** Adds a prefix for type parameters To, Elem and Cont, to eliminate name conflicts. */
  def preventNameConflict(module: SUnitDef): SUnitDef = {
    val pipeline = scala.Function.chain(Seq(
      new TypeNameTransformer("Elem", module.name + "Elem").moduleTransform _,
      new TypeNameTransformer("Cont", module.name + "Cont").moduleTransform _,
      new TypeNameTransformer("To", module.name + "To").moduleTransform _
    ))
    val nonConflictModule = pipeline(module)
    nonConflictModule
  }

  def unrepAllTypes(module: SUnitDef): SUnitDef = {
    val t = new TypeTransformerInAst(new RepTypeRemover())
    t.moduleTransform(module)
  }

  /** Imports scalan._ and other packages needed by Scalan and further transformations. */
  def addImports(unit: SUnitDef) = {
    val usersImport = unit.imports.collect{
      case imp @ SImportStat("scalan.compilation.KernelTypes._") => imp
    }
    unit.copy(imports = SImportStat("scalan._") :: (usersImport))
  }

  def addModuleTrait(unit: SUnitDef) = {
    if (unit.origModuleTrait.isEmpty) {
      val mainName = unit.name
      val mt = STraitDef(
        name = SUnitDef.moduleTraitName(mainName),
        tpeArgs = Nil,
        ancestors = List(STraitCall(s"impl.${mainName}Defs"), STraitCall("scala.wrappers.WrappersModule")).map(STypeApply(_)),
        body = Nil, selfType = None, companion = None)
      unit.copy(origModuleTrait = Some(mt))
    }
    else unit
  }

}

class ModuleVirtualizationPipeline(implicit val context: AstContext) extends (SUnitDef => SUnitDef) {
  val moduleBuilder = new SModuleBuilder()
  import moduleBuilder._

  private val chain = scala.Function.chain(Seq(
    fixExistentialType _,
    tupleToPair _,
    externalTypeToWrapper _,
    //      composeParentWithExt _,
    addBaseToAncestors _,
    addDefAncestorToAllEntities _,
    updateSelf _,
    //      addEntityRepSynonym _,
    addImports _,
    checkEntityCompanion _,
    checkClassCompanion _,
    removeClassTagsFromSignatures _,
//    replaceClassTagByElem _,
    eliminateClassTagApply _,
    genEntityImplicits _,
    // genClassesImplicits _, genMethodsImplicits _,
    fixEntityCompanionName _,
    fixEvidences _
//    optimizeModuleImplicits _
  ))
  override def apply(module: Module): Module = chain(module)
}
