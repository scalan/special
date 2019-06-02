package scalan.meta

import com.trueaccord.lenses.{ObjectLens, Lens}
import scalan.meta.Symbols.SSymbol
import scalan.meta.ScalanAst.{STpeDef, SUnitDef, STpeExpr, SExpr, STpeArg, SEntityAnnotation, SClassDef, SObjectDef, SSelfTypeDef, STraitDef, SMethodDef, STypeApply, SClassArgs, SMethodArgs, SImportStat, SBodyItem, SEntityDef, SMethodAnnotation}

object AstLenses {
  implicit class SeqLikeLens[U, A, Coll[A] <: collection.SeqLike[A, Coll[A]]](val lens: Lens[U, Coll[A]]) extends AnyVal {
    type CBF = collection.generic.CanBuildFrom[Coll[A], A, Coll[A]]

    private def field(getter: Coll[A] => A)(setter: (Coll[A], A) => Coll[A]): Lens[U, A] =
      lens.compose[A](Lens[Coll[A], A](getter)(setter))

    def find(p: A => Boolean)(implicit cbf: CBF): Lens[U, A] = {
      def getter(coll: Coll[A]): A = {
        val i = coll.indexWhere(p)
        val res = if (i != -1) coll.apply(i) else null.asInstanceOf[A]
        res
      }
      def setter(coll: Coll[A], v: A) = {
        val i = coll.indexWhere(p)
        if (i != -1) coll.updated(i, v) else coll
      }
      field(getter)(setter)
    }
  }

  implicit class SUnitDefLens[U](_l: Lens[U, SUnitDef])(implicit ctx: AstContextBase) extends ObjectLens[U, SUnitDef](_l) {
    def imports: Lens[U, List[SImportStat]] = field(_.imports)((c, f) => c.copy(imports = f))
    def name: Lens[U, String] = field(_.name)((c, f) => c.copy(name = f))
    def typeDefs: Lens[U, List[STpeDef]] = field(_.typeDefs)((c, f) => c.copy(typeDefs = f))
    def traits: Lens[U, List[STraitDef]] = field(_.traits)((c, f) => c.copy(traits = f))
    def classes: Lens[U, List[SClassDef]] = field(_.classes)((c, f) => c.copy(classes = f))
    def methods: Lens[U, List[SMethodDef]] = field(_.methods)((c, f) => c.copy(methods = f))
    def selfType: Lens[U, Option[SSelfTypeDef]] = field(_.selfType)((c, f) => c.copy(selfType = f))
    def ancestors: Lens[U, List[STypeApply]] = field(_.ancestors)((c, f) => c.copy(ancestors = f))
    def isVirtualized: Lens[U, Boolean] = field(_.isVirtualized)((c, f) => c.copy(isVirtualized = f))
  }

  implicit class STraitDefLens[U](_l: Lens[U, STraitDef]) extends ObjectLens[U, STraitDef](_l) {
    def owner: Lens[U, SSymbol] = field(_.owner)((c, f) => c.copy(owner = f))
    def name: Lens[U, String] = field(_.name)((c, f) => c.copy(name = f))
    def tpeArgs: Lens[U, List[STpeArg]] = field(_.tpeArgs)((c, f) => c.copy(tpeArgs = f))
    def ancestors: Lens[U, List[STypeApply]] = field(_.ancestors)((c, f) => c.copy(ancestors = f))
    def body: Lens[U, List[SBodyItem]] = field(_.body)((c, f) => c.copy(body = f))
    def selfType: Lens[U, Option[SSelfTypeDef]] = field(_.selfType)((c, f) => c.copy(selfType = f))
    def companion: Lens[U, Option[SEntityDef]] = field(_.companion)((c, f) => c.copy(companion = f))
    def annotations: Lens[U, List[SEntityAnnotation]] = field(_.annotations)((c, f) => c.copy(annotations = f))
  }

  implicit class SClassDefLens[U](_l: Lens[U, SClassDef]) extends ObjectLens[U, SClassDef](_l) {
    def owner: Lens[U, SSymbol] = field(_.owner)((c, f) => c.copy(owner = f))
    def name: Lens[U, String] = field(_.name)((c, f) => c.copy(name = f))
    def tpeArgs: Lens[U, List[STpeArg]] = field(_.tpeArgs)((c, f) => c.copy(tpeArgs = f))
    def args: Lens[U, SClassArgs] = field(_.args)((c, f) => c.copy(args = f))
    def implicitArgs: Lens[U, SClassArgs] = field(_.implicitArgs)((c, f) => c.copy(implicitArgs = f))
    def ancestors: Lens[U, List[STypeApply]] = field(_.ancestors)((c, f) => c.copy(ancestors = f))
    def body: Lens[U, List[SBodyItem]] = field(_.body)((c, f) => c.copy(body = f))
    def selfType: Lens[U, Option[SSelfTypeDef]] = field(_.selfType)((c, f) => c.copy(selfType = f))
    def companion: Lens[U, Option[SEntityDef]] = field(_.companion)((c, f) => c.copy(companion = f))
    def isAbstract: Lens[U, Boolean] = field(_.isAbstract)((c, f) => c.copy(isAbstract = f))
    def annotations: Lens[U, List[SEntityAnnotation]] = field(_.annotations)((c, f) => c.copy(annotations = f))
  }

  implicit class SObjectDefLens[U](_l: Lens[U, SObjectDef]) extends ObjectLens[U, SObjectDef](_l) {
    def owner: Lens[U, SSymbol] = field(_.owner)((c, f) => c.copy(owner = f))
    def name: Lens[U, String] = field(_.name)((c, f) => c.copy(name = f))
    def ancestors: Lens[U, List[STypeApply]] = field(_.ancestors)((c, f) => c.copy(ancestors = f))
    def body: Lens[U, List[SBodyItem]] = field(_.body)((c, f) => c.copy(body = f))
  }

  implicit class SMethodDefLens[U](_l: Lens[U, SMethodDef]) extends ObjectLens[U, SMethodDef](_l) {
    def owner: Lens[U, SSymbol] = field(_.owner)((c, f) => c.copy(owner = f))
    def name: Lens[U, String] = field(_.name)((c, f) => c.copy(name = f))
    def tpeArgs: Lens[U, List[STpeArg]] = field(_.tpeArgs)((c, f) => c.copy(tpeArgs = f))
    def argSections: Lens[U, List[SMethodArgs]] = field(_.argSections)((c, f) => c.copy(argSections = f))
    def tpeRes: Lens[U, Option[STpeExpr]] = field(_.tpeRes)((c, f) => c.copy(tpeRes = f))
    def isImplicit: Lens[U, Boolean] = field(_.isImplicit)((c, f) => c.copy(isImplicit = f))
    def isOverride: Lens[U, Boolean] = field(_.isOverride)((c, f) => c.copy(isOverride = f))
    def overloadId: Lens[U, Option[String]] = field(_.overloadId)((c, f) => c.copy(overloadId = f))
    def annotations: Lens[U, List[SMethodAnnotation]] = field(_.annotations)((c, f) => c.copy(annotations = f))
    def body: Lens[U, Option[SExpr]] = field(_.body)((c, f) => c.copy(body = f))
    def isTypeDesc: Lens[U, Boolean] = field(_.isTypeDesc)((c, f) => c.copy(isTypeDesc = f))
  }

}
