package scalan.meta

import scala.tools.nsc.Global
import scalan.BaseNestedTests
import scalan.meta.ScalanAst.{SUnitDef, STpeExpr, SClassDef, STraitDef, SMethodDef}
import scalan.meta.Symbols.SSymbol

trait BaseMetaTests extends BaseNestedTests with ScalanParsersEx[Global] {
  def getGlobal = new Global(settings, reporter)
  initCompiler()
  implicit val context = new AstContext(Nil, this)
  context.loadModulesFromFolders()

  import compiler._

  override def parseModule(module: TestModule): SUnitDef = {
    val m = super.parseModule(module)
    m.isVirtualized shouldBe module.isVirt
    m
  }

  def test[A](kind: TreeKind, prog: String, expected: A)(f: Tree => A) {
    it(prog) {
      val tree = parseString(kind, prog)
      val res = f(tree)
      res shouldBe expected
    }
  }

  def testModule(module: TestModule, expected: SUnitDef)(implicit ctx: ParseCtx) {
    test(TopLevel, module.text, expected) { case tree: PackageDef =>
       unitDefFromPackageDef(module.moduleName, tree)
    }
  }

  def testTrait(prog: String, expected: STraitDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: ClassDef => traitDef(expected.owner, tree, Some(tree)) }
  }
  def testSClass(prog: String, expected: SClassDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: ClassDef => classDef(expected.owner, tree, Some(tree)) }
  }

  def testSTpe(owner: SSymbol, prog: String, expected: STpeExpr)(implicit ctx: ParseCtx) {
    test(Type, prog, expected)(tpeExpr(owner, _))
  }
  def testSMethod(owner: SSymbol, prog: String, expected: SMethodDef)(implicit ctx: ParseCtx) {
    test(Member, prog, expected) { case tree: DefDef => methodDef(owner, tree) }
  }

  def getMethod(module: SUnitDef, entityName: String, name: String) = {
    val res = for {
      e <- module.findEntity(entityName)
      m <- e.findMethodInBody(name)
    } yield m
    res.get
  }
  def getVal(module: SUnitDef, entityName: String, name: String) = {
    val res = for {
      e <- module.findEntity(entityName)
      m <- e.findValInBody(name)
    } yield m
    res.get
  }

}
