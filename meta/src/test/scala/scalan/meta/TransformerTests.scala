package scalan.meta

import scalan.meta.ScalanAst._
import scalan.meta.ScalanAstExtensions._
import scalan.meta.ScalanAstTransformers.{RepTypeRemover, TypeTransformerInAst}
import ScalanAst.NamedDefTraversableOps
import scalan.util.CollectionUtil._

class TransformerTests extends BaseMetaTests with Examples {
  val colsVirt = parseModule(colsVirtModule)
  val warrays = parseModule(warraysModule)
  context.updateWrapper("Array", WrapperDescr(warrays, WrapperConf("", "scala", "Array")))
  val b = new SModuleBuilder

  describe("Rep removing") {
    context.addUnit(colsVirt)
    val trans = new TypeTransformerInAst(new RepTypeRemover())
    def test(m: SUnitDef, typeIn: SUnitDef => STpeExpr): Unit = {
      val before = typeIn(m)
      val newCols = trans.moduleTransform(m)
      val after = typeIn(newCols)
      after should be(context.RepTypeOf.unapply(before).get)
    }
    it("from method result type") {
      test(colsVirt, m => getMethod(m, "Collection", "length").tpeRes.get)
      test(colsVirt, m => getMethod(m, "Collection", "apply").tpeRes.get)
    }
    it("from method arg") {
      test(colsVirt, m => getMethod(m, "Collection", "apply").allArgs(0).tpe)
    }
    it("from class arg") {
      test(colsVirt, m => m.getEntity("ColOverArray").args.args(0).tpe)
    }
    it("from class val") {
      test(colsVirt, m => getVal(m, "ColOverArray", "list").tpe.get)
    }
  }

  describe("Unit transforms") {
    it("allEntitiesSorted") {
      val es = colsVirt.allEntitiesSorted.map(e => e.name)
      es shouldBe(List("Collection", "PairCollection", "ColOverArray", "PairOfCols"))
    }

    it("addDefAncestorToAllEntities doesn't change virtualized unit") {
      val newCols = b.addDefAncestorToAllEntities(colsVirt)
      newCols.allEntitiesSorted shouldBe(colsVirt.allEntitiesSorted)
    }

    it("ModuleVirtualizationPipeline") {
      val cols = parseModule(colsModule)
      val p = new SourceUnitVirtualization
      val virt = p(cols)
      val opt = optimizeUnitImplicits(virt)
      val names = opt.allEntitiesSorted.map(_.name)
      val expectedNames = colsVirt.allEntitiesSorted.map(_.name)
      names shouldBe(expectedNames)
// TODO     opt.allEntitiesSorted shouldBe(colsVirt.allEntitiesSorted)
    }

    it("addDefAncestorToAllEntities") {
      val cols = parseModule(colsModule)
      val res = b.addDefAncestorToAllEntities(cols)
      val as = res.allEntitiesSorted.map(e => e.ancestors)
      as shouldBe colsVirt.allEntitiesSorted.map(e => e.ancestors)
      res.allEntities forall { _.inherits("Def") } shouldBe(true)
    }

    val testsModule = TestModule("Tests",
      """package scalan.collection
       |  class Test[A] {
       |    def m1[B:ClassTag](x: A): A = x
       |    def m2[B](x: A)(implicit cB: ClassTag[B]): A = x
       |  }
      """.stripMargin, false)

    val tests = parseModule(testsModule)

    it("moduleVirtPipeline") {
      val p = new SourceUnitVirtualization
      val virt = p(tests)
      val test = virt.classes.apply("Test")
      val m1 = test.body.filterCast[SMethodDef].apply("m1")
      m1.tpeArgs(0).contextBound.contains("ClassTag") shouldBe true
    }
    it("removeClassTagsFromSignatures") {
       val b = new SModuleBuilder()
       val test = b.replaceClassTagsWithElems(tests).classes("Test")
       val m2 = test.body.filterCast[SMethodDef].apply("m2")
       m2.argSections.find(sec => sec.args.find(a => ElemTpe.unapply(a.tpe).isDefined).isDefined).isDefined shouldBe true
    }
  }
}
