package scalan.meta

import scalan.{FunctorType, ContainerType}
import scalan.meta.ScalanAst.{STraitCall, STpeExpr, STpeFunc, WrapperConf, TpeString, WrapperDescr, TpeInt}

class AstContextTests extends BaseMetaTests with Examples {

  describe("AstContext methods") {
    val m = parseModule(reactiveModule)
    context.addUnit(m)
    val cols = parseModule(colsVirtModule)
    context.addUnit(cols)
    val warrays = parseModule(warraysModule)
    val arrayConf = WrapperConf("",
      packageName = "scala",
      name = "Array",
      annotations = List(classOf[ContainerType], classOf[FunctorType]).map(_.getSimpleName)
    )
    context.updateWrapper("Array", WrapperDescr(warrays, arrayConf))
    val itersApi = parseModule(itersApiModule)
    context.addUnit(itersApi)
    val itersImpl = parseModule(itersImplModule)
    context.addUnit(itersImpl)

    it("recognize type synonym") {
      def test(t: STpeExpr): Unit = {
        context.TypeDef.unapply(t) should matchPattern { case Some(_) => }
      }
      test(STraitCall("Obs", List(TpeInt)))
      test(STraitCall("Col", List(TpeString)))
      test(STraitCall("RepWArray", List(TpeString)))
      context.TypeDef.unapply(STraitCall("RepIter", List(TpeString))) should matchPattern { case None => }
    }

    it("recognize Ref type") {
      def test(t: STpeExpr, expected: Option[STpeExpr]): Unit = {
        context.RepTypeOf.unapply(t) should be(expected)
      }
      test(TpeInt, None)
      test(STraitCall("Elem", List(TpeInt)), None)
      test(STraitCall("Ref", List(TpeInt)), Some(TpeInt))
      test(STraitCall("RFunc", List(TpeInt, TpeString)), Some(STpeFunc(TpeInt, TpeString)))
    }

    it("resolve Entity by name") {
      List("Observable", "Collection", "WArray",
        "Iter", "IterBuilder", "IterOverArray", "IterOverArrayBuilder") foreach { en =>
        en should matchPattern { case context.Entity(_, e) if e.name == en => }
      }
    }

    it("resolve recognize wrapper entity by name") {
      "WArray" should matchPattern { case context.WrapperEntity(_, e, "Array") if e.name == "WArray" => }
      "Collection" shouldNot matchPattern { case context.WrapperEntity(_, e, _) => }
    }

    it("resolve recognize external type by name") {
      "Array" should matchPattern { case context.ExternalType(u, e) if e.name == "WArray" && u.name == "WArrays" => }
    }
  }

}
