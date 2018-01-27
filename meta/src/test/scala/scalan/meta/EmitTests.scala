package scalan.meta

import scalan.meta.ScalanAst.{SUnitDef, STpeExpr, STpeArg, STpePath, SRangePath, SDomPath, SThunkPath, SEntityPath, STuplePath, SNilPath, STpeStruct, SStructPath}

class EmitTests extends BaseMetaTests with Examples {
  def testPath(unit: SUnitDef, tpeString: String, name: String, expected: STpeExpr => Option[STpePath]): Unit = {
    implicit val ctx = new ParseCtx(unit.isVirtualized)
    val tpe = parseType(unit.unitName, tpeString)

    it(s"find('${tpeString}', '${name}')") {
      assertResult(STpePath.find(tpe, name))(expected(tpe))
    }
  }

  def testStructPath(unit: SUnitDef, tpe: STpeExpr, name: String, expected: Option[STpePath]): Unit = {
    it(s"find(${tpe}, '${name}')") {
      assertResult(STpePath.find(tpe, name))(expected)
    }
  }

  describe("find TpePath") {
    val module = parseModule(reactiveModule)
    val un = module.unitName
    implicit val ctx = new ParseCtx(module.isVirtualized)
    testPath(module, "Int", "A", _ => None)
    testPath(module, "A", "A", _ => Some(SNilPath))
    testPath(module, "A => Int", "A", t => Some(SDomPath(t, SNilPath)))
    testPath(module, "Int => A", "A", t => Some(SRangePath(t, SNilPath)))
    testPath(module, "B => A", "A", t => Some(SRangePath(t, SNilPath)))

    testPath(module, "(A, Int)", "A", t => Some(STuplePath(t, 0, SNilPath)))
    testPath(module, "(Int, A)", "A", t => Some(STuplePath(t, 1, SNilPath)))
    testPath(module, "(B, A)", "A", t => Some(STuplePath(t, 1, SNilPath)))

    testPath(module, "Thunk[A]", "A", t => Some(SThunkPath(t, SNilPath)))
    testPath(module, "Thunk[B]", "A", _ => None)

    val t1 = STpeStruct(List(("a", parseType(un, "A"))))
    testStructPath(module, t1, "A", Some(SStructPath(t1, "a", SNilPath)))
    val t2 = parseType(un, "(A,Int)")
    val t3 = STpeStruct(List(("a", parseType(un, "Int")), ("b", t2)))
    testStructPath(module, t3, "A",
      Some(SStructPath(t3, "b", STuplePath(t2, 0, SNilPath))))

    val entity = module.getEntity("Observable")
    testPath(module, "Observable[Int]", "A", _ => None)
    testPath(module, "Observable[A]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SNilPath)))

    {
      val t1 = parseType(un, "A => Int")
      testPath(module, "Observable[A => Int]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SDomPath(t1, SNilPath))))
    }
    {
      val t1 = parseType(un, "Int => A")
      testPath(module, "Observable[Int => A]", "A", t => Some(SEntityPath(t, entity, STpeArg("A"), SRangePath(t1, SNilPath))))
    }
    {
      val t1 = parseType(un, "Observable[A]")
      testPath(module, "Observable[Observable[A]]", "A",
        t => Some(SEntityPath(t, entity, STpeArg("A"), SEntityPath(t1, entity, STpeArg("A"), SNilPath))))
    }
  }

  def makePath(module: SUnitDef, tpeString: String, name: String): STpePath = {
    implicit val ctx = new ParseCtx(module.isVirtualized)
    val tpe = parseType(module.unitName, tpeString)
    STpePath.find(tpe, name).get
  }
  def makePath(module: SUnitDef, tpe: STpeExpr, name: String): STpePath = {
    STpePath.find(tpe, name).get
  }

  import ScalanCodegen._

  def testEmit(module: SUnitDef, inType: String, name: String, path: STpePath, expected: String): Unit = {
    val prefix = "x.elem"
    val code = emitImplicitElemDeclByTpePath(prefix, path)
    it(s"emit($inType, $name)") {
      assertResult(code)(prefix + expected)
    }
  }

  def testEmit(module: SUnitDef, inType: String, name: String, expected: String): Unit = {
    val path = makePath(module, inType, name)
    testEmit(module, inType, name, path, expected)
  }

  def testEmit(module: SUnitDef, inType: STpeExpr, name: String, expected: String): Unit = {
    val path = makePath(module, inType, name)
    testEmit(module, inType.toString, name, path, expected)
  }

  describe("emitImplicitElemDeclByTpePath") {
    val module = parseModule(reactiveModule)
    context.addModule(module)
    val un = module.unitName
    implicit val ctx = new ParseCtx(module.isVirtualized)
    testEmit(module, "A", "A", "")
    testEmit(module, "A => Int", "A", ".eDom")
    testEmit(module, "Int => A", "A", ".eRange")
    testEmit(module, "(A, Int)", "A", ".eFst")
    testEmit(module, "(Int, A)", "A", ".eSnd")
    testEmit(module, "(Int, A) => Int", "A", ".eDom.eSnd")
    testEmit(module, "Int => (Int, A)", "A", ".eRange.eSnd")

    val tStruct = STpeStruct(List(("a", parseType(un, "Int")), ("b", parseType(un, "(A,Int)"))))
    testEmit(module, tStruct, "A", """.asInstanceOf[StructElem[_]]("b").asInstanceOf[PairElem[_,_]].eFst""")

    testEmit(module, "Observable[A]", "A", """.typeArgs("A")._1.asElem[A]""")
    testEmit(module, "Observable[Observable[A]]", "A",
      """.typeArgs("A")._1.asElem[Observable[A]].typeArgs("A")._1.asElem[A]""")
  }
}
