package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  // Abs -----------------------------------
  trait MonoidInstancesDefs extends scalan.Scalan with MonoidInstances {
    self: Library =>
    import Monoid._
    import MonoidBuilder._
    import MonoidBuilderInst._

    object MonoidBuilderInst extends EntityObject("MonoidBuilderInst") {
      case class MonoidBuilderInstCtor
      ()
          extends MonoidBuilderInst() with Def[MonoidBuilderInst] {
        lazy val resultType = ??? ///element[MonoidBuilderInst]
        override def transform(t: Transformer) = MonoidBuilderInstCtor()
      }
      // elem for concrete class
      class MonoidBuilderInstElem
          extends MonoidBuilderElem[MonoidBuilderInst]
              with ConcreteElem[MonoidBuilderInstData, MonoidBuilderInst] {
        override lazy val parent: Option[Elem[_]] = Some(monoidBuilderElement)
      }

      // state representation type
      type MonoidBuilderInstData = Unit

      // 4) constructor and deconstructor
      class MonoidBuilderInstCompanionCtor extends CompanionDef[MonoidBuilderInstCompanionCtor] with MonoidBuilderInstCompanion {
        def resultType = MonoidBuilderInstCompanionElem
        override def toString = "MonoidBuilderInstCompanion"
        @scalan.OverloadId("fromData")
        def apply(p: Ref[MonoidBuilderInstData]): Ref[MonoidBuilderInst] = {
         ???
        }

        @scalan.OverloadId("fromFields")
        def apply(): Ref[MonoidBuilderInst] =
          mkMonoidBuilderInst()

        def unapply(p: Ref[MonoidBuilder]) = unmkMonoidBuilderInst(p)
      }
      lazy val RMonoidBuilderInst: MutableLazy[MonoidBuilderInstCompanionCtor] = MutableLazy(new MonoidBuilderInstCompanionCtor)
      implicit def unrefMonoidBuilderInstCompanion(p: Ref[MonoidBuilderInstCompanionCtor]): MonoidBuilderInstCompanionCtor = {
        if (p.node.isInstanceOf[MonoidBuilderInstCompanionCtor])
          p.node.asInstanceOf[MonoidBuilderInstCompanionCtor]
        else
          unrefDelegate[MonoidBuilderInstCompanionCtor](p)
      }

      implicit case object MonoidBuilderInstCompanionElem extends CompanionElem[MonoidBuilderInstCompanionCtor]

      implicit def unrefMonoidBuilderInst(p: Ref[MonoidBuilderInst]): MonoidBuilderInst = {
        if (p.node.isInstanceOf[MonoidBuilderInst])
          p.node.asInstanceOf[MonoidBuilderInst]
        else
          unrefDelegate[MonoidBuilderInst](p)
      }

      def mkMonoidBuilderInst
          (): Ref[MonoidBuilderInst] = {
        new MonoidBuilderInstCtor()
      }
      def unmkMonoidBuilderInst(p: Ref[MonoidBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
        case _: MonoidBuilderInstElem @unchecked =>
          Some(())
        case _ =>
          None
      }

    } // of object MonoidBuilderInst
    registerEntityObject("MonoidBuilderInst", MonoidBuilderInst)


    registerModule(MonoidInstancesModule)
  }

  object MonoidInstancesModule extends scalan.ModuleInfo("special.collection", "MonoidInstances")
}

trait MonoidInstancesModule extends special.collection.impl.MonoidInstancesDefs {self: Library =>}

