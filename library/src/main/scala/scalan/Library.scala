package scalan

import java.lang.reflect.Method

import library.WrappersSpecModule

import scala.wrappers.WrappersModule
import scalan.collection.{CostsModule, ConcreteCostsModule, ColsModule, MonoidsModule, ColsOverArraysModule, MonoidInstancesModule}

trait Library extends Scalan
  with WrappersModule
  with WrappersSpecModule
  with ColsModule
  with ColsOverArraysModule
  with CostsModule
  with ConcreteCostsModule
  with MonoidsModule
  with MonoidInstancesModule {

  override protected def getResultElem(receiver: Sym, m: Method, args: List[AnyRef]): Elem[_] = receiver.elem match {
    case ae: WArrayElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        wArrayElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case ce: ColElem[a, _] => m.getName match {
      case "map" =>
        val f = args(0).asInstanceOf[Rep[a => Any]]
        colElement(f.elem.eRange)
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }

}
