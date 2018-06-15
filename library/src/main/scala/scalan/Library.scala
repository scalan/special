package scalan

import scala.wrappers.WrappersModule
import scalan.collection.{ColsModule, ColsOverArraysModule, CostsModule, ConcreteCostsModule, MonoidsModule, MonoidInstancesModule}

trait Library extends Scalan
  with WrappersModule
  with ColsModule
  with ColsOverArraysModule
  with CostsModule
  with ConcreteCostsModule
  with MonoidsModule
  with MonoidInstancesModule
