package scalan

import scalan.compilation.GraphVizExport
import scalan.primitives._
import scalan.staged.{Transforming}

class Scalan
  extends Base
  with TypeDescs
  with Proxy
  with Tuples
  with NumericOps
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with MathOps
  with Equal
  with UniversalOps
  with Functions
  with IfThenElse
  with Transforming
  with GraphVizExport
  with ViewsModule
  with Thunks
  with Entities
  with Structs
  with ConvertersModule
  with Modules
  with DefRewriting

