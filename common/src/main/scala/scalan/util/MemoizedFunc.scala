package scalan.util

import scalan.AVHashMap

class MemoizedFunc(f: AnyRef => AnyRef) {
  private var _table: AVHashMap[AnyRef, AnyRef] = AVHashMap(100)
  def apply[T <: AnyRef](x: T): AnyRef = {
    var v = _table(x)
    if (v == null) {
      v = f(x)
      _table.put(x, v)
    }
    v
  }
  def reset() = {
    _table = AVHashMap(100)
  }
}


