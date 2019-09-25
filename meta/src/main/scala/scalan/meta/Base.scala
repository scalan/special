package scalan.meta

import java.util.Properties
import java.io.FileReader
import java.util

object Base {
  lazy val config = {
    val prop = new Properties
    try {
      val reader = new FileReader("scalan.meta.properties")
      try {
        prop.load(reader)
      } finally {
        reader.close()
      }
    } catch {
      case _: Throwable => {}
    }
    prop.asInstanceOf[util.Hashtable[Object,Object]].putAll(System.getProperties/*.asInstanceOf[util.Hashtable[Object, Object]]*/)
    prop
  }

  def !!!(msg: String) = {
    throw new IllegalStateException(msg)
  }
}