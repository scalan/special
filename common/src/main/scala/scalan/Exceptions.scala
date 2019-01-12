package scalan

/**
  * Can be thrown to prevent invoke
  */
class DelayInvokeException extends Exception {
  override def fillInStackTrace(): Throwable = this  // to avoid spending time on recording stack trace
}

case class ExternalMethodException(className: String, methodName: String) extends DelayInvokeException


