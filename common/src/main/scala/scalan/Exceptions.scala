package scalan

/**
  * Can be thrown to prevent invoke
  */
class DelayInvokeException extends Exception

case class ExternalMethodException(className: String, methodName: String) extends DelayInvokeException


