package object scalan {

  /** Can be used in definition of methods, when implementation of the method
    * cannot be done directly and should be provided by compiler based on given
    * class and method names.
    * */
  def externalMethod(className: String, methodName: String) = {
    throw new ExternalMethodException(className, methodName)
  }

}
