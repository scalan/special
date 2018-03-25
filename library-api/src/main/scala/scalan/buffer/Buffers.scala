package scalan.buffer

import scalan.{Updater, Iso}

trait Buffer {
  def position: Int
  def limit: Int
  def capacity: Int

  def getByte: Byte
  def getDouble: Double
  def getString: String
  def toArray: Array[Byte]

  @Updater def putByte(b: Byte): Buffer
  @Updater def putDouble(b: Byte): Buffer
  @Updater def putString(b: Byte): Buffer

  @Updater def reset: Buffer
}

trait BufferBuilder {
  def allocate(capacity: Int): Buffer
  def fromArray(arr: Array[Byte]): Buffer
}

class ArrayToBufferIso(b: BufferBuilder) extends Iso[Array[Byte], Buffer] {
  override def to(a: Array[Byte]) = b.fromArray(a)
  override def from(buf: Buffer) = buf.toArray
}
