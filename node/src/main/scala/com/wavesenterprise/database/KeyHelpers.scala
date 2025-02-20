package com.wavesenterprise.database

import java.nio.ByteBuffer
import com.google.common.primitives.{Ints, Shorts}
import com.wavesenterprise.database.rocksdb.MainDBColumnFamily
import com.wavesenterprise.state.ByteStr

object KeyHelpers {

  def h(prefix: Short, height: Int): Array[Byte] =
    ByteBuffer.allocate(6).putShort(prefix).putInt(height).array()

  def hBytes(prefix: Short, height: Int, bytes: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(6 + bytes.length).putShort(prefix).putInt(height).put(bytes).array()

  def bytes(prefix: Short, bytes: Array[Byte]): Array[Byte] =
    ByteBuffer.allocate(2 + bytes.length).putShort(prefix).put(bytes).array()

  def addr(prefix: Short, addressId: BigInt): Array[Byte] = bytes(prefix, addressId.toByteArray)

  def hash(prefix: Short, hashBytes: ByteStr): Array[Byte] = bytes(prefix, hashBytes.arr)

  def hAddr(prefix: Short, height: Int, addressId: BigInt): Array[Byte] = hBytes(prefix, height, addressId.toByteArray)

  def historyKey(name: String, prefix: Short, b: Array[Byte]): MainDBKey[Seq[Int]] = MainDBKey(name, bytes(prefix, b), readIntSeq, writeIntSeq)

  def historyKey(name: String, columnFamily: MainDBColumnFamily, prefix: Short, b: Array[Byte]): MainDBKey[Seq[Int]] =
    MainDBKey(name, columnFamily, bytes(prefix, b), readIntSeq, writeIntSeq)

  def intKey(name: String, prefix: Short, default: Int = 0): MainDBKey[Int] =
    MainDBKey(name, Shorts.toByteArray(prefix), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def bytesSeqNr(name: String, prefix: Short, b: Array[Byte], default: Int = 0): MainDBKey[Int] =
    MainDBKey(name, bytes(prefix, b), Option(_).fold(default)(Ints.fromByteArray), Ints.toByteArray)

  def unsupported[A](message: String): A => Array[Byte] = _ => throw new UnsupportedOperationException(message)
}
