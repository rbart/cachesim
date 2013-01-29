package cachesim

import java.util.BitSet

// Describes how to construct a simple cache (non-layered for now).
case class CacheSpec(
  val setBits: Int,
  val blockOffsetBits: Int,
  val byteOffsetBits: Int,
  val numWays: Int,
  val writeBack: Boolean) {
  
  def accessTime = -52
  
  lazy val decoder = new AddressDecoder(this)
}