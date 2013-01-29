package cachesim

// Describes how to construct a simple cache (non-layered for now).
case class CacheSpec(
  val setBits: Int,
  val blockOffsetBits: Int,
  val byteOffsetBits: Int)