package cachesim

import java.util.BitSet

import scala.math._

// Describes how to construct a simple cache (non-layered for now).
case class CacheSpec(
  val setBits: Int,
  val blockOffsetBits: Int,
  val byteOffsetBits: Int,
  val numWays: Int,
  val writeBack: Boolean,
  val lru: Boolean) {
  
  def accessTime = {
    val cacheSizeKB = cacheDataBytes.toDouble / 1024.0
    ceil(sqrt((cacheSizeKB / 8.0) * (numWays.toDouble / 2.0))).toLong
  }
    
  private val cacheDataBytes = (pow(2, blockOffsetBits + byteOffsetBits + setBits) * numWays).toInt
  
  private val cacheOverheadBytes = {
    val numSets = pow(2, setBits)
    val numBlocks = numSets * numWays
    val tagBitsPerBlock = 48 - setBits - blockOffsetBits - byteOffsetBits
    val numTagBits = numBlocks * tagBitsPerBlock
    val numValidBits = numBlocks
    val numDirtyBits = if(writeBack) numBlocks else 0
    // add in additional overhead for LRU, etc..
    val lruBits = if (lru) numBlocks * blockOffsetBits else 0
    
    val totalBits = numTagBits + numValidBits + numDirtyBits + lruBits
    val totalBytes = ceil(totalBits.toDouble / 8.0).toInt
    totalBytes
  }
  
  lazy val decoder = new AddressDecoder(this)
  
  override def toString: String = {
    
    val dataKB = cacheDataBytes.toDouble / 1024.0
    
    val dataMB = dataKB / 1024.0
    
    val overheadKB = cacheOverheadBytes.toDouble / 1024.0
    
    val overheadMB = overheadKB / 1024.0
    
    val totalB = cacheDataBytes + cacheOverheadBytes
    val totalKB = dataKB + overheadKB
    val totalMB = dataMB + overheadMB
    
    Seq(
     "CacheSpec:",
     " Num Sets: %d".format(pow(2, setBits).toInt),
     " Block Size (B): %d".format(pow(2, blockOffsetBits + byteOffsetBits).toInt),
     " Associativity: %d way".format(numWays),
     " WriteBack: %s".format(writeBack),
     " Data: %d B, %.1f KB, %.1f MB".format(cacheDataBytes, dataKB, dataMB),
     " Overhead: %d B, %.1f KB, %.1f MB".format(cacheOverheadBytes, overheadKB, overheadMB),
     " Total Size: %d B, %.1f KB, %.1f MB".format(totalB, totalKB, totalMB),
     " Delay (cyc): %d".format(accessTime)
    ).mkString("\n")
  }
}