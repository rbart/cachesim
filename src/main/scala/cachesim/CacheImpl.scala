package cachesim

import java.util.BitSet

import scala.collection.mutable

class WriteThroughCache(val decoder: AddressDecoder, val nextCache: Option[CacheInterface]) extends CacheInterface {

  val sets = new mutable.HashMap[BitSet, CacheSet]
  
  def getCachedBlock(addr: BitSet): Block = {
    val setBits = decoder.setBits(addr)
    val blockBits = decoder.blockOffsetBits(addr)
    sets.getOrElseUpdate(setBits, new CacheSet()).getBlock(blockBits)
  }
  
  def read(addr: BitSet): Result = {
    
    val cachedBlock = getCachedBlock(addr)
    
    // if tags equal, hit, else miss
    if (cachedBlock.tag.equals(decoder.tagBits(addr))) {
      Hit(-1, cachedBlock)
    } else {
      if (nextCache.isDefined) {
        val nextResult = nextCache.get.read(addr)
        Miss(-2, nextResult.block, Some(nextResult))
      } else {
        Miss(-3, new Block(decoder.tagBits(addr), true), None)
      }
    }
  }
  
  def write(addr: BitSet): Result = {
    null
  }
}