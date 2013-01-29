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
    
    val decoderTagBits = decoder.tagBits(addr)
    
    // if tags equal, hit, else miss
    if (cachedBlock.valid && cachedBlock.tag.equals(decoderTagBits)) {
      Hit(-1, cachedBlock)
    } else {
      cachedBlock.tag = decoderTagBits
      cachedBlock.valid = true
      if (nextCache.isDefined) {
        val nextResult = nextCache.get.read(addr)
        Miss(-2, cachedBlock, Some(nextResult))
      } else {
        Miss(-3, new Block(decoder.tagBits(addr), true), None)
      }
    }
  }
  
  def write(addr: BitSet): Result = {
    null
  }
}