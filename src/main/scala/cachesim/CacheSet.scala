package cachesim

import java.util.BitSet

import scala.collection.mutable

class CacheSet(spec: CacheSpec, nextCache: CacheInterface) {

  private val blockMap = new mutable.HashMap[BitSet, Block] 
  
  def read(addr: BitSet): Result = getBlock(addr)
  
  def write(addr: BitSet): Result = {
    // depends on write-through or write-back
    if (!spec.writeBack) {
      // write through to next cache now. 
      val writeResult = nextCache.write(addr)
      // set it in our cache too.
      setBlock(addr) match {
        case (block, Some(victim)) => new Result(spec.accessTime + victim.time + writeResult.time, block, writeResult)
        case (block, None) => new Result(spec.accessTime + writeResult.time, block, writeResult)
      }
    }
    else {
      // write-back: just set the dirty bit. 
      val result = getBlock(addr)
      result.block.dirty = true
      result
    }
  }
  
  /**
   * Get the block, fetch on miss if necessary, evict on miss if necessary.
   */
  private def getBlock(addr: BitSet): Result = {
    
    val tag = spec.decoder.tagBits(addr) 
    
    blockMap.get(tag) match {
      case Some(block) => {
        // Return a hit containing this block
        new Result(spec.accessTime, block, None)
      }
      case None => {
        // call next cache in order to retrieve it
        val missedResult = nextCache.read(addr)
        // call setBlock to store it here. In case of eviction, account for any write-back time.
        setBlock(addr) match {
          case (block, Some(victim)) => new Result(victim.time + spec.accessTime + missedResult.time, block, missedResult)
          case (block, None) => new Result(spec.accessTime + missedResult.time, block, missedResult)
        }
      }
    }
  }
  
  /**
   * ensures that block is stored in this set, performing eviction
   * if necessary. The client must figure out how to get block,
   * e.g. from calling the next cache or main memory.
   * Returns optionally any evicted block. 
   */
  private def setBlock(addr: BitSet): (Block, Option[Result]) = {
    
    // if blockMap is larger than the number of ways,
    // then somebody has to go
    
    val evicted = if (blockMap.size == spec.numWays) Some(evict()) else None
    
    // now we're guaranteed that there is room for the new block
    require(blockMap.size < spec.numWays)
    
    val newBlock = new Block(valid = true, dirty = false, addr, spec.decoder)
    
    blockMap.put(newBlock.tag, newBlock)
    
    (newBlock, evicted)
  }

  /**
   * Remove one of the blocks from the blockmap
   * and return wrapped in a Result, which also contains the runnting time
   * of the eviction in the event of a write-back. It is an error to call me
   * if this CacheSet is empty.
   */
  private def evict(): Result = {
    
    if (blockMap.isEmpty) throw new IllegalStateException("evict() called on empty CacheSet, implementation error")
    
    // default implementation: evict the first block
    val (victimTag, victimBlock) = blockMap.head
    blockMap.remove(victimTag)
    
    // on write-back, write victim to the next cache and account for the time taken.
    val evictionTime = 
      if (spec.writeBack && victimBlock.dirty) {
        val evictResult = nextCache.write(victimBlock.addr)
        evictResult.time
      }
      else 0
    
    // neither hit nor miss actually
    return new Result(evictionTime, victimBlock, None)
  }
}