package cachesim

import java.util.BitSet

import scala.collection.mutable

class CacheSet {

  private val blocks = new mutable.HashMap[BitSet, Block] 
  
  def getBlock(blockBits: BitSet): Block = {
    
    blocks.getOrElseUpdate(blockBits, new Block(new BitSet(), false))
  }
}