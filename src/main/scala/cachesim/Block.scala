package cachesim

import java.util.BitSet

class Block(var valid: Boolean, var dirty: Boolean, val addr:BitSet, val decoder: AddressDecoder) {
  
  var lastUse = System.nanoTime()
  
  def tag = decoder.tagBits(addr) 
  
  def use(): Unit = { lastUse = System.nanoTime() }
  
}