package cachesim

import java.util.BitSet

class Block(var valid: Boolean, var dirty: Boolean, val addr:BitSet, val decoder: AddressDecoder) {
  def tag = decoder.tagBits(addr) 
}