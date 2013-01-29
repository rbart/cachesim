package cachesim

import java.util.BitSet

class AddressDecoder(val spec: CacheSpec) {

  def byteOffsetBits(addr: BitSet): BitSet = {
    // the byte offset is bytes 0 through log(block size)-1
    
    addr.get(0, spec.byteOffsetBits)
    
  }
  
  def blockOffsetBits(addr: BitSet): BitSet = {
    
    addr.get(spec.byteOffsetBits, spec.byteOffsetBits + spec.blockOffsetBits)
    
  }
  
  def setBits(addr: BitSet): BitSet = {
    
    addr.get(spec.byteOffsetBits + spec.blockOffsetBits, spec.byteOffsetBits + spec.blockOffsetBits + spec.setBits)
  }
  
  def tagBits(addr: BitSet): BitSet = {
    
    addr.get(spec.byteOffsetBits + spec.blockOffsetBits + spec.setBits, addr.length())
  }
  
}