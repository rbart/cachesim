package cachesim

import java.util.BitSet

abstract class CacheInterface {

  def decoder: AddressDecoder
  
  def read(addr: BitSet): Result
  
  def write(addr: BitSet): Result
  
}