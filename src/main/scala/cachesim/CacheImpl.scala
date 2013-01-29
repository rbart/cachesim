package cachesim

import java.util.BitSet

import scala.collection.mutable

class CacheImpl(val spec: CacheSpec, val nextCache: CacheInterface) extends CacheInterface {
  
  private val cacheSetMap = new mutable.HashMap[BitSet, CacheSet]
  
  def decoder = spec.decoder
  
  def read(addr: BitSet): Result = {
    val setBits = decoder.setBits(addr)
    cacheSetMap.getOrElseUpdate(setBits, new CacheSet(spec, nextCache)).read(addr)
  }
  
  def write(addr: BitSet): Result = {
    val setBits = decoder.setBits(addr)
    cacheSetMap.getOrElseUpdate(setBits, new CacheSet(spec, nextCache)).write(addr)
  }
}