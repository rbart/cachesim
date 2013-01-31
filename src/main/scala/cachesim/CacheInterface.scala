package cachesim

import java.util.BitSet

abstract class CacheInterface {

  def spec: CacheSpec
  
  def read(addr: BitSet): Result
  
  def write(addr: BitSet): Result
  
  def perform(op: MemOp): Result = {
    if (op.isInstanceOf[Read]) read(op.memoryAddress)
    else write(op.memoryAddress)
  }
}

class MainMem(val spec: CacheSpec) extends CacheInterface {
  
  val accessTime = 100 + (10 * scala.math.pow(2, spec.blockOffsetBits + spec.byteOffsetBits).toInt)
  
  // grr... somehow we need to know "how many bytes" we're requesting. It's always one word for the input. But when we actually read from
  // memory, we need to fetch the whole block. So we need to know what the block size is here.
  def read(addr: BitSet): Result = {
    val block = new Block(valid = true, dirty = false, addr, spec.decoder)
    new Result(accessTime, block, None)
  }
  
  def write(addr: BitSet): Result = read(addr)
  
}