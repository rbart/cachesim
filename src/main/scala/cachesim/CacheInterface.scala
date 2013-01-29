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

object MainMem extends CacheInterface {
  
}