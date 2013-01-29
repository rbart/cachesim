package cachesim

import java.util.BitSet

class Result(
  val time: Long,
  val block: Block,
  val nextOpt: Option[Result]) {
  
  def hit = nextOpt.isEmpty
  
  def this(time: Long, block: Block, next: Result) = this(time, block, Some(next))
  
}
