package cachesim

import java.util.BitSet

class Result(
  val time: Long,
  val block: Block,
  val nextOpt: Option[Result]) {
  
  def hit = nextOpt.isEmpty
  
  def this(time: Long, block: Block, next: Result) = this(time, block, Some(next))
  
  private def statString:String = {
    val t = if (nextOpt.isDefined) "MISS" else "HIT"
    
    (Seq("%s:%d".format(t, time)) ++ nextOpt.map(_.statString)).mkString(" -> ")
  }
  
  override def toString: String = {
    
    val t = if (nextOpt.isDefined) "MISS" else "HIT"
    
    "%s\t%s".format(block.addr.toLongArray().map(_.toHexString).mkString, statString)
      
  }
  
}
