package cachesim

import java.lang.{ Long => JLong }
import java.util.BitSet

import scala.collection.JavaConversions._

// A memory access, e.g. a read or a write. Lines of the pin trace files
// translate one-to-one into these objects
sealed abstract class MemOp {
  def instructionAddress: BitSet
  def memoryAddress: BitSet
}

object MemOp {
  
  private val wsSplit = "\\s+".r
  
  
  // 0x362fa01533: W 0x7fff59af4458
  def deserializeFromString(str: String): MemOp = {
    
    // split on whitespace
    val split = wsSplit.split(str)
    
    split match {
      case Array(iAddrStr, rw, mAddrStr, _*) => {
        // iAddrStr will have a trailing colon...
        val iAddrLong = JLong.decode(iAddrStr.dropRight(1)).toLong
        val mAddrLong = JLong.decode(mAddrStr).toLong
        
        val iAddr = BitSet.valueOf(Array(iAddrLong))
        val mAddr = BitSet.valueOf(Array(mAddrLong))
        if (rw.equals("R")) new Read(iAddr, mAddr)
        else if (rw.equals("W")) new Write(iAddr, mAddr)
        else throw new RuntimeException("couldn't parse %s".format(str))
      }
      case _ => throw new RuntimeException("couldn't parse %s".format(str))
    }
    
  }
  
}

case class Read(instructionAddress: BitSet, memoryAddress: BitSet) extends MemOp

case class Write(instructionAddress: BitSet, memoryAddress: BitSet) extends MemOp

