package cachesim

import java.lang.{ Long => JLong }
import java.util.BitSet

object AddressDecoderTest {

  // a simple unit test for the address decoder
  def main(args: Array[String]): Unit = {
    
    val spec1 = new CacheSpec(2, 2, 2, 2, writeBack=false)
    val decoder1 = new AddressDecoder(spec1)
    
    val test1 = "0x7abcde11"
    val test1Long = JLong.decode(test1).toLong
    val test1Bits = BitSet.valueOf(Array(test1Long))
    
    val byteOffset = decoder1.tagBits(test1Bits).toLongArray().headOption
    println(byteOffset)
  }
  
}