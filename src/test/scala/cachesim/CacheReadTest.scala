package cachesim

import scala.io.Source

object CacheReadTest {

  
  def main(args: Array[String]): Unit = {
    
    val testData = Source.fromFile("src/main/resources/basic.trace")
    
    val spec1 = new CacheSpec(8, 4, 4)
    val decoder1 = new AddressDecoder(spec1)
    
    val cache = new WriteThroughCache(decoder1, None)
    
    val testInputs = testData.getLines.take(10) map MemOp.deserializeFromString
    
    testInputs map { op => cache.read(op.memoryAddress) } foreach println
    
  }
  
}