package cachesim

import scala.io.Source

object CacheTwoLevelTest {

  
  def main(args: Array[String]): Unit = {
    
    val testData = Source.fromFile("src/main/resources/twolevel-test.trace")
    
    val spec2 = new CacheSpec(3, 3, 3, 4, writeBack=false)
    
    val cache2 = new CacheImpl(spec2, new MainMem(spec2))
    
    val spec1 = new CacheSpec(2, 2, 2, 2, writeBack=false)
    
    val cache1 = new CacheImpl(spec1, cache2)
    
    
    println(spec1)
    println(spec2)
    
    val testInputs = testData.getLines map MemOp.deserializeFromString
    
    testInputs map { op => cache1.perform(op) } foreach println
  }
}