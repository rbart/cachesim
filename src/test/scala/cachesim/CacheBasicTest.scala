package cachesim

import scala.io.Source

object CacheBasicTest {

  
  def main(args: Array[String]): Unit = {
    
    val testData = Source.fromFile("src/main/resources/basic.trace")
    
    val spec1 = new CacheSpec(2, 2, 2, 2, writeBack=false)
    
    println(spec1)
    
    val cache = new CacheImpl(spec1, new MainMem(spec1))
    
    val testInputs = testData.getLines map MemOp.deserializeFromString
    
    testInputs map { op => cache.perform(op) } foreach println
    
  }
  
}