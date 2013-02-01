package cachesim

import scala.io.Source

object CacheLargeTest {

  def main(args: Array[String]): Unit = {
    
    val testData = Source.fromFile("/scratch/cse548/bzip2.trace")
    
    val spec3 = new CacheSpec(15, 0, 4, 8, writeBack=true, lru=true)
    val cache3 = new CacheImpl(spec3, new MainMem(spec3))
    
    val spec2 = new CacheSpec(12, 0, 4, 8, writeBack=true, lru=true)
    val cache2 = new CacheImpl(spec2, cache3)
    
    val spec1 = new CacheSpec(12, 0, 3, 2, writeBack=true, lru=true)
    val cache1 = new CacheImpl(spec1, cache2)
    
    
    println(spec1)
    println(spec2)
    println(spec3)
    
    val testInputs = testData.getLines map MemOp.deserializeFromString
    
    val results = testInputs map { op => cache1.perform(op) }
    
    val agResults = ResultAggregator.aggregate(results)
    
    println(AggregateResult.columns)
    println(agResults.mkString("\t"))
  }
  
  def insertCommas(str: String): String = 
    {
        if(str.length() < 4){
            return str;
        }
        return insertCommas(str.substring(0, str.length() - 3)) + "," + str.substring(str.length() - 3, str.length());
    }
}