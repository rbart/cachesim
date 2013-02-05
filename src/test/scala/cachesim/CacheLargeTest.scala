package cachesim

import scala.io.Source

object CacheLargeTest {

  def main(args: Array[String]): Unit = {
    
    val testData = Source.fromFile("/scratch/cse548/bzip2.trace")
    
    val spec3 = new CacheSpec(15, 0, 6, 2, writeBack=true)
    val cache3 = new CacheImpl(spec3, new MainMem(spec3))
    
    val spec2 = new CacheSpec(13, 0, 6, 1, writeBack=true)
    val cache2 = new CacheImpl(spec2, cache3)
    
    val spec1 = new CacheSpec(11, 0, 3, 1, writeBack=true)
    val cache1 = new CacheImpl(spec1, cache2)
    
    
    println(spec1)
    println(spec2)
    println(spec3)
    
    val testInputs = testData.getLines map MemOp.deserializeFromString
    
    val aggr = new ResultAggregator(cache1)
    
    aggr.runOps(testInputs)
    
    val agResults = aggr.getResult
    
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