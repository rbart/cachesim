package cachesim

class ResultAggregator(val cache: CacheInterface) {

  private val totalResult = new AggregateResult("Total")
  
  private var agResults = IndexedSeq.empty[AggregateResult]
  
  def getResult: Seq[AggregateResult] = agResults 
  
  def runOps(ops: Iterator[MemOp]): Unit = aggregate(ops map cache.perform)
  
  private def aggregate(results: Iterator[Result]): Unit = {
  
    results foreach { headResult =>

      // flatten
      var tempResults = IndexedSeq.empty[Result]
      var currentResult: Option[Result] = Some(headResult)
      var level = 0
      while (currentResult.isDefined && !currentResult.get.mainMem) {
        tempResults ++= Seq(currentResult.get)
        if (agResults.size <= level) agResults ++= Seq(new AggregateResult("L%d".format(level+1)))
        currentResult = currentResult.get.nextOpt
        level += 1
      }
      
      if (tempResults.exists(_.hit)) {
        totalResult.hits += 1
        totalResult.hitCycles += tempResults.head.time
      } else {
        totalResult.misses += 1
        totalResult.missCycles += tempResults.head.time
      }
      
      // iterate
      for ((res, idx) <- tempResults.zipWithIndex) {
        val currentAgResult = agResults(idx)
        currentAgResult.addResult(res)      
      }
    }
    Seq(totalResult) ++ agResults
  }
  
}