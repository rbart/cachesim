package cachesim

object ResultAggregator {

  def aggregate(results: Iterator[Result]): Seq[AggregateResult] = {
  
    var agResults = IndexedSeq.empty[AggregateResult]
    
    val totalResult = new AggregateResult("Total")
    
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