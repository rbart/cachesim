package cachesim

object ResultAggregator {

  def aggregate(results: Iterator[Result]): IndexedSeq[AggregateResult] = {
  
    var agResults = IndexedSeq.empty[AggregateResult]
    
    results foreach { headResult =>
      var currentResult: Option[Result] = Some(headResult)
      var level = 0
      while (currentResult.isDefined) {
        if (agResults.size <= level) agResults ++= Seq(new AggregateResult("L%d".format(level+1)))
        val currentAgResult = agResults(level)
        currentAgResult.addResult(currentResult.get)
        level += 1
        currentResult = currentResult.get.nextOpt
      }
    }
    
    agResults
  }
}