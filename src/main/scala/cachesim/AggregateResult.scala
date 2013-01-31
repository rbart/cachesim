package cachesim

class AggregateResult(
  val name: String, // e.g. Overall, L1, L2, L3, etc.
  var hits: Long, // total number of hits
  var misses: Long, // total number of misses
  var hitCycles: Long, // total number of cycles due to hits
  var missCycles: Long // total number of cycles due to misses
) {

  def this(name:String) = this(name, 0l, 0l, 0l, 0l)
  
  def cycles = hitCycles + missCycles
  
  def operations = hits + misses
  
  def missRate = misses.toDouble / (hits.toDouble + misses.toDouble)
  
  def cyclesPerOp = cycles.toDouble / operations.toDouble
  
  def cyclesPerHit = hitCycles.toDouble / hits.toDouble
  
  def cyclesPerMiss = missCycles.toDouble / misses.toDouble
  
  def addResult(result: Result): Unit = {
    
    if (result.hit) {
      hits += 1
      hitCycles += result.time
    } else {
      misses += 1
      missCycles += result.time
    } 
  }
  
  def dataString: String = {
    val fields: Seq[String] = Seq(
      name,
      missRate.formatted("%.3f"),
      cyclesPerOp.formatted("%.1f"),
      cyclesPerHit.toString,
      cyclesPerMiss.toString,
      cycles.toString,
      operations.toString
    )
    fields.mkString("\t")
  }
}

object AggregateResult {
  val columns = Seq(
    "name",
    "miss rate",
    "cyc/op",
    "cyc/hit",
    "cyc/miss",
    "cycles",
    "ops"
  ).mkString("\t")
} 