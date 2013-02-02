package cachesim

/**
 * Runs multiple experiments in parallel
 */
object Main {

  def main(args: Array[String]): Unit = {
    
    
    val inputFile = args(0)
    
    val output = if (args.length > 1) new java.io.PrintStream(args(1)) else System.out
    
    val numLevels = 2
    val setBits = Seq(11, 12, 13, 14, 15)
    val assocs = Seq(1, 2, 4, 8, 16, 32)
    val blockBits = Seq(3, 4, 5, 6)
    val writePolicies = Seq(true) //, false)
    
    val allIndividualCaches: List[CacheSpec] = {
      val notList = for (setB <- setBits; assoc <- assocs; blockB <- blockBits; writeBack <- writePolicies) yield {
        new CacheSpec(setB, 0, blockB, assoc, writeBack)
      } 
      notList.toList
    }

    println(allIndividualCaches.size)
    
    val allCacheSpecs: List[List[CacheSpec]] = {
      val multiplied = List.fill(numLevels)(allIndividualCaches)
      val perms = cartesianProduct(multiplied) 
      perms filter filterCacheSpecs
    }
    
    println("Running %d different configurations...".format(allCacheSpecs.size))
    val allCaches = allCacheSpecs map getCombinedCache
    var numFinished = 0
    allCaches.par.iterator foreach { cache =>
      val inputSource = scala.io.Source.fromFile(inputFile)
      val results = inputSource.getLines map MemOp.deserializeFromString map cache.perform
      val agResult = ResultAggregator.aggregate(results)
      val resultString = (Seq(agResult.head.cyclesPerOp.formatted("%.2f"), agResult.head.missRate.formatted("%.2f")) ++ getSpecs(cache).dropRight(1).map(_.paramString)).mkString("\t")
      output.println(resultString)
      numFinished += 1
      if (numFinished % 100 == 0) System.err.println("Finished %d".format(numFinished))
      inputSource.close()
    }    
  }

  def cartesianProduct[T](xss: List[List[T]]): List[List[T]] = xss match {
    case Nil => List(Nil)
    case h :: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh :: xt
  }
  
  def filterCacheSpecs(cacheSpecs: Iterable[CacheSpec]): Boolean = {
    var lastSets = Int.MaxValue
    var lastBlocks = Int.MaxValue
    var lastSize = Int.MaxValue
    var lastAssoc = Int.MaxValue
    var lastDelay = Long.MaxValue
    var totalSize = 0
    for (spec <- cacheSpecs) {
      if (spec.cacheDataBytes < 8192) return false // not smaller than 8KB
      else if (spec.cacheDataBytes >= 7340032) return false // not bigger than 7MB
      else if (spec.cacheDataBytes >= lastSize) return false
      else if (spec.blockOffsetBits + spec.byteOffsetBits > lastBlocks) return false
      else if (spec.numWays > lastAssoc) return false
      else if (spec.setBits > lastSets) return false
      else if (spec.accessTime >= lastDelay) return false
      else {
        lastBlocks = spec.blockOffsetBits + spec.byteOffsetBits 
        lastSize = spec.cacheDataBytes
        lastSets = spec.setBits
        lastAssoc = spec.numWays
        lastDelay = spec.accessTime
        totalSize += spec.cacheDataBytes + spec.cacheOverheadBytes
      }
    }
    if (totalSize > 8388608) return false
    
    true
  }
  
  // first specs are considered the higher cache levels
  def getCombinedCache(cacheSpecs: Iterable[CacheSpec]): CacheImpl = {
    var nextCache: CacheInterface = new MainMem(cacheSpecs.head.decoder.spec)
    var curCache = new CacheImpl(cacheSpecs.head.decoder.spec, nextCache)
    
    for (spec <- cacheSpecs.drop(1)) {
      nextCache = curCache
      curCache = new CacheImpl(spec, nextCache)
    }
    curCache
  }
  
  def getSpecs(cache: CacheInterface): Seq[CacheSpec] = {
    if (cache.nextCacheOpt.isDefined) Seq(cache.spec) ++ getSpecs(cache.nextCacheOpt.get)
    else Seq(cache.spec)
  }
  
}