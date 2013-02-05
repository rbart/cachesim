package cachesim

/**
 * Runs multiple experiments in parallel
 */
object Main {

  private val KB = 1024
  
  private val MB = KB*KB
  
  def getCaches(setBits: Seq[Int], assocs: Seq[Int], blockBits: Seq[Int], writePolicies: Seq[Boolean]): List[CacheSpec] = {
    val notList = for (setB <- setBits; assoc <- assocs; blockB <- blockBits; writeBack <- writePolicies) yield {
      new CacheSpec(setB, 0, blockB, assoc, writeBack)
    }
    notList.toList
  }
  
  def main(args: Array[String]): Unit = {
    
    val inputFile = args(0)
    
    val output = if (args.length > 1) new java.io.PrintStream(args(1)) else System.out
    
    val level1Specs = getCaches(
      Seq(11),
      Seq(1),
      Seq(3),
      Seq(true)
    )

    val level2Specs = getCaches(
      Seq(10, 11, 12, 13),
      Seq(1),
      Seq(3, 4, 5, 6),
      Seq(true)
    ).filter(_.cacheDataBytes <= 512*MB)
    
    val level3Specs = getCaches(
      Seq(15, 16),
      Seq(1, 2),
      Seq(6),
      Seq(true)
    )
    
    println("L1s: %d".format(level1Specs.size))
    println("L2s: %d".format(level2Specs.size))
    println("L3s: %d".format(level3Specs.size))
    
    val allCacheSpecs: List[List[CacheSpec]] = {
      val perms = cartesianProduct(List(level3Specs, level2Specs))
      perms filter filterCacheSpecs
    }
    
    println("Running %d different configurations...".format(allCacheSpecs.size))
    val allCaches = allCacheSpecs map getCombinedCache map { cache => new ResultAggregator(cache) }
    var numOpsDone = 0
    
    val inputSource = scala.io.Source.fromFile(inputFile)
    val memOps = inputSource.getLines.grouped(1000000).map({ largePack =>
      largePack.grouped(10000).toSeq.par.map(_.map(MemOp.deserializeFromString)).flatten
    })
    
    memOps.foreach { packet =>
      allCaches.par.foreach { cache =>
        cache.runOps(packet.iterator)
      }
      numOpsDone += packet.size
      System.err.println("Ops done: %d".format(numOpsDone)) 
    }
    
    inputSource.close()
    
    allCaches.foreach { cache =>
      val agResult = cache.getResult
      val resultStr = (Seq(agResult.head.cyclesPerOp.formatted("%.3f"), agResult.head.missRate.formatted("%.3f")) ++ getSpecs(cache.cache).dropRight(1).map(_.paramString)).mkString("\t")
      output.println(resultStr)
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
      if (spec.cacheDataBytes < 8*KB) return false // not smaller than 8KB
      else if (spec.cacheDataBytes >= 7*MB) return false // not bigger than 7MB
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
    if (totalSize > 8*MB) return false
    
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