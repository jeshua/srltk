package srltk.projects.psrs
import scala.collection.mutable._
import util.control.Breaks._
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix
import scala.math._


//----------------------------------------------------------------------
object DiscreteSampleDatabase {

  //aliases to make code a bit more concise
  def Vec0Int(len : Int) = DenseVector.zeros[Int](len)
  def Mat0Int(len1 : Int, len2 : Int) = DenseMatrix.zeros[Int](len1,len2)
  type Sample = (Int, Int)
  type History = Array[(Int, Int)]
  type Test = History

  //create test action sequences
  def getTestActionsSingle(test : Test) = for(i <- test) yield i._1
  //create testActions and testToTestAction
  def getTestActions(tests: Array[Array[(Int, Int)]]) = {
    val nT = tests.length
    val set = new HashMap[Array[Int],Int]
    val temp = new ArrayBuffer[Array[Int]]
    var count = 0
    val taIndex = new Array[Int](nT)
    for(t <- 0 until nT){
      val ta = getTestActionsSingle(tests(t))
      if(set.contains(ta))
        taIndex(t) = set(ta)
      else {
        set(ta) = count
        temp += ta
        taIndex(t) = count
        count += 1
      }
    }
    (temp.toArray,taIndex)
  }
}

//----------------------------------------------------------------------

abstract class DiscreteSampleDatabase(
  val na: Int,
  val no: Int,
  val histories: Array[Array[(Int, Int)]],
  val tests: Array[Array[(Int, Int)]]) {

  import DiscreteSampleDatabase._

  val nH = histories.length
  val nT = tests.length

  val temp = getTestActions(tests)
  var testActions : Array[Array[Int]] = temp._1
  var taIndex : Array[Int] = temp._2


  //counts of history-test pairs h, ht, haot
  var tCount  : DenseVector[Int] = null
  var hCount  : DenseVector[Int] = null
  var htCount : DenseMatrix[Int] = null
  var hatCount : DenseMatrix[Int] = null

  //counts of test action sequences
  protected var attemptCount : DenseVector[Int] = null 
  def getAttemptCount(test : Int) =
    attemptCount(taIndex(test))

  var samples = new ArrayBuffer[Sample]
  def numSamples( t : Int, h : Int) : Int = {
    samples.length - 
    (histories(h).length + tests(t).length - 1)
  }
  def append(samples: Iterable[Sample], update : Boolean = true) = {
    this.samples ++= samples
    if(update)
      updateCounts()
  }  

  def computePaT(a : Int) : DenseVector[Double]
  def computePHaoT(a : Int, o : Int) : DenseMatrix[Double]
  def computePaoTgivenH(a : Int, o : Int) : DenseMatrix[Double] = {null}
  def updateCounts()
}


//----------------------------------------------------------------------


class DiscreteSampleDatabaseAC(
  na: Int,
  no: Int,
  histories: Array[Array[(Int, Int)]],
  tests: Array[Array[(Int, Int)]])
extends DiscreteSampleDatabase(na,no,histories,tests)
{

  //======================================================================

  def computePaT(a : Int) : DenseVector[Double] = {
    val o = 0
    var PaT = DenseVector.zeros[Double](nT)
    val extests = for(t <- this.tests) yield (a,o) +: t
    val matcher = new ACMatcher(na,no,samples)
    val ret = matcher.getCounts(extests,histories)
    val exAttemptCount = ret._4
    val nta = testActions.length
    for (t <- 0 until nT) {
      val ta = taIndex(t)
      val len = testActions(ta).length
      val denom = samples.length - len
      if(denom > 0)  PaT(ta) = exAttemptCount(ta)/denom.toDouble
    }
    PaT
  }

  //======================================================================

  def computePHaoT(a : Int, o : Int) : DenseMatrix[Double]= { 
    if(hCount == null) updateCounts()
    var HaoT : DenseMatrix[Int] = null   
    var PHaoT : DenseMatrix[Double] = DenseMatrix.zeros[Double](nH,nT)
    val extests = for(t <- this.tests) yield (a,o) +: t
    //count extension tests/histories
    val matcher = new ACMatcher(na,no,samples)
    val ret = matcher.getCounts(extests,histories)
    HaoT = ret._3 //initialize to htCount
    val exAttemptCount = ret._4
    //now divide count by attempts
    for (h <- 0 until nH) 
      for (t <- 0 until nT) {
        val denom = exAttemptCount(taIndex(t))
        PHaoT(h, t) =
          if (denom == 0) 0d
          else HaoT(h,t) / denom.toFloat
      }
    PHaoT
  }
  

  //======================================================================

  override def computePaoTgivenH(a : Int, o : Int) : DenseMatrix[Double]= { 
    if(hCount == null) updateCounts()
    var HaoT : DenseMatrix[Int] = null   
    var PHaoT : DenseMatrix[Double] = DenseMatrix.zeros[Double](nH,nT)
    val extests = for(t <- this.tests) yield (a,o) +: t
    //count extension tests/histories
    val matcher = new ACMatcher(na,no,samples)
    val ret = matcher.getCounts(extests,histories)
    HaoT = ret._3 //initialize to htCount
    val hatCount = ret._5
    //now divide count by attempts
    for (h <- 0 until nH) 
      for (t <- 0 until nT) {
        val denom = hatCount(h,taIndex(t))
        PHaoT(h, t) =
          if (denom == 0) 0d
          else HaoT(h,t) / denom.toFloat
      }
    PHaoT
  }

  //======================================================================

  def updateCounts() = {   
    val matcher = new ACMatcher(na,no,samples)
    val ret = matcher.getCounts(tests,histories,testActions,taIndex)
    this.hCount = ret._1
    this.tCount = ret._2
    this.htCount = ret._3
    this.attemptCount = ret._4
    this.hatCount = ret._5
  }

}





class ACMatcher(na : Int, no : Int, samples : ArrayBuffer[(Int,Int)]) {
  import DiscreteSampleDatabase._

  //======================================================================
  //For AC string matching

  def sampleToInt(s : (Int,Int)) = (s._1 * no + s._2)

  def seqToIntArray(seq : Array[(Int,Int)]) : Array[Int] = {
    for(s <- seq) yield sampleToInt(s)
  }

  def seqsToIntArrays(seqs : Array[Array[(Int,Int)]]) : Array[Array[Int]] = {
    for(seq <- seqs) yield for(s <- seq) yield sampleToInt(s)
  }

  /**
   * Get marginal and joint counts for array of tests and histories
   *
   * @returns (hCount,tCount,htCount, attemptCount, hatCount)
   * */
  
  def getCounts(tests : Array[Array[(Int,Int)]],
                histories : Array[Array[(Int,Int)]],
                //if left null, these are computed automatically
                _testActions : Array[Array[Int]] = null,
                _taIndex : Array[Int] = null
              ) = {
    val hCount  = Vec0Int(histories.length)
    val tCount  = Vec0Int(tests.length)   
    val htCount = Mat0Int(histories.length,tests.length)

    //we will also count test attempts here using a second automaton
    var testActions = _testActions
    var taIndex = _taIndex
    if(_testActions == null || _taIndex == null){
      val temp = getTestActions(tests)
      testActions = temp._1
      taIndex = temp._2
    }
    val nta = testActions.length
    val hatCount = Mat0Int(histories.length,nta)
    val attemptCount = Vec0Int(nta)

    /* *
     * Set of loc --> List[histories]: histories present at loc
     * we will ensure this set does not grow much larger than the size of the longest test
     * this is relatively efficient because we need access time and size is divorced from
     * sample length, and has near constant access time and deletion
     * */
    val maxTestLength = (for(t <- tests) yield t.length).foldLeft(0)(max(_,_))
    val histBufferLen = maxTestLength+1
    var histAtLoc = new LimitedHistoryArray[List[Int]](histBufferLen,Nil)


    /**
     *Create automota:
     * acAutom        : matches all histories and tests
     * acAttemptAutom : matches test attempts (sequence of actions only)
     * */
    val acAutom = new ACAutomaton(seqsToIntArrays(tests ++ histories),
                                  no*na)
    acAutom.reset()
    val acAttemptAutom = new ACAutomaton(testActions,na)
    acAttemptAutom.reset()

    //update hcount and tcount
    for(loc <- 0 until samples.length){
      histAtLoc(loc+1) = Nil
      val output = acAutom.step(sampleToInt(samples(loc)))

      //MATCH HISTORIES/TESTS
      //output contains all tests and histories that ended here
      for(i <- 0 until output.length){
        //check if history or test
        val isHist = output(i) >= tests.length
        if(isHist){
          val h = output(i) - tests.length //history index
          val histLoc = loc+1;//histories are counted at end+1
          if(histLoc < samples.length){
            hCount(h) += 1
            histAtLoc(histLoc) ::= h
          }
        } else{
          val t = output(i) //test index
          val testLoc = loc - tests(t).length+1 //test is counted from start
          if(testLoc >= 0){
            tCount(t) += 1
            //increment htcount for t and histories that ended at testLoc
            val hists = histAtLoc(testLoc)
            for(h <- hists)
              htCount(h,t) += 1            
          }
        }//end hist/test conditional
      }//end output loop

      //MATCH ATTEMPTED TESTS
      val attemptOutput = acAttemptAutom.step(samples(loc)._1)
      for(i <- 0 until attemptOutput.length){
        val ta = attemptOutput(i) //test index
        val testLoc = loc - testActions(ta).length+1 
        if(testLoc >= 0) attemptCount(ta) += 1
        //update hta count
        val hists = histAtLoc(testLoc)
        for(h <- hists)
          hatCount(h,ta) += 1            
      }//end loop over matched test
    }//end sample loop
    //return counts
    (hCount, tCount, htCount, attemptCount, hatCount)
  }

  //==============================  

 
}
