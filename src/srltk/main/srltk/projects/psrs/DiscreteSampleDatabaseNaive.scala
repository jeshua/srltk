package srltk.projects.psrs
import scala.collection.mutable._
import util.control.Breaks._
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix

class DiscreteSampleDatabaseNaive(
 numActions: Int,
 numObservations: Int,
 histories: Array[Array[(Int, Int)]],
 tests: Array[Array[(Int, Int)]]) 
extends DiscreteSampleDatabase(numActions,numObservations,histories,tests)
{
  import DiscreteSampleDatabase._
  var histAtLoc : DenseMatrix[Int] = null
  var testAtLoc : DenseMatrix[Int] = null
  var sampleLocs :  Array[ArrayBuffer[Int]] = null
  var extensionAttemptCount = DenseMatrix.zeros[Int](numActions,testActions.length)


  //======================================================================

  def computePaT(a : Int) : DenseVector[Double] = {
    var PaT = DenseVector.zeros[Double](nT)
    for (t <- 0 until nT) {
      val len = tests(t).length
      val denom = samples.length - len
      PaT(t) = if(denom ==0) 0 else extensionAttemptCount(a,taIndex(t)).toDouble / denom
    }
    PaT
  }

  //======================================================================

  def computePHaoT(a : Int, o : Int) : DenseMatrix[Double] = { 
    if(histAtLoc == null)
      updateCounts()

    var HaoT = DenseMatrix.zeros[Int](nH,nT)   
    var PHaoT  = DenseMatrix.zeros[Double](nH,nT)
    val ind = a * numObservations + o
    //loop over samples where this action-observation pair appears
    val locs = sampleLocs(ind)
    var i = 0
    while(i < locs.length){
      if(i < samples.length-1){
        //find haot
        for(h <- 0 until nH if(histAtLoc(h,i)!=0);
            t <- 0 until nT if(testAtLoc(t,i+1))!=0)
          HaoT(h,t) += 1
        
      }
      i += 1
    }
    
    //==================================================

    //now divide count by attempts
    for (h <- 0 until nH) {
      for (t <- 0 until nT) {
        val denom = extensionAttemptCount(a,taIndex(t))
        val numer = HaoT(h,t)
        PHaoT(h, t) =
          if (denom == 0) 0
          else numer.toFloat / denom.toFloat
      }
    }
    PHaoT
  }
  //======================================================================
  private def updateHCount() = {
    for(i <- 0 until samples.length){
      for(h <- 0 until nH){
        if(histAtLocation(histories(h),i)){
          histAtLoc(h,i) = 1
          hCount(h) += 1
        }
      }
    }
  }

  private def updateTCount() = {
    for(i <- 0 until samples.length)
      for(t <- 0 until nT){
        if(testAtLocation(tests(t),i)){
          testAtLoc(t,i) = 1
            tCount(t) += 1
        }
      }
  }

  private def updateHTCount() = {
    //counts of interest
    for(i <- 0 until (samples.length)){
      //find ht
      for(h <- 0 until nH if(histAtLoc(h,i)!=0);
          t <- 0 until nT if(testAtLoc(t,i))!=0)
        htCount(h,t) += 1
    }
  }

  private def updateAttemptCount() = {
    for(i <- 0 until samples.length){     
      //check if action sequence is here
      for(ta <- 0 until testActions.length)
        if(actionSeqAtLocation(testActions(ta),i)){
          attemptCount(ta) += 1
          //update extension test attempt count as well
          if(i > 0)
            extensionAttemptCount(samples(i-1)._1,ta) += 1
        }
    }
  }


  private def updateSampleLocs() = {
    for(i <- 0 until samples.length){     
      //annotate locations of each observation/action pair
      sampleLocs(samples(i)._1*numObservations+samples(i)._2).append(i)
    }
  }
  
  def updateCounts() = {   
    hCount                = Vec0Int(nH)
    histAtLoc             = Mat0Int(nH,samples.length)
    tCount                = Vec0Int(nT)   
    testAtLoc             = Mat0Int(nT,samples.length)
    htCount               = Mat0Int(nH,nT)
    attemptCount          = Vec0Int(testActions.length)
    extensionAttemptCount = Mat0Int(numActions,testActions.length)
    updateHCount()
    updateTCount()
    updateHTCount()
    updateAttemptCount()
    //a list of location where each action/observation pair appears
    sampleLocs            = new Array[ArrayBuffer[Int]](numObservations*numActions)
    for(i <- 0 until sampleLocs.length) sampleLocs(i) = new ArrayBuffer[Int]
    updateSampleLocs() 

  }

  //======================================================================
  // Utility function to find sequences of actions/observations

  def histAtLocation(t : Array[(Int,Int)], loc : Int) = seqAtLocation(t,loc,false)
  def testAtLocation(t : Array[(Int,Int)], loc : Int) = seqAtLocation(t,loc,true)
  def seqAtLocation(seq: Array[(Int,Int)],
                    loc: Int, 
                    isTest : Boolean = false//test vs history
                  ): Boolean = {
    //for history move location backward
    val i = if(isTest) loc else loc-seq.length
    if (i + seq.length > samples.length || i < 0) false
    else {
      var success = true
      breakable {
	for (j <- i until (i + seq.length)) {
	  val actionMatch = seq(j - i)._1 == samples(j)._1
	  val obsMatch = seq(j - i)._2 == samples(j)._2				  
	  if(!actionMatch || !obsMatch){
            success = false;break
	  }
	}
      }//end breakable
      success
    }
  }
  def actionSeqAtLocation(seq: Array[Int],i: Int): Boolean = {
    if (i + seq.length > samples.length) false
    else {
      var success = true
      breakable {
	for (j <- i until (i + seq.length)) {
	  val actionMatch = seq(j - i) == samples(j)._1
	  if(!actionMatch){
            success = false;break
	  }
	}
      }//end breakable
      success
    }
  }


  //======================================================================

}
