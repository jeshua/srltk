package srltk.projects.psrs
import DataSequences._
import srltk.tools.linearalgebra.MiscStats._
class ContinuousTPSR(
		numActions : Int,    
		obsDim : Int,        
		data : DataSequence, //sequence of observed data
		testLen : Int,       //length of tests to use
		numTests : Int,      //total number of samples to use to make test centers
		histLen : Int,       //length of histories to use
		numHists : Int,      //total number of histories to use as centers
		rng : scala.util.Random
		)
		{
	//choose at least 100 centers
	val numObsCenters = math.max(data.length/1000,math.min(100,data.length))
	
	//choose observation centers: matrix of size {numFeats x numObsCenters}
	val obsX = DataSequences.chooseRandomSeqObs(data,1, numObsCenters, rng)
	//choose test and history centers of size {(numFeats*len) x num}
	val testX = DataSequences.chooseRandomSeqObs(data,testLen,numTests, rng)
	val histX = DataSequences.chooseRandomSeqObs(data,histLen,numHists, rng)
  
	
		
		}