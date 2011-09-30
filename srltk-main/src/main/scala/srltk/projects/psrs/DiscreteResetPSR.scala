package srltk.projects.psrs
import scalala.tensor.mutable._
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.library.LinearAlgebra._
import scala.annotation.tailrec
class DiscreteResetPSR(simulator: DiscreteResetSimulator) {
  val numActions = simulator.numActions
  val numObservations = simulator.numObservations


  def sigmaCutoff = 0.0001 //TODO actually calculate this! 
  @tailrec
  private final def rankFromSigma(S : DenseVector[Double], n : Int = 0) : Int = {
    if(S(n) < sigmaCutoff)
      n-1
    else
      rankFromSigma(S,n+1)    
  }
  
  
  def extendTest(t : Long, a : Int, o : Int) : Long ={ //extend test with action a and observation o
	  4
  }
  def pTGivenH(t : Long, h : Long) : Double = { //tests and histories are represented index in total ordering
    
    0
  }
  
  @tailrec
  private final def discover(n: Int, oldRank: Int, 
		  oldZ: DenseMatrix[Double]
  ) : DenseMatrix[Double] = {
    val sizeN = scala.math.pow(numActions * numObservations, n).toLong
    //use svd to compute rank and row/column space of proposal matrix Z
    val s = svd(oldZ)
    val U = s._1
    val S = s._2
    val V = s._3
    //approximate rank by counting singular values above a threshold
    val newRank = rankFromSigma(S) 
    //set of linearly independent columns given by first r columns of U
    val QT : DenseMatrix[Double] = DenseMatrix.zeros[Double](U.numRows,newRank) 
    QT := U(0 until U.numRows,0 until newRank)
    //set of linearly independent rows given by first r rows of transpose(V)
    val QH : DenseMatrix[Double] = DenseMatrix.zeros[Double](newRank,V.numCols) 
    QH := V(0 until newRank,0 until V.numCols)
        
    //stop if rank is unchanged
    if (oldZ.numRows > 0 && newRank == oldRank)
      oldZ
    else {
      //create new Z and copy submatrix oldZ into Z
      val sizeN = 5
      var Z: DenseMatrix[Double] = DenseMatrix.zeros[Double](sizeN, sizeN)
      if (oldZ.numRows > 0)
        Z(0 until oldZ.numRows, 0 until oldZ.numCols) := oldZ

      //now insert all the probabilities for new rows and columns
      //core tests
   
      discover(n + 1, newRank, Z)
    }
  }

}