package srltk.algs.linear.learners

import srltk.common._
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;
import srltk.common._

class QLearning(
    val numActions : Int,
    val obsDim : Int,
    val alpha: Double, 
    val lambda: Double, 
    val gamma: Double,
    initialValue: Double = 0)
  extends LearnerQ(numActions) {  
  
	val numFeats  : Int = obsDim * numActions
	val theta = DenseVector.zeros[Double](numFeats)
	val z = DenseVector.zeros[Double](numFeats)
	
  //linear value function
  def value(f : VectorCol[Double]): Double = f dot theta

  //create phi_sa
  private def phiSA(f : Feats, a : Int) = {
    val phi = SparseVector.zeros[Double](f.length * numActions)
    val start = f.length * a
    f foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    phi
  } 

  def getQ(o : Feats, a : Int) = value(phiSA(o,a))
  
  def learn(otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double) : Unit = {

    val phi1 = phiSA(otm1, atm1);
    val phi2 = phiSA(ot, getMaxA(ot));

    //calculate the TD error
    val delta = rt + gamma * value(phi2) - value(phi1)

    //Update z and theta, note vector operations
    if (lambda > 0) {
      z := phi1 + z * gamma * lambda
      theta += z * alpha * delta
    } else
      theta += phi1 * alpha * delta;
  }
  def reset() = ()
}
