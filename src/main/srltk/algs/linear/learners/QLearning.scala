package srltk.algs.linear.learners

import srltk.common._
import srltk.common._
import srltk.linalg.RealSparseVector

class QLearning(
    val num_actions : Int,
    val obs_dim : Int,
    val alpha: Double, 
    val lambda: Double, 
    val gamma: Double,
    initial_value: Double = 0)
  extends LearnerQ(num_actions) {  
  
	val numFeats  : Int = obs_dim * num_actions
	val theta = Array[Double](numFeats)
	val z = Array[Double](numFeats)
	
  //linear value function
  def value(f : Array[Double]): Double = 0

  //create phi_sa
  private def phiSA(f : Feats, a : Int) = {
    val phi = SparseVector.zeros[Double](f.length * num_actions)
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
