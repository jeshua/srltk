package srltk.algs.linear.learners

import srltk.common._
import scalala.tensor.sparse.SparseVector
import srltk.algs.linear.common._
import scalala.tensor.dense.DenseVector

class QLearning(
    val num_actions : Int,
    val obs_dim : Int,
    val alpha: Double, 
    val lambda: Double, 
    val gamma: Double,
    initial_value: Double = 0)
  extends LearnerQ(num_actions) {  
  
  def numActions() = num_actions;
	val n_ao_features  : Int = obs_dim * num_actions
	val theta = DenseVector.zeros[Double](n_ao_features)
	val z = DenseVector.zeros[Double](n_ao_features)
	
  //linear value function
  def value(f : Vec): Double = f dot theta

  //create phi_sa
  private def phiSA(f : Vec, a : Int) = {
    val phi = SparseVector.zeros[Double](f.length * num_actions)
    val start = f.length * a
    f foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    phi
  } 

  def getQ(o : Vec, a : Int) = value(phiSA(o,a))
  
  def learn(otm1: Vec, rtm1 : Double, atm1: Int, ot: Vec, rt : Double) : Unit = {

    val phi1 = phiSA(otm1, atm1);
    val phi2 = phiSA(ot, getMaxA(ot));

    //calculate the TD error
    val delta = rt + gamma * value(phi2) - value(phi1)

    //Update z and theta
    if (lambda > 0) {
      z := phi1 + z * gamma * lambda
      theta += z * alpha * delta
    } else
      theta += phi1 * alpha * delta;
  }
  def reset() = ()
}
