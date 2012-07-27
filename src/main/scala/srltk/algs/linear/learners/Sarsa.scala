package srltk.algs.linear.learners

import srltk.common._
import srltk.algs.linear.common._
import scalala.tensor.sparse.SparseVector

class Sarsa(
    val num_actions : Int,
    val obs_dim : Int,
    val alpha0: Double, 
    val lambda0: Double, 
    val gamma0: Double,
    initial_value: Double = 0)
  extends LearnerQ(num_actions)  {
  
  def numActions() = num_actions
  
  val TD = new TD(obs_dim * num_actions, alpha0, lambda0, gamma0, initial_value)

  //create phi_sa
  private def phiSA(o: Vec, a: Int) : Vec = {
    if(o.length != obs_dim) throw new IllegalArgumentException("Input features wrong size.")
    val phi = SparseVector.zeros[Double](obs_dim * num_actions)
    val start = o.length * a
    o foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    phi
  }

  def getQ(o: Vec, a: Int) = TD.getV(phiSA(o, a))

  //learn on each sarsa
  def learn(otm1: Vec, rtm1 : Double, atm1: Int, ot: Vec, rt : Double) : Unit = ()
  override def learn(otm2: Vec, rtm2 : Double, atm2: Int,otm1: Vec, rtm1 : Double, atm1: Int, ot: Vec, rt : Double) : Unit = {
	  TD.learn(phiSA(otm2, atm2),rtm2,0,phiSA(otm1, atm1),rtm1);
  }  
  def reset() = TD.reset
}
