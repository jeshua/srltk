/**
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 * **/

package srltk.algs.linear.learners

import srltk.common._
import scalala.tensor.sparse.SparseVector
import scalala.tensor._
import scalala.tensor.dense._
class Sarsa(
    val numActions : Int,
    val obsDim : Int,
    val alpha0: Double, 
    val lambda0: Double, 
    val gamma0: Double,
    initialValue: Double = 0)
  extends LearnerQ(numActions)  {
  val TD = new TD(obsDim*numActions, alpha0, lambda0, gamma0, initialValue)

  //create phi_sa
  private def phiSA(o: Feats, a: Int) : Feats = {
    if(o.length != obsDim) throw new IllegalArgumentException("Input features wrong size.")
    val phi = SparseVector.zeros[Double](obsDim * numActions)
    val start = o.length * a
    o foreachNonZeroPair { case (key: Int, value: Double) => phi(key + start) = value }
    phi
  }

  def getQ(o: Feats, a: Int) = TD.getV(phiSA(o, a))

  //learn on each sarsa
  def learn(otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double) : Unit = ()
  override def learn(otm2: Feats, rtm2 : Double, atm2: Int,otm1: Feats, rtm1 : Double, atm1: Int, ot: Feats, rt : Double) : Unit = {
	  TD.learn(phiSA(otm2, atm2),rtm2,0,phiSA(otm1, atm1),rtm1);
  }  
  def reset() = TD.reset
}
