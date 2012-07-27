package srltk.algs.linear.policies

import srltk.common._
import srltk.algs.linear.learners.QFunction
import srltk.algs.linear.common._
//epsilon-greedy with respect to a Q-function
class EpsilonGreedy(val Q : QFunction, val epsilon : Double) {
  private var tmp = epsilon
  private var eps = epsilon
  def disableExploration() = {tmp = eps; eps = 0;}
  def enableExploration() = eps = tmp;
  
  def act(o: Vec): Int = {
    if (GlobalRNG.nextDouble() < epsilon)
      GlobalRNG.nextInt(Q.numActions());
    else {
    	Q.getMaxA(o)
    }
  }
}
