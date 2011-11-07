package srltk.algs.linear.policies

import srltk.common._

//epsilon-greedy with respect to a Q-function
class EpsilonGreedy(
		val numActions : Int,
		var epsilon: Double,  
		Q: (Feats, Int) => Double) {

  private var tmpEpsilon = epsilon
  def disableExploration() = {tmpEpsilon = epsilon; epsilon = 0;}
  def enableExploration() = epsilon = tmpEpsilon
  
  def act(o: Feats): Int = {
    if (GlobalRNG.nextDouble() < epsilon)
      GlobalRNG.nextInt(numActions);
    else {
      //tie breaker max
      val vals = for (i <- 0 until numActions) yield Q(o, i)
      val max = vals.reduceLeft(math.max _)
      //println("max = "+max)
      val maxs = for (i <- 0 until numActions; if vals(i) == max) yield i
      if (max.isNaN) throw new IllegalArgumentException("Values contain Nans")
      val index = maxs(GlobalRNG.nextInt(maxs.length))
      index
    }
  }
}
