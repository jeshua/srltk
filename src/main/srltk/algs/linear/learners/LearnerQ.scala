package srltk.algs.linear.learners

import srltk.common._
import srltk.algs.linear.agents.HasQFunction


abstract class LearnerQ(numActions : Int) extends Learner with HasQFunction{

  def getMaxA(o: Feats): Int =
    {
      val vals = for (i <- 0 until numActions) yield getQ(o, i)
      val max = vals.reduceLeft(scala.math.max _)
      val maxs = for (i <- 0 until numActions; if vals(i) == max) yield i
      maxs(GlobalRNG.nextInt(maxs.length))
      maxs(0)
    }
}
