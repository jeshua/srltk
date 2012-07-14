package srltk.algs.linear.learners

import srltk.common._
import srltk.algs.linear.agents.HasQFunction


abstract class LearnerQ(num_actions : Int) extends Learner with HasQFunction{

  def getMaxA(o: Feats): Int =
    {
      val vals = for (i <- 0 until num_actions) yield getQ(o, i)
      val max = vals.reduceLeft(scala.math.max _)
      val maxs = for (i <- 0 until num_actions; if vals(i) == max) yield i
      maxs(GlobalRNG.nextInt(maxs.length))
      maxs(0)
    }
}
