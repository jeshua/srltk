/*******************************************************************************
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 ******************************************************************************/
package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import srltk.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D

object BallBounceWorldTests {
  val rng = new scala.util.Random

  class Policy1(gamma: Double = .9) extends TestPolicy(BallBounceWorld.dd) {
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]

    def getEpVal(s: BallBounceWorldState): Double = {
      if (s.absorbing) 0
      else {
        val a = s.successor(getAction(s))
        -1 + gamma * getEpVal(a)
      }
    }

    override def getValue(s: SimState) = {
      0
    }
    def reset() = ()
    def getInitialState() = BallBounceWorldState()

    def getAction(s: SimState) = {
      val state = s.asInstanceOf[BallBounceWorldState];
      new BallBounceWorldAction(0)
    }
  }
}
