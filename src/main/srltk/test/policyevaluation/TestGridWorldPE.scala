package srltk.test.policyevaluation

import scala.util.Random
import srltk.utils.Bounds2D
import srltk.common._
import srltk.common._
import srltk.driver._
import srltk.algs.linear.learners._
import srltk.domains.rlstandard._
import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.vis.ActivePlot

//======================================================================

class TestGridWorldPE(visualize: Boolean = true) extends Test2DPE(visualize) {
  override val bounds = new Bounds2D(0, GridWorld.width, 0, GridWorld.height)
  val valueStep = 1.0
  val integerStates = true

  def getState(x: Double, y: Double) = new GridWorldState(x.toInt, y.toInt)

  def run(lf: (Double) => LearnerV = null, name: String = ""): Double = {
    val gamma: Double = .99;
    val policy = new GridWorldTests.Policy1(gamma)
    val learner = lf(gamma)
    val gw = new GridWorld
    runTest(gw, policy, learner, 30000)
  }
}

object TestGridWorldPE {
  def main(args: Array[String]): Unit = {
    val dd = GridWorld.dd
    //def lf(gamma: Double) = new TD(.05, 0, gamma,-20,0,.1)
    def lf(gamma: Double) = new TD(dd.obsDim,.01, 0, gamma, 0, 0)
    //def lf(gamma: Double) = new TD(.1, 0, gamma,0)
    val tester = new TestGridWorldPE()
    tester.run(lf, "alpha=.05")
    def lf2(gamma: Double) = new TD(dd.obsDim,.5, 0, gamma, 0, 0)
    tester.run(lf2, "alpha=.5")

    //tester.testMountainCar()
  }
}
