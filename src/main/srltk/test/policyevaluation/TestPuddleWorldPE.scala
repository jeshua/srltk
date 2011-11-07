package srltk.test.policyevaluation

import scala.util.Random
import srltk.common._
import srltk.common._
import srltk.driver._
import srltk.algs.linear.learners._
import srltk.domains.rlstandard._
import srltk.domains.rlstandard.MountainCar._
import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.features.CMAC
import srltk.vis.ActivePlot

//======================================================================

class TestPuddleWorldPE(visualize: Boolean = true) extends Test2DPE(visualize) {

  override val bounds = PuddleWorld.bounds
  val valueStep = 0.03
  val integerStates = false

  def getState(x: Double, y: Double) = new PuddleWorldState(x, y)

  override val ex = new CMAC(
    List((0, 1),
      (0, 1)),
    List(5, 5), 3)

  def run(lf: (Double) => LearnerV = null, steps: Int = 100000, useEx: Boolean = true): Double = {
    val gamma: Double = .999;

    val policy = new PuddleWorldTests.Policy1(gamma)
    val learner = lf(gamma)
    val gw = new PuddleWorld
    runTest(gw, policy, learner, steps, useEx)
  }
}

object TestPuddleWorldPE {
  def main(args: Array[String]): Unit = {
    def lf(gamma: Double) = new TD(PuddleWorld.dd.obsDim,.01, 0, gamma, 0, 0)
    //def lf(gamma: Double) = new RLSTD(gamma)
    val tester = new TestPuddleWorldPE()
    tester.run(lf)
  }
}
