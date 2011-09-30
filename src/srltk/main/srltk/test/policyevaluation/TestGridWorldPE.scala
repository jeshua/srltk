/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
 * 
 * Copyright 2011 jeshua
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package srltk.test.policyevaluation

import scala.util.Random
import srltk.tools.utils.Bounds2D
import srltk.api.agent._
import srltk.api.domain._
import srltk.api.driver._
import srltk.tools.learners._
import srltk.test.domains._
import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.tools.features.CMAC
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
    //def lf(gamma: Double) = new TD(.05, 0, gamma,-20,0,.1)
    def lf(gamma: Double) = new TD(.01, 0, gamma, 0, 0)
    //def lf(gamma: Double) = new TD(.1, 0, gamma,0)
    val tester = new TestGridWorldPE()
    tester.run(lf, "alpha=.05")
    def lf2(gamma: Double) = new TD(.5, 0, gamma, 0, 0)
    tester.run(lf2, "alpha=.5")

    //tester.testMountainCar()
  }
}
