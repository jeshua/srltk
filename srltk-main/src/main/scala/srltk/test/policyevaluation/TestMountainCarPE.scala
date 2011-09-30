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
import srltk.api.agent._
import srltk.api.domain._
import srltk.api.driver._
import srltk.tools.learners._
import srltk.test.domains._
import srltk.test.domains.MountainCar._
import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.tools.features.CMAC
import srltk.vis.ActivePlot
import srltk.tools.utils.Bounds2D

//======================================================================

class TestMountainCarPE(visualize: Boolean = true) extends Test2DPE(visualize) {

  override val bounds = MountainCar.bounds
  val valueStep = 0.03
  val integerStates = false
  val rng = new Random

  def getState(x: Double, y: Double) = new MountainCarState(x, y, rng)

  override val ex = new CMAC(
    List((MountainCar.xMin, MountainCar.xMax),
      (MountainCar.xDotMin, MountainCar.xDotMax)),
    List(15, 15), 10, rng)

  def run(lf: (Double) => LearnerV = null, steps: Int = 100000, useEx: Boolean = true): Double = {
    val gamma: Double = .99;
    val policy = new MountainCarTests.Policy1(gamma)
    val learner = lf(gamma)
    
    val gw = new MountainCar
    runTest(gw, policy, learner, steps, useEx)
  }
}

object TestMountainCarPE {
  def main(args: Array[String]): Unit = {
	srltk.tools.utils.LibFunctions.addLibs()
    def lf(gamma: Double) = new TD(.1, 0, gamma, -20, .01)
    val tester = new TestMountainCarPE()
    tester.run(lf)
  }
}
