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

import srltk.vis._
import srltk.vis.spacesconnect._
import srltk.tools.features.CMAC
import srltk.vis.ActivePlot
import srltk.tools.utils.Bounds2D

/**
 * Wraps a learner and a fixed test policy into an agent structure.
 */
class AgentWrapper(learner: CanLearn, policy: TestPolicy, ex: FeatureTransform = null) extends Agent(ex) {
  override def learn(o1: Observation): Unit =
    learner.learn(o1);
  override def learn(o1: Observation, a1: Action, o2: Observation): Unit =
    learner.learn(o1, a1, o2);
  override def learn(otm2: Observation, atm2: Action, otm1: Observation, atm1: Action, ot: Observation): Unit =
    learner.learn(otm2, atm2, otm1, atm1, ot);
  override def act(o: Observation): Action = policy.getAction(o.state)
  override def onImprint() = learner.imprint(imprintedO, imprintedA)
}

abstract class Test2DPE(val visualize: Boolean = true) {

  //======================================================================

  //OVERRIDE
  val bounds: Bounds2D
  val valueStep: Double
  val integerStates: Boolean
  var useEx: Boolean = true
  val ex: FeatureTransform = null
  def getState(x: Double, y: Double): State

  //params
  val vis2D = false

  //======================================================================

  //function to get value function from learner
  def learnerV2D(learner: LearnerV)(x: Double, y: Double) = {
    if (learner.isImprinted) {
      val o = getState(x, y).observation
      val fo = if (ex != null && useEx) ex <= o else o
      learner.value(fo);
    } else 0
  }

  def realV2D(policy: TestPolicy)(x: Double, y: Double) = {
    policy.getValue(getState(x, y));
  }

  //calculate sum of squared error between V2D and policy.V2D
  def RMSE(realV2D: (Double, Double) => Double, learnerV2D: (Double, Double) => Double)() = {
    val range1 = bounds.xMin until bounds.xMax by valueStep
    val range2 = bounds.yMin until bounds.yMax by valueStep
    //first make list of squared errors
    val mse = (for (x <- range1; y <- range2)
      yield scala.math.pow(scala.math.abs(realV2D(x, y) - learnerV2D(x, y)), 2))
      //then sum the errors and divide by number of tests
      .foldLeft(0.0)(_ + _) / (range1.length + range2.length).toDouble
    //return root of mean squared error
    math.sqrt(mse)
  }

  var rmse: () => Double = null

  def createValueVis(valFunction: (Double, Double) => Double): ValueFunctionVisualizer =
    if (vis2D)
      new ValueFunctionVisualizer2D(bounds, valFunction, integerStates)
    else
      new ValueFunctionVisualizer3D(bounds, valFunction)

  var vis1: ValueFunctionVisualizer = null
  var vis2: ValueFunctionVisualizer = null
  var plot: ActivePlot = null
  var policy: TestPolicy = null
  var learner: LearnerV = null

  def setupVis(policy: TestPolicy, learner: LearnerV) {

    if (vis1 == null) {
      vis1 = createValueVis(realV2D(policy) _)
      vis1.setValueFunction(learnerV2D(learner) _)
      vis2 = createValueVis(realV2D(policy) _)
      
      vis1.getFrame.setSize(500, 500)
      vis2.getFrame.setSize(500, 500)
      vis1.getFrame.setLocation(0, 0)
      vis2.getFrame.setLocation(500, 0)
    } else {
      vis1.setValueFunction(learnerV2D(learner) _)
      vis2.setValueFunction(realV2D(policy) _)
      vis1.getFrame.repaint()
      vis2.getFrame.repaint()
    }
    if (plot == null) {
      plot = new ActivePlot("", "Timestep", "RMSE", 1000, 500)
      plot.setLocation(0, 500)
      plot.display()
    }
  }

  var updateVisFunction: (Int, () => Double, ActivePlot) => Unit = null
  def setUpdateVisFunction(f: (Int, () => Double, ActivePlot) => Unit) {
    this.updateVisFunction = f
  }

  def updateVis(timestep: Int) {
    if (timestep % 500 == 0) {
      vis1.update()
      //vis2.update()
    }
    if (updateVisFunction == null) {
      if (timestep % 1000 == 0) {
        plot.addPoint(timestep, rmse())
        Thread.sleep(40)
      }
    } else
      updateVisFunction(timestep, rmse, plot)
  }

  def close() {
    if (plot != null)
      plot.dispose()
    if (vis1 != null)
      vis1.getFrame.dispose()
    if (vis2 != null)
      vis2.getFrame.dispose()

  }

  //======================================================================

  /**
   * Test given learner
   * @returns RMSE between value function learned and true value function
   */
  def runTest(domain: Domain,
    policy: TestPolicy,
    learner: LearnerV,
    steps: Int,
    useEx: Boolean = true): Double = {

    this.useEx = useEx
    if (visualize)
      setupVis(policy, learner)
    this.rmse = RMSE(learnerV2D(learner) _, realV2D(policy) _)_

    val driver = new SimpleDriver(domain, new AgentWrapper(learner, policy, if (useEx) ex else null))
    for (i <- 0 to steps) {
      val st = driver.step
      if (visualize) {
        updateVis(i)
      }
    }
    //calculate SSE
    RMSE(learnerV2D(learner) _, realV2D(policy) _)
  }
}
