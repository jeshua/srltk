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
package srltk.projects.tdnets.test

import srltk.api.domain._
import srltk.api.agent._
import srltk.test.domains.MountainCar
import srltk.test.domains.MountainCar._
import srltk.test.domains._
import srltk.test.policyevaluation.AgentWrapper
import srltk.api.driver._
import scalala.operators.Implicits._
import scalala.tensor._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import srltk.vis.ActivePlot
import srltk.tools.features.CMAC
import srltk.vis.ValueFunctionVisualizer2D
import srltk.tools.utils.Bounds2D
import srltk.projects.tdnets._

class TestMVTD(
  alpha: Double,
  lambda: Double,
  gamma: Double,
  ex: FeatureTransform)
  extends CanLearn {
  var mvtd: MultivariateTD = null

  override def learn(otm1: Observation, atm1: Action, ot: Observation) {
    val input1 = ex <= otm1.features;
    val input2 = ex <= ot.features;
    val prediction2 = mvtd.predict(input2)

    def error(prediction1: VectorCol[Double]) = {
      ot.features + prediction2 * gamma - prediction1
    }
    mvtd.learn(input2, error _)

  }

  def onImprint(): Unit = {
    val f = imprintedO.features
    this.mvtd =
      new MultivariateTD(alpha, lambda,
        (ex <= f).length,
        f.length)
  }

  def predict(o: Observation): VectorCol[Double] = {
    mvtd.predict(ex <= o.features)
  }

  def lastError() = mvtd.lastError

}

//======================================================================

object TestMultivariateTD {

  def main(args: Array[String]): Unit = {

    val pw = false

    if (pw) {
      val ex = new CMAC(
        List((0, 1),
          (0, 1)),
        List(10, 10), 10, new scala.util.Random)
      val mc = new PuddleWorld
      val policy = new PuddleWorldTests.Policy1(.2)
      def createState(d1: Double, d2: Double): State = new PuddleWorldState(d1, d2)
      test(PuddleWorld.bounds, mc, policy, createState, ex)
    } else {

      val rng = new scala.util.Random
      val ex = new CMAC(
        List((MountainCar.xMin, MountainCar.xMax),
          (MountainCar.xDotMin, MountainCar.xDotMax)),
        List(10, 10), 10, rng)

      val mc = new MountainCar
      val policy = new MountainCarTests.Policy1
      def createState(d1: Double, d2: Double): State = new MountainCarState(d1, d2, rng)
      test(MountainCar.bounds, mc, policy, createState, ex)
    }

  }

  //======================================================================

  def test(bounds: Bounds2D,
    mc: Domain,
    policy: TestPolicy,
    createState: (Double, Double) => State,
    ex: FeatureTransform): Unit =
    {
      val learner = new TestMVTD(.2, 0, .99, ex)
      val agent = new AgentWrapper(learner, policy, null)
      val driver = new SimpleDriver(mc, agent)

      def dim1(d1: Double, d2: Double): Double = {
        val obs = createState(d1, d2).observation
        val prediction = learner.predict(obs)
        prediction(0)
      }
      def dim2(d1: Double, d2: Double): Double = {
        val obs = createState(d1, d2).observation
        val prediction = learner.predict(obs)
        prediction(1)
      }
      val viewer1 = new ValueFunctionVisualizer2D(bounds, dim1)
      val viewer2 = new ValueFunctionVisualizer2D(bounds, dim2)

      var obs: Observation = null;
      var lastObs: Observation = driver.step()._2;

      val plot = new ActivePlot()
      plot.display()
      var sum = 0.0
      val step = 500.0
      for (i <- 1 to 300000) {
        val output = driver.step()
        obs = output._2
        sum = if (i % step == 0) {
          viewer1.vis.repaint()
          viewer2.vis.repaint()
          plot.addPoint(i, sum / step)
          0
        } else sum + learner.lastError

        lastObs = obs
      }

    }

}

