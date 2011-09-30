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
 ***************************************************************************** */
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
import scala.collection.mutable.ArrayBuffer
import srltk.vis.StateViewer
import srltk.projects.tdnets._
import srltk.projects._
object TestTDChain {	

  def main(args: Array[String]): Unit = {

    val domain = "mc"
    	val steps = 50000
    if (domain == "pw") {
      val ex = new CMAC(
        List((0, 1),
          (0, 1)),
        List(10, 10), 10, new scala.util.Random)
      val mc = new PuddleWorld
      val policy = new PuddleWorldTests.Policy1(.2)
      def createState(p: Feats): State = new PuddleWorldState(p(0), p(1))
      test(PuddleWorld.bounds,
        mc, policy, createState, ex, steps)
    } else if (domain == "mc") {

      val rng = new scala.util.Random
      val ex = new CMAC(
        List((MountainCar.xMin, MountainCar.xMax),
          (MountainCar.xDotMin, MountainCar.xDotMax)),
        List(20, 20), 20, rng)
      val mc = new MountainCar
      val policy = new MountainCarTests.Policy1
      def createState(p: Feats): State = new MountainCarState(p(0), p(1), rng)
      test(MountainCar.bounds, mc, policy, createState, ex,70000,.04,8)
    } else if (domain == "ball") {

      val rng = new scala.util.Random
      val bin = 9	
      val ex = new CMAC(
        List((0, 1),
          (0, 1),
          (0, 1),
          (0, 1),
          (0, scala.math.Pi)),
        List(1, 1, bin, bin, bin), 7, rng)

      val mc = new BallBounceWorld
      val policy = new BallBounceWorldTests.Policy1
      def createState(p: Feats): State = new BallBounceWorldState(p(0), p(1), p(2), p(3), p(4))
      test(MountainCar.bounds, mc, policy, createState, ex, 200000,0.04,10)
    }

  }

  //======================================================================

  def test(bounds: Bounds2D,
    domain: Domain,
    policy: TestPolicy,
    createState: (Feats) => State,
    ex: FeatureTransform,
    timesteps : Int,
    alpha : Double = 0.01,
    k : Int = 4): Unit =
    {
      val learner = TDChain(k,alpha, ex)
      
      val agent = new AgentWrapper(learner, policy, null)
      val driver = new SimpleDriver(domain, agent)
      var obs: Observation = null;
      var lastObs: Observation = driver.step()._2;

      val plotRMSE = new ActivePlot("","Timesteps","RMSE")
      val plotTDE = new ActivePlot("","Timesteps","TD Error")
      var names : List[String] = Nil
      println(learner.learner)
      for (n <- learner.net.internalNodes) {
        plotRMSE.newDataset(n._1)
        plotTDE.newDataset(n._1)
        names ::= n._1
      }
      plotRMSE.display()
      plotTDE.display()
      plotTDE.setLocation(500, 0);

      val step = timesteps/50.0
      val sumRMSE = new Array[Double](learner.net.internalNodes.size)
      val sumTDE = new Array[Double](learner.net.internalNodes.size)
      
      
      for (i <- 1 to timesteps) {
        val output = driver.step()
        obs = output._2
        val errors = learner.lastRMSErrors
        //val errors = learner.lastTDErrors

        if (i % step == 0) {
          for (n <- 0 until sumRMSE.length) plotRMSE.addPointToSeries(i, sumRMSE(n) / step, n)
          for (n <- 0 until errors.length) sumRMSE(n) = 0
          for (n <- 0 until sumTDE.length) plotTDE.addPointToSeries(i, sumTDE(n) / step, n)
          for (n <- 0 until errors.length) sumTDE(n) = 0
        } else {
          for (i <- 0 until errors.length) sumRMSE(i) += learner.lastRMSErrors(i)
          for (i <- 0 until errors.length) sumTDE(i) += learner.net.lastErrors(names(i))
        }
        lastObs = obs
      }

      //==================================================
      val viewer = new StateViewer(600, 600, domain.renderer)
      for (i <- 1 to 10000000) {

        val output = driver.step()
        domain.renderer.clearPredictions();
        for (node <- learner.net.internalNodes) {
          val prediction2 = node._2.getY(output._2)
          val state2 = createState(prediction2)
          domain.renderer.addPrediction(state2);          
        }
        viewer.updateDisplay(output._1)
        Thread.sleep(500)
      }
    }
}

