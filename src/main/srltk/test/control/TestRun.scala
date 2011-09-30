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
package srltk.test.control
import srltk.tools.features.CMAC
import srltk.test.domains._
import srltk.tools.learners.Sarsa
import srltk.tools.actors.EpsilonGreedy
import srltk.tools.agent.EGreedySarsa
import srltk.api.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D
import srltk.tools.agent.EGreedyQLearning
import util.control.Breaks._
import srltk.vis.MaxAVisualizer2D
import srltk.api.domain._
import srltk.api.agent._
import srltk.tools.learners._
import srltk.tools.utils.Bounds2D
import srltk.api.agent.HasActor
import srltk.vis.ValueFunctionVisualizer
import srltk.vis.ValueFunctionVisualizer3D

object TestRun {

  def apply(
    domain: Domain,
    agent: Agent,
    timesteps: Option[Int],
    episodes: Option[Int],
    visuals: Boolean = true) {
	  srltk.tools.utils.LibFunctions.addLibs()
    require(timesteps != None || episodes != None)
    val rng = new scala.util.Random()
    val driver = new SimpleDriver(domain, agent)

    //==================================================

    //record and show reward
    val plot =
      if (visuals) {
        val p = new ActivePlot()
        p.display()
        p.setLocation(500, 0)
        p
      } else null

    //see if we can display a 2d value function
    var updateVisuals: ((Int) => Unit) = (a : Int) => ()

    if (visuals && agent.isInstanceOf[HasQFunction] && domain.isInstanceOf[Domain2D]) {
      val aq = agent.asInstanceOf[HasQFunction]
      val d2d = domain.asInstanceOf[Domain2D]
      val action = domain.exampleAction().asInstanceOf[Action]
      def V2D(x: Double, y: Double) = {
        val o = d2d.createState(x, y).observation
        val v = for (i <- 0 until action.numActions)
          yield aq.getQ(if (agent.extractor != null) agent.extractor <= o else o, action.manufacture(i));
        v.reduceLeft(scala.math.max(_, _))
      }
      def MaxA2D(x: Double, y: Double): Int = {
        val o = d2d.createState(x, y).observation
        aq.getMaxA(if (agent.extractor != null) agent.extractor <= o else o);
      }
      val visualizer1 : ValueFunctionVisualizer = new ValueFunctionVisualizer3D(d2d.bounds, V2D)
      visualizer1.getFrame.setLocation(0,500)
      val visualizer2 = new MaxAVisualizer2D(d2d.bounds, MaxA2D, d2d.integerState)
      visualizer2.vis.setLocation(500,500)
      var calls = 0
      updateVisuals = (timestep : Int) => { 
    	calls = calls+1
        visualizer1.update();
        visualizer2.vis.repaint();
        if(calls == 2 || calls % 10 == 0)
          visualizer1.reRender()
    	  
      }
    }

    //++++++++++++++++++++

    def runEpisodes(maxEps: Int) {
      var timesteps: Int = 0
      var episodes = 0
      val step = 20
      var timestepSum : Double = 0
      breakable {
        for (i <- 1 to 10000000) {
          val data = driver.step()
          val reward = data._2.reward
          timesteps =
            if (data._5) {
              
              episodes = episodes + 1;
              if (visuals) {
                if(episodes % step == 0)
                {
                	plot.addPoint(episodes,timestepSum/step)
                	updateVisuals(timesteps)
                	timestepSum = 0
                } else {timestepSum += timesteps}
              } 
              0
            } else { timesteps + 1 }
          
          if (episodes > maxEps) break
        }
      }
    }

    //++++++++++++++++++++

    def runTimesteps(timesteps: Int) {
      val steps = 10000
      var sum = 0.0
      for (i <- 1 to timesteps) {
        val data = driver.step()

        val reward = data._2.reward
        if (i % steps == 0) {

          if (visuals) {
            plot.addPoint(i, sum / steps)
            updateVisuals(i)
          } //else println("Timestep: "+i)   

          sum = 0
          0
        } else { sum += reward }
      }      
    }
    //run the steps
    if (timesteps != None)
      runTimesteps(timesteps.get)
    else
      runEpisodes(episodes.get)
    //show the policy visually
    if (visuals) {

      if (agent.isInstanceOf[HasActor])
        agent.asInstanceOf[HasActor].disableExploration()

      val sv = new StateViewer(500, 500, domain.renderer)
      for (i <- 1 to 100000) {
        val data = driver.step()
        if (i % 1 == 0) {
          sv.updateDisplay(domain.state)
          Thread.sleep(20)
        }
      }
    }
  }
}
