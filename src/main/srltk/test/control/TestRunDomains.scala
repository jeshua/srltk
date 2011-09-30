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
import srltk.tools.actors.EpsilonGreedy
import srltk.tools.agent.EGreedySarsa
import srltk.tools.agent.EGreedyQLearning
import srltk.api.agent._
import srltk.api.driver.SimpleDriver
import srltk.vis.StateViewer
import scala.collection.mutable.ArrayBuffer
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D

object TestRunMountainCar
{
  val rng = new scala.util.Random()
  //create mountain car
  val domain = new MountainCar(rng)
  //Feature extractor for mountain car
  val ex = new CMAC(
      List((MountainCar.xMin, MountainCar.xMax),
          (MountainCar.xDotMin, MountainCar.xDotMax)),
          List(13, 13), 10, rng)
  
  def apply(agent : Agent,episodes : Int,  visuals :Boolean = true)
  {
    TestRun(domain,agent,None,Some(episodes),visuals)
  }
}


object TestRunPuddleWorld
{
  val rng = new scala.util.Random()
  //Feature extractor for mountain car
  val domain = new PuddleWorld()
  val ex = new CMAC(
      List((0, 1),
          (0, 1)),
          List(10, 10), 10, rng)
  def apply(agent : Agent,episodes : Int,  visuals :Boolean = true)
  {
    TestRun(domain,agent,None,Some(episodes),visuals)
  }
  def run2(agent : Agent, timesteps: Int,  visuals :Boolean = true)
  {
    TestRun(domain,agent,Some(timesteps),None,visuals)
  }
}


object TestRunBallBounceWorld
{
  val rng = new scala.util.Random()
  val domain = new BallBounceWorld()
  val bin = 10
  val ex = new CMAC(
      List((0, 1),
          (0, 1),
          (0, 1),
          (0, 1),
          (0, scala.math.Pi)),
          List(bin, bin, bin, bin, bin), 8, rng)

  def apply(agent : Agent, timesteps : Int, visuals :Boolean = true)
  {
    TestRun(domain,agent,Some(timesteps),None,visuals)
  }
}
