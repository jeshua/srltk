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

object TestSarsaPuddleWorld {
  import PuddleWorld._
  def main(args: Array[String]): Unit = {
    TestRunPuddleWorld(        
        new EGreedySarsa(.01, .01, 0, .99, 0, new scala.util.Random(), TestRunPuddleWorld.ex),
        episodes=1000
    );
  }
}