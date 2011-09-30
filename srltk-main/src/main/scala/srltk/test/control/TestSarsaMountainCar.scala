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
import srltk.test.domains.MountainCar._
import srltk.api.domain.State

object TestSarsaMountainCar {
  import TestRunMountainCar._
  def main(args: Array[String]): Unit = {
    TestRunMountainCar(        
        new EGreedySarsa(.005, .01, 0, .999, 0, rng, ex),
        episodes=100000
    );
  }
}
