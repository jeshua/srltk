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
package srltk.test.domains
import srltk.api.domain._
import srltk.api.driver.SimpleDriver
import srltk.vis.StateViewer
import srltk.vis.ActivePlot
import srltk.vis.ValueFunctionVisualizer2D

object BallBounceWorldTests {
  val rng = new scala.util.Random

  class Policy1(gamma: Double = .9) extends TestPolicy {
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]

    def getEpVal(s: BallBounceWorldState): Double = {
      if (s.absorbing) 0
      else {
        val a = s.successor(getAction(s))
        -1 + gamma * getEpVal(a)
      }
    }

    override def getValue(s: State) = {
      0
    }

    def getInitialState() = BallBounceWorldState()

    def getAction(s: State) = {
      val state = s.asInstanceOf[BallBounceWorldState];
      new BallBounceWorldAction(0) //rng.nextInt(4))

    }
  }
}
