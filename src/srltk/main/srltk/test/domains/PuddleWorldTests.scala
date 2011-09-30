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

object PuddleWorldTests {
  val rng = new scala.util.Random

  class Policy1(gamma: Double) extends TestPolicy {
    val rng = new scala.util.Random

    def V2D(x: Double, y: Double) = getValue(new PuddleWorldState(x, y))
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]
    //= new java.util.HashMap[(Double,Double), Double] 
    def getEpVal(s: PuddleWorldState): Double = {
      if (s.absorbing) 0
      else PuddleWorld.getReward(s.x, s.y) + gamma * getEpVal(s.successor(getAction(s)))
    }
    override def getValue(s: State) = {
      var state = s.asInstanceOf[PuddleWorldState];
      if (cache.contains((state.x, state.y))) {
        cache((state.x, state.y))
      } else {
        val values = for (i <- 0 to 20)
          yield getEpVal(state)
        val v = values.foldLeft(0.0)(_ + _) / values.length
        cache((state.x, state.y)) = v
        v
      }
    }

    def getInitialState() = PuddleWorld.getInitial

    def getAction(s: State) = {
      val state = s.asInstanceOf[PuddleWorldState]
      val x = state.x
      val y = state.y
      //if(x < .5 && (y > .3&& y < .8))
      //  PuddleWorldAction.get("South")
      if ((y > 0.05 && (y * 10).toInt % 2 == 0) || x > .95)
        PuddleWorldAction.get("North");
      else
        PuddleWorldAction.get("East")

      //if (rng.nextDouble() < .5) PuddleWorldAction.get("East")
      //else PuddleWorldAction.get("North")
    }
  }

  //======================================================================
  //show this policy
  def main(args: Array[String]): Unit = {

    //show value function
    val p = new Policy1(1)
    val visualizer = new ValueFunctionVisualizer2D(PuddleWorld.bounds, p.V2D, false)

    val mc = new PuddleWorld()
    val driver = new SimpleDriver(mc, new Policy1(1))
    val sv = new StateViewer(500, 500, mc.renderer)
    sv.setLocation(500, 0)

    for (i <- 1 to 100000000) {
      val st: State = driver.step()._1
      sv.updateDisplay(st)
      Thread.sleep(20)
    }
  }

}
