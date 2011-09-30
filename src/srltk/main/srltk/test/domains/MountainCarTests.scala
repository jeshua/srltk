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

object MountainCarTests {
  val rng = new scala.util.Random

  class Policy1(gamma: Double = .99) extends TestPolicy {
    def V2D(x: Double, y: Double) = getValue(new MountainCarState(x, y, rng))
    var i = 0
    var cache = new scala.collection.mutable.HashMap[(Double, Double), Double]

    def getEpVal(s: MountainCarState): Double = {
      if (s.absorbing) 0
      else {
        val a = s.successor(getAction(s))
        -1 + gamma * getEpVal(a)
      }
    }

    override def getValue(s: State) : Double = {
      var state = s.asInstanceOf[MountainCarState];
      val st = (state.x, state.xDot)
      if (cache.contains(st)) {
        try{
          cache(st)
        } catch {
          case _ => {cache.remove(st)
          getValue(s)}
        }
      } else {

        val values = for (i <- 0 to 20)
          yield getEpVal(state)

        val v = values.foldLeft(0.0)(_ + _) / values.length
        cache.update((state.x, state.xDot), v)
        v
      }
    }

    def getInitialState() = MountainCar.getInitial(rng)

    def getAction(s: State) = {
      val state = s.asInstanceOf[MountainCarState]
      val x = state.x
      val v = state.xDot

      if (rng.nextDouble() < .2) new MountainCarAction(rng.nextInt(3))
      else {
        if (v <= 0.0)
          new MountainCarAction(0)
        else
          new MountainCarAction(2)
      }
    }
  }

  //======================================================================
  //show this policy
  def main(args: Array[String]): Unit = {

    //show value function
    val p = new Policy1(.99)
    val visualizer =
      new ValueFunctionVisualizer2D(MountainCar.bounds,
        p.V2D, false)

    //animate policy

    val mc = new MountainCar()
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
