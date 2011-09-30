/**
 * *****************************************************************************
 * Scala Reinforcement Learning Toolkit
 *  @author Jeshua Bratman
 *  @email jeshuabratman@gmail.com
 *
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
 * ****************************************************************************
 */
package srltk.test.domains
import srltk.api.domain._
import scala.util.Random
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import srltk.tools.utils.Bounds2D
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.tools.utils.ColorUtils
import java.awt.BasicStroke

//======================================================================

class MountainCar(rng: Random) extends Domain(MountainCar.getInitial(rng),
  MountainCar.rewardFunction,
  MountainCarRenderer) with Domain2D {
  def this() = this(new Random())
  
  def createState(d1 : Double, d2 : Double) = new MountainCarState(d1,d2,rng)
  import MountainCar._
  val bounds = new Bounds2D(xMin, xMax, xDotMin, xDotMax)
  
}

//Mountain car Constants
//Parameters as in Singh and Sutton 96 (Reinforcement Learning with Replacing Eligibility Traces) Appendix B

object MountainCar {

  val force = 0.001 //force of car's motor (0.001 in Singh & Sutton
  val frequency = 3.0 //frequency of hill sin function
  val gravity = -0.0025 //gravity factor (-.0025 in Singh & Sutton)

  //hill is a sin function
  def altitude(x: Double) = -scala.math.sin(frequency * x)
  def slope(x: Double) = scala.math.cos(frequency * x)

  val xMin = -1.2
  val xMax = 0.5
  val xDotMin = -.07 //-0.07 in Singh & Sutton
  val xDotMax = .07 // 0.07 in Singh & Sutton

  val bounds = new Bounds2D(xMin, xMax, xDotMin, xDotMax)

  def randomX(rng: Random) = (rng.nextDouble() * (xMax - xMin)) + xMin
  def randomXDot(rng: Random) = (rng.nextDouble() * (xDotMax - xDotMin)) + xDotMin
  def getInitial(rng: Random) = new MountainCarState(randomX(rng), randomXDot(rng), rng)

  def rewardFunction(s1: State, a: Action, s2: State): Double = {
    val state2 = s2.asInstanceOf[MountainCarState];
    if (state2.x >= MountainCar.xMax) 0 else -1
  }

  def main(args: Array[String]): Unit = {
    val mc = new MountainCar()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class MountainCarAction(index: Int) extends Action(index, 3) {
  override def manufacture(newInd: Int) = new MountainCarAction(newInd)
  override def name() =
    index match {
      case 0 => "Left"
      case 1 => "None"
      case 2 => "Right"
    }
}

//======================================================================

class MountainCarState(val x: Double, val xDot: Double, val rng: Random) extends State {
  import MountainCar._

  def exampleAction() = new MountainCarAction(0)

  def exampleObservation() = observation

  def getInitial = MountainCar.getInitial(rng)

  def absorbing = x >= xMax

  def successor(action: Action): MountainCarState =
    {
      val ac = action.asInstanceOf[Action];
      val direction = (if (ac.index == 0) -1 else if (ac.index == 2) 1 else 0)

      val newX = x + xDot
      //val newXDot = xDot + 0.1 * (-9.8 * mass * scala.math.cos(3 * x) + force * forcebymass - .5 * xDot)
      val newXDot = xDot + direction * force + slope(x) * gravity
      //compute new state
      if (newX < xMin)
        new MountainCarState(xMin, 0, rng)
      else if (newX >= xMax)
        new MountainCarState(xMax, 0, rng)
      else if (newXDot > xDotMax)
        new MountainCarState(newX, xDotMax, rng)
      else if (newXDot < xDotMin)
        new MountainCarState(newX, xDotMin, rng)
      else
        new MountainCarState(newX, newXDot, rng)

    }

  def observation(): Observation = {
    val r = rewardFunction(null, null, this)
    new Observation(scalala.tensor.dense.DenseVector(x,  xDot), r, this)
  }
}

//======================================================================

object MountainCarRenderer extends StateRenderer {

  import MountainCar._

  def xToYs(x: Double, size: Int, box_size: Int): Int =
    (size * box_size + (altitude(x) * box_size * size * .5) - (box_size * size * .4)).toInt

  def xToXs(x: Double, size: Int, box_size: Int): Int =
    (((x - xMin) * size / (xMax - xMin)) * box_size).toInt

  def render(s: State, g2d: Graphics2D, d: Dimension) =
    {
      val state = s.asInstanceOf[MountainCarState]

      val size = 200
      val cellWidth = (d.width - 1) / size
      val cellHeight = (d.height - 1) / size
      val boxSize = scala.math.min(cellWidth, cellHeight);

      val ratio = (xMax - xMin) / size
      val xPoints = for (i <- 0 until size)
        yield xToXs((i * ratio) + xMin, size, boxSize)
      val yPoints = for (i <- 0 until size)
        yield xToYs((i * ratio) + xMin, size, boxSize)

      g2d.setColor(new Color(0, 0, 0))
      g2d.drawPolyline(xPoints.toArray, yPoints.toArray, size)
      g2d.fillOval(xToXs(state.x, size, boxSize) - size / 30,
        (xToYs(state.x, size, boxSize)) - size / 30, size / 15, size / 15);

      //==================================================
      //draw predictions
      for (i <- 0 until predictions.length) {
        val st = predictions(i).asInstanceOf[MountainCarState];
        g2d.setColor(ColorUtils.colorProgression(i));
        val wideStroke = new BasicStroke(4.0f);
        g2d.setStroke(wideStroke);
        g2d.drawOval(xToXs(st.x, size, boxSize) - size / 40,
          (xToYs(st.x, size, boxSize)) - size / 40, size / 20, size / 20);
      }

      new Dimension(size * boxSize, size * boxSize)
    }
}
