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
import scalala.tensor._
import scala.util.Random
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.tools.utils.Bounds2D
import srltk.tools.utils.ColorUtils
import java.awt.BasicStroke

class PuddleWorld() extends Domain(
  new PuddleWorldState(),
  PuddleWorld.rewardFunction,
  PuddleWorldRenderer) with Domain2D {
    
  def createState(d1 : Double, d2 : Double) = new PuddleWorldState(d1,d2)
  val bounds = new Bounds2D(0, 1, 0, 1)
}

object PuddleWorld {
  val movementDistance = .05
  val randomStd = 0.01; //std of gaussian noise added to actions
  var rng: Random = new Random()

  //state value bounds
  val bounds = new Bounds2D(0, 1, 0, 1)

  //goal in top right corner
  val goalX = .9
  val goalY = .1

  //puddle
  val puddle1 = (.1, .45, .25, .25) //xmin,xmax,ymin,ymax 
  val puddle2 = (.45, .45, .2, .6) //xmin,xmax,ymin,ymax 
  val puddleRadius = .05
  val puddleStrength = -400 //-400 * distance to puddle center

  def inPuddle(puddle: (Double, Double, Double, Double), x: Double, y: Double) = {
    (x > (puddle._1 - puddleRadius)) && (x < (puddle._2 + puddleRadius)) &&
      (y > (puddle._3 - puddleRadius)) && (y < (puddle._4 + puddleRadius))
  }
  def distToEdge(puddle: (Double, Double, Double, Double), x: Double, y: Double) = {

    def dists = List(
      x - (puddle._1 - puddleRadius),
      x - (puddle._2 + puddleRadius),
      y - (puddle._3 - puddleRadius),
      y - (puddle._4 + puddleRadius)).map(scala.math.pow(_, 2))

    scala.math.sqrt(dists.reduceLeft(scala.math.min(_, _)))
  }
  def distToEdge(x: Double, y: Double): Double = {
    val p1 = if (inPuddle(puddle1, x, y)) distToEdge(puddle1, x, y) else 0
    val p2 = if (inPuddle(puddle2, x, y)) distToEdge(puddle2, x, y) else 0
    scala.math.max(p1, p2)
  }
  def getReward(x: Double, y: Double) = {
    if (atGoal(x, y)) 0 else (puddleStrength * distToEdge(x, y) - 1)
  }

  def atGoal(x: Double, y: Double) = x > goalX && y < goalY

  def rewardFunction(s1: State, a: Action, s2: State): Double = {
    val state2 = s2.asInstanceOf[PuddleWorldState];
    getReward(state2.x, state2.y)
  }

  def getInitial = new PuddleWorldState(rng.nextDouble, rng.nextDouble)

  def main(args: Array[String]): Unit = {
    val mc = new PuddleWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class PuddleWorldAction(index: Int) extends Action(index, 4) {
  override def manufacture(newInd: Int) = new PuddleWorldAction(newInd)
  override def name() =
    index match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
    }
}

object PuddleWorldAction {
  def get(name: String): PuddleWorldAction = {
    val n = name match {
      case "North" => 0; case "South" => 1; case "East" => 2; case "West" => 3;
    }
    new PuddleWorldAction(n)
  }

}
//======================================================================

class PuddleWorldState(val x: Double, val y: Double) extends State {
  def this() = this(0, 0)
  import PuddleWorld._

  def getInitial = PuddleWorld.getInitial
  def exampleAction = new PuddleWorldAction(0)
  def exampleObservation = observation

  def absorbing = atGoal(x, y)

  def successor(action: Action): PuddleWorldState =
    {
      val ac = action.asInstanceOf[Action];

      //add gaussian noise centered at with std 
      val rand = (rng.nextGaussian()) * randomStd

      val newY =
        if (ac.name() == "North") y - movementDistance
        else if (ac.name() == "South") y + movementDistance else y;
      val newX =
        if (ac.name() == "West") x - movementDistance
        else if (ac.name() == "East") x + movementDistance else x;

      val newYBounded = scala.math.max(0, scala.math.min(1, newY + rand))
      val newXBounded = scala.math.max(0, scala.math.min(1, newX + rand))

      new PuddleWorldState(newXBounded, newYBounded)
    }

  def observation(): Observation = {
    val r = rewardFunction(null, null, this)
    new Observation(scalala.tensor.dense.DenseVector(x, y), r, this)
  }
}

//======================================================================

object PuddleWorldRenderer extends StateRenderer {
  import PuddleWorld._

  def render(state: State, g2d: Graphics2D, dimension: Dimension) = {
    val s = state.asInstanceOf[PuddleWorldState];
    val w = dimension.width
    val h = dimension.height

    g2d.setColor(new Color(0, 0, 0))
    def drawPuddle(puddle: (Double, Double, Double, Double)) = {
      val startx = puddle._1 - .01
      val starty = puddle._3 - 0.01
      val width = (puddle._2 - puddle._1) + .02
      val height = (puddle._4 - puddle._3) + .02
      g2d.fillRect((startx * w).toInt, (starty * h).toInt, (width * w).toInt, (height * h).toInt)
    }
    //drawPuddle(puddle1)
    //drawPuddle(puddle2)

    //draw some reward points
    for (i <- 0f until 1f by .01f; j <- 0f until 1f by .01f) {
      val d = (distToEdge(i, j) / puddleRadius).toFloat
      g2d.setColor(new Color(0, 0, 0, d))
      g2d.fillOval((i * w).toInt, (j * w).toInt, (.008 * w).toInt, (.008 * w).toInt)
    }
    g2d.setColor(new Color(1, 0, 0))
    val radius = .05 * w
    g2d.fillOval(
      (s.x * w - radius).toInt,
      (s.y * h - radius).toInt,
      radius.toInt * 2, radius.toInt * 2);

    //==================================================
    //draw predictions
    for (i <- 0 until predictions.length) {
      val st = predictions(i).asInstanceOf[PuddleWorldState];
      val wideStroke = new BasicStroke(8.0f);
      g2d.setStroke(wideStroke);
      g2d.setColor(ColorUtils.colorProgression(i));
      g2d.drawOval(
        (st.x * w - radius).toInt,
        (st.y * h - radius).toInt,
        radius.toInt * 2, radius.toInt * 2);
    }

    dimension
  }
}

//======================================================================

