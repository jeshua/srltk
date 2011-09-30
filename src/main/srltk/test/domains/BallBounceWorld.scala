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

class BallBounceWorld() extends Domain(
  BallBounceWorldState(),
  null,
  BallBounceWorldRenderer) {
}

object BallBounceWorld {
  val ballRadius = .02
  val ballGrowSpeed = .00003
  val ballAuraRadius = .1
  val agentRadius = .015
  val agentLOSRadius = .3
  val agentForce = .06
  val ballVelocity = .04
  var drawAgent = false 
  var useLOS = false

  private def distance(x1: Double, y1: Double, x2: Double, y2: Double) =
    scala.math.sqrt(scala.math.pow((x1 - x2), 2) + scala.math.pow((y1 - y2), 2));

  def withinBall(state: BallBounceWorldState): Boolean =
    (distance(state.agentX, state.agentY, state.ballX, state.ballY)) - agentRadius < ballRadius
  def withinAura(state: BallBounceWorldState): Boolean =
    (distance(state.agentX, state.agentY, state.ballX, state.ballY)) - agentRadius < ballAuraRadius
  def withinLOS(state: BallBounceWorldState): Boolean =
    (distance(state.agentX, state.agentY, state.ballX, state.ballY)) - ballAuraRadius < agentLOSRadius

  def getReward(state: BallBounceWorldState): Double = {
    if (withinBall(state)) -1
    else if (withinAura(state)) .01
    else 0
  }

  def main(args: Array[String]): Unit = {
    val mc = new BallBounceWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class BallBounceWorldAction(index: Int) extends Action(index, 4) {
  override def manufacture(newInd: Int) = new BallBounceWorldAction(newInd)
  override def name() =
    index match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
    }
}
//======================================================================

object BallBounceWorldState {
  import BallBounceWorld._
  def apply() = new BallBounceWorldState(0d, 0d, 0.5, 0.5, 0.1)
}

class BallBounceWorldState(
  val agentX: Double,
  val agentY: Double,
  val ballX: Double,
  val ballY: Double,
  val ballTheta: Double) extends State {
  import BallBounceWorld._

  def getInitial = BallBounceWorldState()
  val rng = new scala.util.Random
  def exampleAction = new BallBounceWorldAction(0)
  def exampleObservation = observation
  def absorbing = false

  def boundTheta(theta: Double): Double =
    if (theta > 2 * scala.math.Pi) boundTheta(theta - 2 * scala.math.Pi)
    else if (theta < 0) boundTheta(theta + 2 * scala.math.Pi)
    else theta
  def boundCoord(coord: Double) = scala.math.max(0, scala.math.min(1, coord))

  def successor(action: Action): BallBounceWorldState =
    {

      //update agent location
      val ac = action.asInstanceOf[Action];
      val newY =
        if (ac.name() == "North") agentY - agentForce
        else if (ac.name() == "South") agentY + agentForce else agentY;
      val newX =
        if (ac.name() == "West") agentX - agentForce
        else if (ac.name() == "East") agentX + agentForce else agentX;

      val newYBounded = boundCoord(newY)
      val newXBounded = boundCoord(newX)
      //update ball location
      val newBallX = boundCoord(ballX + scala.math.cos(ballTheta) * ballVelocity)
      val newBallY = boundCoord(ballY + scala.math.sin(ballTheta) * ballVelocity)
      val rand = rng.nextGaussian() * scala.math.Pi / 12
      //update ball orientation
      val newBallTheta = {
        if (newBallX >= 1 || newBallX <= 0)
          boundTheta(scala.math.Pi - ballTheta + rand)
        else if (newBallY <= 0 || newBallY >= 1)
          boundTheta(-ballTheta + rand);
        else ballTheta
      }

      new BallBounceWorldState(newXBounded, newYBounded, newBallX, newBallY, newBallTheta)
    }

  def observation(): Observation = {
    val r = getReward(this)

    //full observation
    if (withinLOS(this) || !useLOS)
      new Observation(scalala.tensor.dense.DenseVector(agentX, agentY, ballX, ballY, ballTheta), r, this)
    else
      new Observation(scalala.tensor.dense.DenseVector(agentX, agentY, ballX, 0, ballTheta), r, this)

  }
}

//======================================================================

object BallBounceWorldRenderer extends StateRenderer {
  import BallBounceWorld._

  def render(state: State, g2d: Graphics2D, dimension: Dimension) = {
    val s = state.asInstanceOf[BallBounceWorldState];
    val w = dimension.width
    val h = dimension.height

    def drawFilled(x: Double, y: Double, radius: Double) {
      val size = radius * w
      g2d.fillOval((x * w - size).toInt,
        (y * h - size).toInt,
        size.toInt * 2, size.toInt * 2);
    }
    def drawOutline(x: Double, y: Double, radius: Double) {
      val size = radius * w
      g2d.drawOval((x * w - size).toInt,
        (y * h - size).toInt,
        size.toInt * 2, size.toInt * 2);
    }
    def drawTheta(x: Double, y: Double, theta : Double) {
      
      g2d.drawLine((x * w).toInt,
        (y * h ).toInt,
        (x*w+scala.math.cos(theta)*w/10).toInt,
        (y*h+scala.math.sin(theta)*w/10).toInt)
    }
    if (withinBall(s)) {
      g2d.setColor(new Color(100, 50, 50))
      g2d.fillRect(0, 0, w, h)
    }

    val inLOS = withinLOS(s);
    val stroke = g2d.getStroke
    val wideStroke = new BasicStroke(3.0f);
      
    //==================================================
    //Draw ball
    val alpha = if (!inLOS) 120 else 255
    g2d.setColor(new Color(255, 0, 0, alpha));
    drawFilled(s.ballX, s.ballY, ballRadius)
    if(drawAgent){
    g2d.setColor(new Color(255, 0, 0, alpha - 100))
    drawFilled(s.ballX, s.ballY, ballAuraRadius)
    }
    
    g2d.setStroke(wideStroke);
    g2d.setColor(new Color(0, 0, 0));
    drawTheta(s.ballX,s.ballY,s.ballTheta)
    g2d.setStroke(stroke)
    
    //==================================================
    //Draw agent location
    if(drawAgent)
    {
    	g2d.setColor(new Color(0, 0, 150))
    	drawFilled(s.agentX, s.agentY, agentRadius)
    	g2d.setColor(new Color(0, 0, 150, 20))
    	drawOutline(s.agentX, s.agentY, agentLOSRadius)
    }

    //==================================================
    //Draw annotations around ball when within aura
    if (withinAura(s) && drawAgent) {
      val stroke = g2d.getStroke
      val wideStroke = new BasicStroke(8.0f);
      g2d.setStroke(wideStroke);
      g2d.setColor(new Color(100, 100, 255))
      drawOutline(s.agentX, s.agentY, agentRadius)
      g2d.setStroke(stroke)
    }

    //==================================================
    //Draw predictions
    for (i <- 0 until predictions.length) {
      val s = predictions(i).asInstanceOf[BallBounceWorldState];
      g2d.setStroke(wideStroke);
      g2d.setColor(ColorUtils.colorProgression(i));
      drawOutline(s.ballX, s.ballY, ballRadius)
      if(drawAgent)
    	  drawOutline(s.ballX, s.ballY, ballAuraRadius)
      drawTheta(s.ballX,s.ballY,s.ballTheta)
    }
    dimension
  }
}

//======================================================================

