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

class GridWorld() extends Domain(
  new GridWorldState(),
  GridWorld.rewardFunction,
  GridWorldRenderer) {
}

object GridWorld {
  val width = 10
  val height = 10
  val numStates = width * height
  var rng: Random = new Random()

  def stateToInt(s: GridWorldState) = {
    if (s.x >= width || s.x < 0 || s.y >= height || s.y < 0)
      throw new IllegalArgumentException("State out of range: x=" + s.x + ", y=" + s.y)
    s.x + s.y * width
  }

  val goalX = 8
  val goalY = 4
  def atGoal(x: Int, y: Int) = x == goalX && y == goalY

  def rewardFunction(s1: State, a: Action, s2: State): Double = {
    val state2 = s2.asInstanceOf[GridWorldState];
    if (GridWorld.atGoal(state2.x, state2.y)) 0 else -1
  }

  def getInitial = new GridWorldState(rng.nextInt(width), rng.nextInt(height))

  def main(args: Array[String]): Unit = {
    val mc = new GridWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class GridWorldAction(index: Int) extends Action(index, 4) {
  override def manufacture(newInd: Int) = new GridWorldAction(newInd)
  override def name() =
    index match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
    }
}

object GridWorldAction {
  def get(name: String): GridWorldAction = {
    val n = name match {
      case "North" => 0; case "South" => 1; case "East" => 2; case "West" => 3;
    }
    new GridWorldAction(n)
  }

}
//======================================================================

class GridWorldState(val x: Int, val y: Int) extends TabularState {
  def this() = this(0, 0)
  import GridWorld._

  //for tabular state
  def numStates() = GridWorld.numStates
  def index() = stateToInt(this)

  def getInitial = GridWorld.getInitial
  def exampleAction = new GridWorldAction(0)
  def exampleObservation = observation

  def absorbing = atGoal(x, y)

  def successor(action: Action): GridWorldState =
    {
      val ac = action.asInstanceOf[Action];
      val newY =
        if (ac.name() == "North") scala.math.max(0, y - 1)
        else if (ac.name() == "South") scala.math.min(height - 1, y + 1) else y;
      val newX =
        if (ac.name() == "West") scala.math.max(0, x - 1)
        else if (ac.name() == "East") scala.math.min(width - 1, x + 1) else x;
      new GridWorldState(newX, newY)
    }

  def observation(): Observation = {
    val r = rewardFunction(null, null, this)
    val vec: mutable.Vector[Double] = sparse.SparseVector.zeros(numStates)
    vec(stateToInt(this)) = 1
    new Observation(vec, r, this)
  }
}

//======================================================================

object GridWorldRenderer extends StateRenderer {
  import GridWorld._

  def render(state: State, g2d: Graphics2D, dimension: Dimension) = {
    val s = state.asInstanceOf[GridWorldState];
    val cellWidth = (dimension.width - 1) / width
    val cellHeight = (dimension.height - 1) / height
    val boxSize = scala.math.min(cellWidth, cellHeight);

    for (i <- 0 until width) g2d.drawLine(i * boxSize, 0, i * boxSize, (height) * boxSize)
    for (i <- 0 until height) g2d.drawLine(0, i * boxSize, (width) * boxSize, i * boxSize)

    g2d.drawOval(s.x * boxSize, s.y * boxSize, boxSize, boxSize);

    new Dimension(width * boxSize, height * boxSize)
  }
}

//======================================================================

