package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import scalala.tensor._
import scala.util.Random
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.utils.Bounds2D

class GridWorld() extends SimDomain(
  new GridWorldState(),
  GridWorld.rewardFunction,
  GridWorldRenderer) {
  val domainDescription = GridWorld.dd
}

object GridWorld {
  val width = 10
  val height = 10
  val numStates = width * height
  val dd = new DomainDescription(4,numStates)
  def stateToInt(s: GridWorldState) = {
    if (s.x >= width || s.x < 0 || s.y >= height || s.y < 0)
      throw new IllegalArgumentException("State out of range: x=" + s.x + ", y=" + s.y)
    s.x + s.y * width
  }

  val goalX = 8
  val goalY = 4
  def atGoal(x: Int, y: Int) = x == goalX && y == goalY

  def rewardFunction(s1: SimState, a: Action, s2: SimState): Double = {
    val state2 = s2.asInstanceOf[GridWorldState];
    if (GridWorld.atGoal(state2.x, state2.y)) 0 else -1
  }

  def getInitial = new GridWorldState(GlobalRNG.nextInt(width), GlobalRNG.nextInt(height))

  def main(args: Array[String]): Unit = {
    val mc = new GridWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class GridWorldAction(index: Int) extends IntAction(index) {
  override def toString() =
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

class GridWorldState(val x: Int, val y: Int) extends SimState {
  def this() = this(0, 0)
  import GridWorld._

  //for tabular state
  def numStates() = GridWorld.numStates
  def index() = stateToInt(this)

  def getInitial = GridWorld.getInitial
  def absorbing = atGoal(x, y)

  def successor(action: Action): GridWorldState =
    {
      val ac = action.asInstanceOf[IntAction].n
      val newY =
        if (ac == 0) scala.math.max(0, y - 1)
        else if (ac == 1) scala.math.min(height - 1, y + 1) else y;
      val newX =
        if (ac == 3) scala.math.max(0, x - 1)
        else if (ac == 2) scala.math.min(width - 1, x + 1) else x;
      new GridWorldState(newX, newY)
    }

  def observation(): SimObservation = {
    val r = rewardFunction(null, null, this)
    val vec = for(i <- 0 until numStates) yield (if(i == stateToInt(this)) 1d else 0d)    
    new SimObservation(vec, r, this)
  }
}

//======================================================================

object GridWorldRenderer extends SimStateRenderer {
  import GridWorld._

  def render(state: SimState, g2d: Graphics2D, dimension: Dimension) = {
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

