package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import scala.util.Random
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import srltk.utils.Bounds2D
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.utils.Colors
import java.awt.BasicStroke
import java.awt.geom.AffineTransform
import java.awt.geom.Line2D
import java.awt.Polygon

//======================================================================

class GridWorld(state: GridWorldState = GridWorld.default_initial) extends SimDomain[GridWorldState](
  state, GridWorldRenderer) {
  def numActions() = 4;
}
object GridWorld {
  val W = Maze.W;
  val N = Maze.N;
  val S = Maze.S;
  val G = Maze.G;
  val maze_data: List[Array[Int]] = List(
    Array(S,S,S,S,0),
    Array(0,S,S,S,S),
    Array(0,S,0,S,0),
    Array(G,0,0,0,0));
  
 /* val maze_data: List[Array[Int]] = List(
    Array(S,S,S,S,0), 
    Array(0,S,S,S,S),
    Array(0,0,0,0,G));*/
  val default_maze = new Maze(maze_data.toArray)
  val default_initial = new GridWorldState(0, 0, default_maze).getInitial()
  def main(args: Array[String]): Unit = {
    val mc = new GridWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}
class GridWorldAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "North"
      case 1 => "South"
      case 2 => "East"
      case 3 => "West"
    }
}
class GridWorldState(val x: Int, val y: Int, val maze: Maze) extends SimState[GridWorldState] {
  import GridWorld._
  def getInitial() = new GridWorldState(maze.width()-1,1, maze)
  def isAbsorbing = false; //maze.isGoal(x,y)
  def getReward() : Double = if (maze.isGoal(x, y)) 1d else -.001
  def copy = new GridWorldState(x, y, maze);
  def successor(action: Int): GridWorldState = {
    if (maze.isGoal(x, y))
      getInitial
    else {
      if (maze.legalMove(x, y, action)) {
        if (action == 0) //north
          new GridWorldState(x, y - 1, maze)
        else if (action == 1) //south
          new GridWorldState(x, y + 1, maze)
        else if (action == 2) //east
          new GridWorldState(x + 1, y, maze)
        else
          new GridWorldState(x - 1, y, maze)
      } else this
    }
  }
}
object GridWorldRenderer extends SimStateRenderer[GridWorldState] {
  import GridWorld._
  def render(action: Int, state: GridWorldState, g2d: Graphics2D, d: Dimension) = {
    MazeGFX.draw(g2d, d, state.maze, state.x, state.y, null, null)
  }
}

//======================================================================

class LineWorld(state: LineWorldState = LineWorld.default_initial) extends SimDomain[LineWorldState](
  state, LineWorldRenderer) {
  def numActions() = 2;
}
object LineWorld {
  val G = Maze.G;
  val maze_data: List[Array[Int]] = List(
    Array(0),
    Array(0),
    Array(0),
    Array(0),
    Array(0),
    Array(G));
  val default_maze = new Maze(maze_data.toArray)
  val default_initial = new LineWorldState(0, 0, default_maze)
  def main(args: Array[String]): Unit = {
    val mc = new LineWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}
class LineWorldAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "North"
      case 1 => "South"
    }
}
class LineWorldState(val x: Int, val y: Int, val maze: Maze) extends SimState[LineWorldState] {
  import LineWorld._
  def getInitial() = new LineWorldState(0, maze.width()-1, maze)
  def isAbsorbing = maze.isGoal(x,y)
  def getReward() = if (maze.isGoal(x, y)) 1d else 0
  def copy = new LineWorldState(x, y, maze);
  def successor(action: Int): LineWorldState = {
    if (maze.isGoal(x, y))
      getInitial
    else {
      if (maze.legalMove(x, y, action)) {
        if (action == 0) //north
          new LineWorldState(x, y - 1, maze)
        else
          new LineWorldState(x, y + 1, maze)

      } else this
    }
  }
}
object LineWorldRenderer extends SimStateRenderer[LineWorldState] {
  import LineWorld._
  def render(action: Int, state: LineWorldState, g2d: Graphics2D, d: Dimension) = {
    MazeGFX.draw(g2d, d, state.maze, state.x, state.y, null, null)
  }
}

