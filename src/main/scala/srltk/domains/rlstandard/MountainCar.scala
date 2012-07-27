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

//======================================================================

class MountainCar() extends SimDomain(
    MountainCar.getInitial,    
    MountainCarRenderer) with Domain2D {
  
  def numActions() = 3;
  def createState(d1 : Double, d2 : Double) = new MountainCarState(d1,d2)  
  val bounds = new Bounds2D(MountainCar.x_min, MountainCar.x_max, MountainCar.xdot_min, MountainCar.xdot_max)  
}

//Mountain car Constants
//Parameters as in Singh and Sutton 96 (Reinforcement Learning with Replacing Eligibility Traces) Appendix B
object MountainCar {
  val force = 0.001 //force of car's motor (0.001 in Singh & Sutton
  val frequency = 3.0 //frequency of hill sin function
  val gravity = -0.0025 //gravity factor (-.0025 in Singh & Sutton)

  def altitude(x: Double) = -scala.math.sin(frequency * x)
  def slope(x: Double) = scala.math.cos(frequency * x)

  val x_min = -1.2
  val x_max = 0.5
  val xdot_min = -.07 //-0.07 in Singh & Sutton
  val xdot_max = .07 // 0.07 in Singh & Sutton

  val bounds = new Bounds2D(x_min, x_max, xdot_min, xdot_max)
  
  def randomX()    = (GlobalRNG.nextDouble() * (x_max - x_min)) + x_min
  def randomXDot() = (GlobalRNG.nextDouble() * (xdot_max - xdot_min)) + xdot_min
  def getInitial() = new MountainCarState(randomX, randomXDot)

  def rewardFunction(s1: SimState, a: Action, s2: SimState): Double = {
    val state2 = s2.asInstanceOf[MountainCarState];
    if (state2.x >= MountainCar.x_max) 0 else -1
  }
  //for testing
  def main(args: Array[String]): Unit = {
    val mc = new MountainCar()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class MountainCarAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "Left"
      case 1 => "None"
      case 2 => "Right"
    }
}

//======================================================================

class MountainCarState(val x: Double, val xdot: Double) extends SimState {
  import MountainCar._
  
  def getInitial = MountainCar.getInitial
  def isAbsorbing = x >= x_max
  def copy = new MountainCarState(x,xdot);
  
  def successor(action: Action): MountainCarState =
    {
      val ac = action.asInstanceOf[IntAction];
      val direction = (if (ac.n == 0) -1 else if (ac.n == 2) 1 else 0)
      val new_x = x + xdot
      val new_xdot = xdot + direction * force + slope(x) * gravity
      //compute new state
      if (new_x < x_min)
        new MountainCarState(x_min, 0)
      else if (new_x >= x_max)
        new MountainCarState(x_max, 0)
      else if (new_xdot > xdot_max)
        new MountainCarState(new_x, xdot_max)
      else if (new_xdot < xdot_min)
        new MountainCarState(new_x, xdot_min)
      else
        new MountainCarState(new_x, new_xdot)
    }
}

//======================================================================

object MountainCarRenderer extends SimStateRenderer {

  import MountainCar._

  def xToYs(x: Double, size: Int, box_size: Int): Int =
    (size * box_size + (altitude(x) * box_size * size * .5) - (box_size * size * .4)).toInt

  def xToXs(x: Double, size: Int, box_size: Int): Int =
    (((x - x_min) * size / (x_max - x_min)) * box_size).toInt

  def render(s: SimState, g2d: Graphics2D, d: Dimension) =
    {
      val state = s.asInstanceOf[MountainCarState]

      val size = 200
      val cellWidth = (d.width - 1) / size
      val cellHeight = (d.height - 1) / size
      val boxSize = scala.math.min(cellWidth, cellHeight);

      val ratio = (x_max - x_min) / size
      val xPoints = for (i <- 0 until size)
        yield xToXs((i * ratio) + x_min, size, boxSize)
      val yPoints = for (i <- 0 until size)
        yield xToYs((i * ratio) + x_min, size, boxSize)

      g2d.setColor(new Color(0, 0, 0))
      g2d.drawPolyline(xPoints.toArray, yPoints.toArray, size)
      g2d.fillOval(xToXs(state.x, size, boxSize) - size / 30,
        (xToYs(state.x, size, boxSize)) - size / 30, size / 15, size / 15);

      new Dimension(size * boxSize, size * boxSize)
    }
}
