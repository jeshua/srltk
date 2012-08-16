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

class MountainCar extends SimDomain[MountainCarState](
    MountainCar.getInitial,
    MountainCarRenderer) with Domain2D[MountainCarState] {
  
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
  val x_max = 0.6
  val x_goal = .5
  val xdot_min = -.08 //-0.07 in Singh & Sutton
  val xdot_max = .08 // 0.07 in Singh & Sutton

  val bounds = new Bounds2D(x_min, x_max, xdot_min, xdot_max)
  
  def randomX()    = (GlobalRNG.nextDouble() * (x_max - x_min)) + x_min
  def randomXDot() = (GlobalRNG.nextDouble() * (xdot_max - xdot_min)) + xdot_min
  def getInitial() = new MountainCarState(-.524,0.0)//new MountainCarState(randomX, randomXDot)

  def rewardFunction(s1: MountainCarState, a: Action, s2: MountainCarState): Double = {
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

class MountainCarState(val x: Double, val xdot: Double) extends SimState[MountainCarState] {
  import MountainCar._
  
  def getInitial = MountainCar.getInitial
  def isAbsorbing = x >= x_goal
  def copy = new MountainCarState(x,xdot);
  
  def successor(action: Int): MountainCarState =
    {
      val direction = (if (action == 0) -1 else if (action == 2) 1 else 0)
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

object MountainCarRenderer extends SimStateRenderer[MountainCarState] {

  import MountainCar._

  def xToYs(x: Double, size: Int, box_size: Int): Int =
    (size * box_size + (altitude(x) * box_size * size * .5) - (box_size * size * .4)).toInt

  def xToXs(x: Double, size: Int, box_size: Int): Int =
    (((x - x_min) * size / (x_max - x_min)) * box_size).toInt

  def render(action : Int, state: MountainCarState,  g2d: Graphics2D, d: Dimension) =
    {  
	  val size = 200
      val cellWidth = (d.width - 1) / size
      val cellHeight = (d.height - 1) / size
      val boxSize = scala.math.min(cellWidth, cellHeight);

      val ratio = (x_max - x_min) / size
      val xPoints = for (i <- 0 until size)
        yield xToXs((i * ratio) + x_min, size, boxSize)
      val yPoints = for (i <- 0 until size)
        yield xToYs((i * ratio) + x_min, size, boxSize)
      
 g2d.setStroke(new BasicStroke(size/40f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL));
      g2d.setColor(new Color(0, 0, 0))
      g2d.drawPolyline(xPoints.toArray, yPoints.toArray, size)
      val carx_min = xToXs(state.x, size, boxSize) - size / 30
      val cary_min = xToYs(state.x, size, boxSize) - size / 30
      val carsize = size/15
      
      if(action != 1){
        val carx_mid = carx_min + carsize/2
        val line = 
            if(action == 2)
              new Line2D.Double(carx_mid,cary_min+carsize/2,carx_mid+carsize*1.5,cary_min+carsize/2)
            else
              new Line2D.Double(carx_mid,cary_min+carsize/2,carx_mid-carsize*1.5,cary_min+carsize/2)
        g2d.setColor(new Color(150, 150, 150))
        g2d.setStroke(new BasicStroke(carsize/3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL));            
        
        drawArrow(g2d,line)
      }
      
      g2d.setColor(new Color(0, 255, 0))
      g2d.fillOval(carx_min,cary_min, carsize,carsize);
      
      new Dimension(size * boxSize, size * boxSize)
    }
  //==================================================
  //ARROWS
  
  
  def drawArrow(g2d : Graphics2D, arrow_line : Line2D.Double) = { 
    val arrow_head= new Polygon();  
    arrow_head.addPoint( 0,5);
    arrow_head.addPoint( -5, -5);
    arrow_head.addPoint( 5,-5);
    g2d.draw(arrow_line)
    val arrow_tx = new AffineTransform();
    val angle = math.atan2(arrow_line.y2-arrow_line.y1, arrow_line.x2-arrow_line.x1);
    arrow_tx.translate(arrow_line.x2, arrow_line.y2);
    arrow_tx.rotate((angle-math.Pi/2d));  

    val g = g2d.create().asInstanceOf[Graphics2D];
    g.setTransform(arrow_tx);   
    g.fill(arrow_head);
    g.dispose();
}
  
}
