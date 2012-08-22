package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import scala.util.Random
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Rectangle
import java.awt.geom.AffineTransform
import java.awt.geom.Ellipse2D
import srltk.utils.Bounds2D
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.utils.Colors
import java.awt.BasicStroke
import java.awt.geom.Rectangle2D
import java.awt.geom.Line2D
import srltk.utils.Bounds1D
import java.awt.Dimension
import java.awt.Font

//======================================================================

class Acrobot extends SimDomain[AcrobotState](
  Acrobot.getInitial,
  AcrobotRenderer) {
  def numActions() = 3;
}

object Acrobot {
  val num_actions = 3;
  val g           = 9.8d;
  val m1          = 1d;
  val m2          = 1d;
  val l1          = 1d;
  val l2          = 1d;
  val lc1         = .5;
  val lc2         = .5;
  val I1          = 1d;
  val I2          = 1d;

  val min_t1 = -math.Pi;
  val max_t1 = math.Pi;
  val min_t1dot = -4 * math.Pi;
  val max_t1dot = 4 * math.Pi;
  val min_t2 = -math.Pi;
  val max_t2 = math.Pi;
  val min_t2dot = -9 * math.Pi;
  val max_t2dot = 9 * math.Pi;
  val goal_pos = 1d;
  val dt = .04


  def getInitial() = new AcrobotState(0,0,0,0)
    //new AcrobotState(GlobalRNG.nextDouble()-.5, GlobalRNG.nextDouble()-.5, GlobalRNG.nextDouble()-.5, GlobalRNG.nextDouble()-.5);
  
  def bounds() : List[Bounds1D] = 
		  List(Bounds1D(min_t1,max_t1),
		      Bounds1D(min_t1dot,max_t1dot),
		      Bounds1D(min_t2,max_t2),
		      Bounds1D(min_t2dot,max_t2dot))
		      
  def calcHeight(t1 : Double,  t2 : Double) : Double = {
    val j1 = l1 * math.cos(t1)
    val j2 = -(l2 * math.sin(math.Pi / 2 - t1 - t2) + j1);
    j2
  }

  //for testing
  def main(args: Array[String]): Unit = {
    val mc = new Acrobot()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class AcrobotAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "Left"
      case 1 => "Right"
    }
}

//======================================================================

class AcrobotState(val t1: Double, val t1dot: Double, val t2: Double, val t2dot: Double)
  extends SimState[AcrobotState] {
  import Acrobot._
  val num_actions = Acrobot.num_actions;
  
  def getInitial = Acrobot.getInitial
  def isAbsorbing = success()

  def success(theta1 : Double, theta2 : Double): Boolean = {
 
    (calcHeight(theta1,theta2) > goal_pos);
  }
  def success(): Boolean = success(t1,t2)

  def copy = new AcrobotState(t1, t1dot, t2, t2dot);

  def successor(action: Int): AcrobotState =
    {
      val torque = (action - 1d);
      
      var theta1 = t1;
      var theta2 = t2;
      var theta1_dot = t1dot;
      var theta2_dot = t2dot;

      var count = 0;
      while(!success(theta1,theta2) && count < 4){
        val d1 = m1 * math.pow(lc1, 2) + m2 * (math.pow(l1, 2) + math.pow(lc2, 2) 
                                               + 2 * l1 * lc2 * math.cos(theta2)) + I1 + I2;
        val d2 = m2 * (math.pow(lc2, 2) + l1 * lc2 * math.cos(theta2)) + I2;

        val phi_2 = m2 * lc2 * g * math.cos(theta1 + theta2 - math.Pi / 2.0);
        val phi_1 = -(m2 * l1 * lc2 * math.pow(theta2_dot, 2) * math.sin(theta2) 
                      - 2 * m2 * l1 * lc2 * theta1_dot * theta2_dot * math.sin(theta2)) 
          + (m1 * lc1 + m2 * l1) * g * math.cos(theta1 - math.Pi / 2.0) + phi_2;

        val theta2_ddot = (torque + (d2 / d1) * phi_1 - m2 * l1 * lc2 * 
                           math.pow(theta1_dot, 2) * math.sin(theta2) - phi_2) / (m2 * math.pow(lc2, 2) + I2 - math.pow(d2, 2) / d1);

        val theta1_ddot = -(d2 * theta2_ddot + phi_1) / d1;

        theta1_dot += theta1_ddot * dt;
        theta2_dot += theta2_ddot * dt;
        
        theta1 += theta1_dot * dt;
        theta2 += theta2_dot * dt;
        count += 1;
      }

      if (theta1 > max_t1) theta1 = min_t1
      if (theta1 < min_t1) theta1 = max_t1
      if (theta1_dot > max_t1dot) theta1_dot = max_t1dot
      if (theta1_dot < min_t1dot) theta1_dot = min_t1dot
      if (theta2 > max_t2) theta2 = max_t2
      if (theta2 < min_t2) theta2 = min_t2
      if (theta2_dot > max_t2dot) theta2_dot = max_t2dot
      if (theta2_dot < min_t2dot) theta2_dot = min_t2dot
      
      new AcrobotState(theta1,theta1_dot,theta2,theta2_dot);
    }
}

//======================================================================

object AcrobotRenderer extends SimStateRenderer[AcrobotState] {

  import Acrobot._
    val j1X = 50;
    val j1Y = 30;
    val len1 = 25;
    val len2 = len1;
    val sz1 = 6;
    val sz2 = 4;
    val sz3 = 2;

  def render(action : Int, st: AcrobotState, g: Graphics2D, d: Dimension) = {
    val tr = g.getTransform();
    g.setColor(Color.WHITE);
    g.scale(d.width, d.height);
    g.fill(new Rectangle(1, 1));
    g.scale(.01, .01);
        
    g.setColor(Color.red);
    val goalY = j1Y - len1;
    g.drawLine(0, goalY, 100, goalY);
    g.setColor(Color.BLACK);
    
    val j2X = (len1 * math.sin(st.t1) + j1X).toInt;
    val j2Y = (len1 * math.cos(st.t1) + j1Y).toInt;
    g.drawLine(j1X, j1Y, j2X, j2Y);
    
    g.setColor(Color.BLUE);
    g.fill(new Ellipse2D.Float(j1X - sz1 / 2, j1Y - sz1 / 2, sz1, sz1));
    
    val j3X = (len2 * math.cos(math.Pi / 2 - st.t2 - st.t1) + j2X).toInt;
    val j3Y = (len2 * math.sin(math.Pi / 2 - st.t1 - st.t2) + j2Y).toInt;
    g.setColor(Color.BLACK);
    g.drawLine(j2X, j2Y, j3X, j3Y);
   
    g.setColor(Color.BLUE);
    g.fill(new Ellipse2D.Float( j2X - sz2 / 2, j2Y - sz2 / 2, sz2, sz2));
    
    g.setColor(Color.CYAN);
    g.fill(new Ellipse2D.Float(j3X - sz3 / 2, j3Y - sz3 / 2, sz3, sz3));
    val font = new Font("Arial", Font.PLAIN, 3);
    g.setFont(font);
    g.setColor(Color.BLACK);
    
    val j1 = l1 * math.cos(st.t1)
    val j2 = -(l2 * math.sin(math.Pi / 2 - st.t1 - st.t2) + j1);
    g.drawString("(%.3f,%.3f,%.3f,%.3f) H: %.3f".format(st.t1,st.t1dot,st.t2,st.t2dot,calcHeight(st.t1,st.t2)),0f,90f)
    
    g.setTransform(tr);
    
    d
  }
}
