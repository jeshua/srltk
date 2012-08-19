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
import java.awt.geom.Rectangle2D
import java.awt.geom.Line2D
import srltk.utils.Bounds1D

//======================================================================

class CartPoleSwingup extends SimDomain[CartPoleSwingupState](CartPoleSwingup.getInitial,CartPoleSwingupRenderer) {
  def numActions() = 2;
}
object CartPoleSwingup {
  val num_actions = 2;
  
  val gravity = 9.81d;
  val mass_cart = .5d;
  val mass_pole = .6d;//.6  
  val length = 2d;
  val mass_len_pole = mass_pole * length;
  val force_mag = 5.0d;//10
  val tau = .04d; //0.02;
  
  val target_theta_thresh    = .2d;//.05 
  val target_thetadot_thresh = .5d;//.5 
      
  val max_x = 4d;
  val min_x = -4d;
  val max_v = 4d;
  val min_v = -4d;
  val max_a = math.Pi
  val min_a = -math.Pi
  val max_av = 6d;
  val min_av = -6d;

  def getInitial() = new CartPoleSwingupState(0, 0, 0, 0)
  
  def bounds() : List[Bounds1D] = 
		  List(Bounds1D(min_x,max_x),
		      Bounds1D(min_v,max_v),
		      Bounds1D(min_a,max_a),
		      Bounds1D(min_av,max_av))
		      
  //for testing
  def main(args: Array[String]): Unit = {
    val mc = new CartPoleSwingup()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class CartPoleSwingupAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "Left"
      case 1 => "Right"
    }
}

//======================================================================

class CartPoleSwingupState(val x: Double, val xdot: Double, val theta: Double, val thetadot: Double)
  extends SimState[CartPoleSwingupState] {
  import CartPoleSwingup._

  def getInitial = CartPoleSwingup.getInitial
  def isAbsorbing = succeed()
  
  def succeed() = {
    math.abs(math.abs(theta)-math.Pi) < target_theta_thresh && math.abs(thetadot) < target_thetadot_thresh; 
  }

  def copy = new CartPoleSwingupState(x, xdot, theta, thetadot);

  def successor(action: Int): CartPoleSwingupState =
    {
      val dir = if (action > 0) force_mag else -force_mag;
      
      val force = dir
        if(action == 0 && x <= min_x) 0
        else if(action == 1 && x >= max_x) 0
        else dir
      
      val costheta = math.cos(theta);
      val sintheta = math.sin(theta);
      val sin2theta = sintheta*sintheta;
      val tdotsq = thetadot*thetadot
      
      val xdd = force + ((mass_pole * sintheta)/(mass_cart+mass_pole*sin2theta)) *
          (length * tdotsq + gravity * costheta)
      val tdd = (-force*costheta - mass_pole*length*tdotsq*costheta*sintheta - (mass_pole+mass_cart)*gravity*sintheta)/ 
      (length * (mass_cart + mass_pole*sin2theta))

      var new_x = x + tau * xdot;
      var new_xdot = xdot + tau * xdd;
      var new_theta = theta + tau * thetadot;
      var new_thetadot = thetadot + tau * tdd;

      if (new_x > max_x) {
        new_x = max_x;
        new_xdot = 0;
      }
      if (new_x < min_x){
        new_x = min_x;
        new_xdot = 0;
      }
      if (new_xdot > max_v) new_xdot = max_v;
      if (new_xdot < min_v) new_xdot = min_v;
      if (new_thetadot > max_av) new_thetadot = max_av;
      if (new_thetadot < min_av) new_thetadot = min_av;
      if (new_theta > max_a) new_theta = min_a;
      if (new_theta < min_a) new_theta = max_a;
      new CartPoleSwingupState(new_x, new_xdot, new_theta, new_thetadot)
    }
}

//======================================================================

object CartPoleSwingupRenderer extends SimStateRenderer[CartPoleSwingupState] {

  import CartPoleSwingup._

  def render(action : Int, st: CartPoleSwingupState, g2: Graphics2D, d: Dimension) = {
    val width = d.getWidth();
    val int_w: Int = width.toInt
    val height = d.getHeight();
    val int_h = height.toInt
    g2.setColor(new Color(255, 255, 255));

    g2.fillRect(0, 0, int_w, int_h);

    val cart_width = width / 40;
    val cart_height = cart_width / 4;

    val x_scale = width / (1.3*(max_x - min_x));
    val minx = min_x * x_scale;
    val maxx = min_x * x_scale;
    val x = (st.x - min_x) * x_scale;
    val y = 1.0 / 2.0 * height;
    //draw cart
    g2.setColor(new Color(0, 0, 0));
    val rect = new Rectangle2D.Double(
      (x - cart_width / 2),
      (y - cart_height / 2),
      cart_width, cart_height);
    g2.draw(rect);

    //draw pole
    val pole_x_start = x;
    val pole_y_start = y;

    val theta = st.theta
    val pole_len = length * x_scale;

    val pole_x_end = x + math.sin(theta) * pole_len;
    val pole_y_end = pole_y_start + math.cos(theta) * pole_len;
    
    g2.drawString("(%.3f,%.3f,%.3f,%.3f)".format(st.x,st.xdot,st.theta,st.thetadot),10,int_h-40)
    
    val pole = new Line2D.Double(pole_x_start, pole_y_start, pole_x_end, pole_y_end);
    g2.draw(pole)
    d
  }
}
