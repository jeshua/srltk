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

class PoleBalance extends SimDomain[PoleBalanceState](
  PoleBalance.getInitial,
  PoleBalanceRenderer) {

  def numActions() = 2;
}

//Mountain car Constants
//Parameters as in Singh and Sutton 96 (Reinforcement Learning with Replacing Eligibility Traces) Appendix B
object PoleBalance {

  val num_actions = 2;

  val gravity = 9.8d;
  val mass_cart = 1.0d;
  val mass_pole = 0.1d;
  val mass_total = mass_cart + mass_pole;
  val length = 0.5d;
  val mass_len_pole = mass_pole * length;
  val force_mag = 10.0d;
  val tau = .03d; //0.02;
  val four_thirds = 1.333333333333333d;
  val fail_degrees = .3d; //Math.PI/2;//0.2094384;

  val max_x = 2.4d;
  val min_x = -2.4d;
  val max_v = 2d;
  val min_v = -2d;
  val max_a = fail_degrees;
  val min_a = -fail_degrees;
  val max_av = 2d;
  val min_av = -2d;

  def getInitial() = new PoleBalanceState(0, 0, 0, 0);
  
  def bounds() : List[Bounds1D] = 
		  List(Bounds1D(min_x,max_x),
		      Bounds1D(min_v,max_v),
		      Bounds1D(min_a,max_a),
		      Bounds1D(min_av,max_av))
		      
  //for testing
  def main(args: Array[String]): Unit = {
    val mc = new PoleBalance()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class PoleBalanceAction(val index: Int) extends IntAction(index) {
  override def toString() =
    index match {
      case 0 => "Left"
      case 1 => "Right"
    }
}

//======================================================================

class PoleBalanceState(val x: Double, val xdot: Double, val theta: Double, val thetadot: Double)
  extends SimState[PoleBalanceState] {
  import PoleBalance._

  def getInitial = PoleBalance.getInitial
  def isAbsorbing = fail()

  def fail(): Boolean = theta < min_a || theta > max_a || x < min_x || x > max_x

  def copy = new PoleBalanceState(x, xdot, theta, thetadot);

  def successor(action: Int): PoleBalanceState =
    {
      val force = if (action > 0) force_mag else -force_mag;
      val costheta = math.cos(theta);
      val sintheta = math.sin(theta);

      val temp = (force + mass_len_pole * thetadot * thetadot * sintheta) / mass_total;
      val thetaacc = (gravity * sintheta - costheta * temp) / (length * (four_thirds - mass_pole * costheta * costheta
        / mass_total));

      val xacc = temp - mass_len_pole * thetaacc * costheta / mass_total;

      var new_x = x + tau * xdot;
      var new_xdot = xdot + tau * xacc;
      var new_theta = theta + tau * thetadot;
      var new_thetadot = thetadot + tau * thetaacc;

      if (new_xdot > max_v) new_xdot = max_v;
      if (new_xdot < min_v) new_xdot = min_v;
      if (new_thetadot > max_av) new_thetadot = max_av;
      if (new_thetadot < min_av) new_thetadot = min_av;
      new PoleBalanceState(new_x, new_xdot, new_theta, new_thetadot)
    }
}

//======================================================================

object PoleBalanceRenderer extends SimStateRenderer[PoleBalanceState] {

  import PoleBalance._

  def render(action : Int, st: PoleBalanceState, g2: Graphics2D, d: Dimension) = {
    val width = d.getWidth();
    val int_w: Int = width.toInt
    val height = d.getHeight();
    val int_h = height.toInt
    g2.setColor(new Color(255, 255, 255));

    g2.fillRect(0, 0, int_w, int_h);

    val cart_width = width / 10;
    val cart_height = cart_width / 3;

    val x_scale = width / (max_x - min_x);
    val minx = min_x * x_scale;
    val maxx = min_x * x_scale;
    val x = (st.x - min_x) * x_scale;
    val y = 3.0 / 4.0 * height;
    //draw cart
    g2.setColor(new Color(0, 0, 0));
    val rect = new Rectangle2D.Double(
      (x - cart_width / 2),
      (y - cart_height / 2),
      cart_width, cart_height);
    g2.draw(rect);

    //draw pole
    val pole_x_start = x;
    val pole_y_start = y - cart_height / 2;

    val theta = st.theta
    val pole_len = length * x_scale;

    val pole_x_end = x + math.sin(theta) * pole_len;
    val pole_y_end = y - cart_height - math.cos(theta) * pole_len; ;

    val pole = new Line2D.Double(pole_x_start, pole_y_start, pole_x_end, pole_y_end);
    g2.draw(pole)
    d
  }
}
