package srltk.domains.rlstandard
import srltk.common._
import srltk.domains._
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Color
import srltk.vis.spacesconnect.SpacesWorldInterface
import srltk.utils.Bounds2D
import srltk.utils.Colors
import java.awt.BasicStroke

class PuddleWorld extends SimDomain[PuddleWorldState](
  new PuddleWorldState(),
  PuddleWorldRenderer) with Domain2D[PuddleWorldState] {
  
  def numActions() = 4;
  def createState(d1 : Double, d2 : Double) = new PuddleWorldState(d1,d2)
  val bounds = new Bounds2D(0, 1, 0, 1)
}

object PuddleWorld {
  val mov_dist = .05
  val rand_std = 0.01; //std of gaussian noise added to actions
  val bounds = new Bounds2D(0, 1, 0, 1)  //state value bounds
  //goal in top right corner
  val goal_x = .9
  val goal_y = .1

  //puddle
  val puddle1 = (.1, .45, .25, .25) //xmin,xmax,ymin,ymax 
  val puddle2 = (.45, .45, .2, .6) //xmin,xmax,ymin,ymax 
  val puddle_radius = .05
  val puddle_strength = -400 //-400 * distance to puddle center

  def inPuddle(puddle: (Double, Double, Double, Double), x: Double, y: Double) = {
    (x > (puddle._1 - puddle_radius)) && (x < (puddle._2 + puddle_radius)) &&
      (y > (puddle._3 - puddle_radius)) && (y < (puddle._4 + puddle_radius))
  }
  
  def distToEdge(puddle: (Double, Double, Double, Double), x: Double, y: Double) = {
    def dists = List(
      x - (puddle._1 - puddle_radius),
      x - (puddle._2 + puddle_radius),
      y - (puddle._3 - puddle_radius),
      y - (puddle._4 + puddle_radius)).map(scala.math.pow(_, 2))

    scala.math.sqrt(dists.reduceLeft(scala.math.min(_, _)))
  }
  
  def distToEdge(x: Double, y: Double): Double = {
    val p1 = if (inPuddle(puddle1, x, y)) distToEdge(puddle1, x, y) else 0
    val p2 = if (inPuddle(puddle2, x, y)) distToEdge(puddle2, x, y) else 0
    scala.math.max(p1, p2)
  }
  
  def getReward(x: Double, y: Double) = {
    if (atGoal(x, y)) 0 else (puddle_strength * distToEdge(x, y) - 1)
  }

  def atGoal(x: Double, y: Double) = x > goal_x && y < goal_y

  def rewardFunction(s1: PuddleWorldState, a: Action, s2: PuddleWorldState): Double = {
    val state2 = s2.asInstanceOf[PuddleWorldState];
    getReward(state2.x, state2.y)
  }

  def getInitial = new PuddleWorldState(GlobalRNG.nextDouble, GlobalRNG.nextDouble)

  def main(args: Array[String]): Unit = {
    val mc = new PuddleWorld()
    val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
}

//======================================================================

class PuddleWorldAction(index: Int) extends IntAction(index) {
  override def toString() =
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

class PuddleWorldState(val x: Double, val y: Double) extends SimState[PuddleWorldState] {
  def this() = this(0, 0)
  import PuddleWorld._
  def getInitial = PuddleWorld.getInitial
  def isAbsorbing = atGoal(x, y)
  def copy() = new PuddleWorldState(x,y);
  def successor(action: Action): PuddleWorldState =
    {
      val ac = action.asInstanceOf[IntAction].n;

      //add gaussian noise centered at with std 
      val rand = (GlobalRNG.nextGaussian()) * rand_std

      val newY =
        if (ac==0) y - mov_dist
        else if (ac == 1) y + mov_dist else y;
      val newX =
        if (ac == 3) x - mov_dist
        else if (ac == 2) x + mov_dist else x;

      val newYBounded = scala.math.max(0, scala.math.min(1, newY + rand))
      val newXBounded = scala.math.max(0, scala.math.min(1, newX + rand))

      new PuddleWorldState(newXBounded, newYBounded)
    }
}

//======================================================================

object PuddleWorldRenderer extends SimStateRenderer[PuddleWorldState] {
  import PuddleWorld._

  def render(s: PuddleWorldState, g2d: Graphics2D, dimension: Dimension) = {
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
      val d = (distToEdge(i, j) / puddle_radius).toFloat
      g2d.setColor(new Color(0, 0, 0, d))
      g2d.fillOval((i * w).toInt, (j * w).toInt, (.008 * w).toInt, (.008 * w).toInt)
    }
    g2d.setColor(new Color(1, 0, 0))
    val radius = .05 * w
    g2d.fillOval(
      (s.x * w - radius).toInt,
      (s.y * h - radius).toInt,
      radius.toInt * 2, radius.toInt * 2);
    dimension
  }
}

//======================================================================

