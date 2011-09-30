package srltk.projects.tdnets.experiments

import srltk.api.domain._
import srltk.vis.spacesconnect._
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.image.BufferedImage
import java.awt.Transparency
import java.awt.Graphics
import scalala.tensor.dense.DenseVector
import scalala.tensor.VectorCol
import scala.util.Random

class BouncingBall() extends Domain(
  BouncingBallState(),
  null,
  BouncingBallRenderer) {
}

object BouncingBall {
  val ballRadius = .05
  val ballVelocity = .01
  val imgWidth = 150
  val imgHeight = 150
  
  def main(args: Array[String]): Unit = {
    val mc = new BouncingBall()
    var a = new BouncingBallAction(0)
    def run() : Unit = {for(i <- 0 until 100000) {
    	mc.state = mc.state.successor(a)
    	mc.state.observation()
    }}
    val time = srltk.tools.utils.Timer.time(run)
    println("Took "+time+"seconds to run 100,000 steps")

       val spaces = new SpacesWorldInterface(mc)
    spaces.display()
  }
 
  val rng = new Random
  val exampleAction = new BouncingBallAction(0)
  def boundTheta(theta: Double): Double =
	  if (theta > 2 * scala.math.Pi) boundTheta(theta - 2 * scala.math.Pi)
	  else if (theta < 0) boundTheta(theta + 2 * scala.math.Pi)
	  else theta
	  def boundCoord(coord: Double) = if(coord < ballRadius) ballRadius else if(coord > 1-ballRadius) 1-ballRadius else coord  
}

//======================================================================

class BouncingBallAction(index: Int) extends Action(index, 1) {
  override def manufacture(newInd: Int) = new BouncingBallAction(newInd)
  override def name() = ""
}
//======================================================================

object BouncingBallState {
  import BouncingBall._
  def apply() = new BouncingBallState(.5,.5,.1)
}
import BouncingBall._
class BouncingBallState(
		val ballX: Double,
		val ballY: Double,
		val ballTheta: Double) extends State {
	
  def getInitial = BouncingBallState()
  val rng : Random = BouncingBall.rng
  def exampleAction = BouncingBall.exampleAction
  def exampleObservation = observation
  def absorbing = false

  def successor(action: Action): BouncingBallState =
    {
      val newBallX = boundCoord(ballX + scala.math.cos(ballTheta) * ballVelocity)
      val newBallY = boundCoord(ballY + scala.math.sin(ballTheta) * ballVelocity)
      val rand = rng.nextGaussian() * scala.math.Pi / 12
      //update ball orientation
      val newBallTheta = {
        if (newBallX >= 1-ballRadius || newBallX <= ballRadius)
          boundTheta(scala.math.Pi - ballTheta + rand)
        else if (newBallY <= ballRadius || newBallY >= 1-ballRadius)
          boundTheta(-ballTheta + rand);
        else ballTheta
      }
      new BouncingBallState(newBallX, newBallY, newBallTheta)
    }

  def observation(): Observation = {
    val r = 0
    //full observation
    //new Observation(scalala.tensor.dense.DenseVector(ballX, 0, ballTheta), r, this)
    //image observation
    new Observation(BouncingBallRenderer.getObservation(this),0,this)    
  }
}

//======================================================================

object BouncingBallRenderer extends StateRenderer {
  import BouncingBall._

  def drawFilled(gc : Graphics, x: Double, y: Double, radius: Double, w : Int, h : Int) {
	  val size = radius * w
	  gc.fillOval((x * w - size).toInt,
			  (y * h - size).toInt,
			  size.toInt * 2, size.toInt * 2);
  }
  def drawBall(gc : Graphics, s : BouncingBallState, w : Int, h : Int)
  {
	  drawFilled(gc,s.ballX,s.ballY,ballRadius,w,h)
  }  
  
  def copyRegion(s : BouncingBallState)
  {
	  val rad : Int = scala.math.ceil(ballRadius * imgWidth).toInt
	  var xStart : Int = (s.ballX*imgWidth).toInt - rad
	  var yStart : Int = (s.ballY*imgHeight).toInt - rad
	  var xEnd   : Int = xStart + 2*rad
	  var yEnd   : Int = yStart + 2*rad 
	  if(xStart < 0) xStart = 0
	  if(yStart < 0) yStart = 0
	  if(xEnd >= imgWidth) xEnd = imgWidth
	  if(yEnd >= imgHeight) yEnd = imgHeight
	  for(i <- xStart until xEnd; j <- yStart until yEnd){
		  obsVec(i*imgWidth+j) = raster.getSampleDouble(i,j,0)//only 1 band since its black and white		  
	  }
  }
  
  //==================================================
  //draw to buffered image
  //*NOT THREAD SAFE*
  val img = new BufferedImage(imgWidth,imgHeight,BufferedImage.TYPE_BYTE_GRAY)
  val obsVec = DenseVector.zeros[Double](imgWidth*imgHeight)
  val obsGC = img.createGraphics()
  val pixelArray = new Array[Float](imgWidth * imgHeight)
  val raster = img.getRaster()
  var lastState : BouncingBallState = null
  val cBackground = new Color(200, 200, 200);
  val cBall = new Color(100, 100, 100);
  def getObservation(s : BouncingBallState) : VectorCol[Double] = {
    //this could be done more efficiently
	  obsGC.setColor(cBackground)
	  if(lastState == null)
		  obsGC.fillRect(0,0,imgWidth,imgHeight)
	  else
		  drawBall(obsGC, lastState, imgWidth, imgHeight)
	  
	  obsGC.setColor(cBall)
	  drawBall(obsGC, s, imgWidth, imgHeight)
	  //obsVec(i*imgWidth+j) = 
	  //there is only 1 band
	  //raster.getPixels(0,0,imgWidth,imgHeight,pixelArray)
	  
	  if(lastState != null) {
		  copyRegion(lastState)
		  copyRegion(s)		  
	  }else{
		  for(i <- 0 until imgWidth; j <- 0 until imgHeight){
			  obsVec(i*imgHeight+j) = raster.getSampleDouble(i,j,0)//only 1 band since its black and white		  
		  }
	  }
	  lastState = s
	  obsVec
  }
  
  //==================================================
  //draw to screen
  def render(state: State, g2d: Graphics2D, dimension: Dimension) = {
    val s = state.asInstanceOf[BouncingBallState];
    val w = dimension.width
    val h = dimension.height
    
    g2d.setColor(new Color(255, 0, 0));
    drawFilled(g2d, s.ballX, s.ballY, ballRadius, w, h)
    val vec = getObservation(s)
    val sample = new Array[Int](1)
    for(i <- 0 until imgWidth; j <- 0 until imgHeight) {
    	sample(0) = vec(i*imgHeight+j).toInt
    	raster.setPixel(i,j,sample)
    }
    
    g2d.drawImage(img,null,0,0)
    dimension
  }
}

//======================================================================

