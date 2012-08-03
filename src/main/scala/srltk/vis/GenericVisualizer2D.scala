package srltk.vis

import srltk.utils.Bounds2D
import javax.swing._
import java.awt._
import java.awt.Graphics2D
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import scala.collection.immutable.NumericRange


class GenericVisualizer2D(
  val bounds: Bounds2D,
  var colorizer: (Double, Double) => Color,
  integer: Boolean = false, resolution: Int = 100) {

  val xMin = bounds.xMin
  val xMax = bounds.xMax
  val yMin = bounds.yMin
  val yMax = bounds.yMax
    
  val step: Float = 1.0f / resolution
  val xSize = xMax - xMin + (if(integer) 1d else 0d)
  val ySize = yMax - yMin + (if(integer) 1d else 0d)
  val rangeX = (if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / (xSize)).toFloat).toArray
  val rangeY = (if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / (ySize)).toFloat).toArray
      
  var colors = new Array[Color](rangeX.length*rangeY.length)
  
  val vis = new Visualizer2D(500, 500, f_paint)
  
  def getFrame = vis
  def update() = {
	  f_remake_v();
	  vis.repaint() 
  }  
  def reRender() = update()
  vis.setVisible(true)

  def f_remake_v(): Unit =
    {    
      @inline def getX(percent: Float) = (xSize) * percent + xMin
      @inline def getY(percent: Float) = (ySize) * percent + yMin
      //calculate value for every point in the range and then find min and max value
      var x = 0;
      var y = 0;
      while(x < rangeX.length){
        y = 0
        while(y < rangeY.length){
          val v = colorizer(getX(rangeX(x)), getY(rangeY(y)));
          colors(x * rangeY.length + y) = v
          y+=1;
        }      
        x+=1;
      }
    }
  
  def f_paint(g: Graphics, dimension: Dimension){
    if(colors(0) != null){
      val g2d = g.asInstanceOf[Graphics2D]
      //loop over points and draw each as a box
      for (i <- 0 until rangeX.length; j <- 0 until rangeY.length) {       
        g2d.setColor(colors(i*rangeY.length+j))
       
        val x = scala.math.floor(dimension.width * rangeX(i)).toInt
        val y = scala.math.floor(dimension.height * rangeY(j)).toInt

        val w = scala.math.ceil(dimension.width * (1.0 / rangeX.length)).toInt
        val h = scala.math.ceil(dimension.height * (1.0 / rangeY.length)).toInt
        g2d.fillRect(x, y, w, h)
      }    
    }
    }
}
