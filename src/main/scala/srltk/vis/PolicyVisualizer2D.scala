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
import srltk.utils.Colors


class PolicyVisualizer2D(
  val bounds: Bounds2D,
  val num_actions : Int,
  val pi: (Double, Double) => Seq[Double],
  integer: Boolean = false, resolution: Int = 100) {

  val xMin = bounds.xMin
  val xMax = bounds.xMax
  val yMin = bounds.yMin
  val yMax = bounds.yMax
  val step: Float = 1.0f / resolution
  val xSize = xMax - xMin + (if(integer) 1d else 0d)
  val ySize = yMax - yMin + (if(integer) 1d else 0d)
  val rangeX = (if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / xSize).toFloat).toArray
  val rangeY = (if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / ySize).toFloat).toArray
      
  var values = Array.fill(rangeX.length * rangeY.length * num_actions){0d}
  val vis = new Visualizer2D(500, 500, f_paint)
  def getFrame = vis
  def update() = {
	  f_remake();
	  vis.repaint() 
  }  
  def reRender() = update()
  vis.setVisible(true)

  def f_remake(): Unit =
    {    
      @inline def getX(percent: Float) = (xSize) * percent + xMin
      @inline def getY(percent: Float) = (ySize) * percent + yMin
      //calculate value for every point in the range and then find min and max value
      var x = 0;
      var y = 0;
      while(x < rangeX.length){
        y = 0
        while(y < rangeY.length){
          val mu = pi(getX(rangeX(x)), getY(rangeY(y)))
          for(a <- 0 until num_actions){
            values(x * rangeY.length*num_actions + y*num_actions + a) = mu(a)
          }          
          y+=1;
        }      
        x+=1;
      }
      if(values.max > 1 || values.min < 0) throw new IllegalArgumentException("Policy values out of range!")
    }
  
  def f_paint(g: Graphics, dimension: Dimension){    
    if(values != null){
      val g2d = g.asInstanceOf[Graphics2D]
      //loop over points and draw each as a box
      for (i <- 0 until rangeX.length; j <- 0 until rangeY.length) {
        //determine color by value of point
        
        var alphas = for(a <- 0 until num_actions)
          yield values(i * rangeY.length*num_actions + j*num_actions+a).toFloat
        val cols = for(i <- 0 until num_actions) 
          yield Colors.colorProgression(i)
          
          val col = Colors.mix(cols,alphas)
          
        g2d.setColor(col)
        
        /*var max = alphas(0)
        var maxi = 0
        for(i <- 0 until alphas.length){
          if(alphas(i) > max){
            max = alphas(i)
            maxi = i
          }
        }
        g2d.setColor(Colors.colorProgression(maxi));*/

        //convert point to a pixel location on screen
        //notice we floor the start location and ceil the end so the rectangles fit snugly
        val x = scala.math.floor(dimension.width * rangeX(i)).toInt
        val y = scala.math.floor(dimension.height * rangeY(j)).toInt

        val w = scala.math.ceil(dimension.width * (1.0 / rangeX.length)).toInt
        val h = scala.math.ceil(dimension.height * (1.0 / rangeY.length)).toInt
        g2d.fillRect(x, y, w, h)
      }
    }
    }
}

