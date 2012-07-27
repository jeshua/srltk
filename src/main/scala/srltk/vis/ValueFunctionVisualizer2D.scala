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

trait ValueFunctionVisualizer {  
  def getFrame : JFrame
  def update() : Unit
  def reRender() : Unit
  def setValueFunction(v : (Double,Double) => Double)
 
}


class ValueFunctionVisualizer2D(
  val bounds: Bounds2D,
  var V: (Double, Double) => Double,
  integer: Boolean = false, resolution: Int = 100) extends ValueFunctionVisualizer {

  val xMin = bounds.xMin
  val xMax = bounds.xMax
  val yMin = bounds.yMin
  val yMax = bounds.yMax

  
  
  val vis = new Visualizer2D(500, 500, f)
  
  def getFrame = vis
  def update() = vis.repaint()
  def reRender() = vis.repaint()
  def setValueFunction(v : (Double,Double)=>Double) = {this.V = v}
  vis.setVisible(true)

  def f(g: Graphics, dimension: Dimension): Unit =
    {
      val g2d = g.asInstanceOf[Graphics2D]
      val xSize = xMax - xMin
      val ySize = yMax - yMin
      def getX(percent: Float) = (xSize) * percent + xMin
      def getY(percent: Float) = (ySize) * percent + yMin
      //create locations to evaluate
      val step: Float = 1.0f / resolution
      val rangeX = if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / xSize).toFloat
      val rangeY = if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / ySize).toFloat

      //println(rangeX)
      //println(rangeX.map(getX _))

      //calculate value for every point in the range and then find min and max value
      val values = for (i <- rangeX; j <- rangeY) yield V(getX(i), getY(j))

      val vMax = values.foldLeft(values(0))(_ max _)
      val vMin = values.foldLeft(values(0))(_ min _)

      //loop over points and draw each as a box
      for (i <- 0 until rangeX.length; j <- 0 until rangeY.length) {

        //determine color by value of point
        val value = values(i * rangeY.length + j)
        val normVal = (value - vMin) / (vMax - vMin)
        val r = (50 * (normVal + .1) / (1.1)).toInt
        val g = (255 * (normVal + .2) / (1.2)).toInt
        val b = (190 * (normVal + .4) / (1.4)).toInt
        g2d.setColor(new Color(r, g, b))

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

