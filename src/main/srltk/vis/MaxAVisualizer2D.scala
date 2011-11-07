/*******************************************************************************
 * Scala Reinforcement Learning Toolkit
 *   @author Jeshua Bratman
 *    @email jeshuabratman@gmail.com 
 * 
 * Copyright 2011 jeshua
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
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

class MaxAVisualizer2D(
  val bounds: Bounds2D,
  var maxA: (Double, Double) => Int,
  integer: Boolean = false, resolution: Int = 100) {

  val vis = new Visualizer2D(500, 500, f)
  vis.setVisible(true)

  val colors = immutable.List(new Color(255, 0, 0), new Color(0, 255, 0), new Color(0, 0, 255),
    new Color(255, 255, 0), new Color(0, 255, 255), new Color(255, 0, 255),
    new Color(150, 0, 0), new Color(0, 150, 0), new Color(0, 0, 150),
    new Color(150, 150, 0), new Color(0, 150, 150), new Color(150, 0, 150),
    new Color(75, 0, 0), new Color(0, 75, 0), new Color(0, 0, 75),
    new Color(75, 75, 0), new Color(0, 75, 75), new Color(75, 0, 75))

  //Draw function
  def f(g: Graphics, dimension: Dimension): Unit =
    {
      val g2d = g.asInstanceOf[Graphics2D]
      val xSize = bounds.xMax - bounds.xMin
      val ySize = bounds.yMax - bounds.yMin

      def getX(percent: Float) = (xSize) * percent + bounds.xMin
      def getY(percent: Float) = (ySize) * percent + bounds.yMin
      //create locations to evaluate
      val step: Float = 1.0f / resolution
      val rangeX = if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / xSize).toFloat
      val rangeY = if (!integer) 0.0f to 1.0f by step else 0.0f until 1.0f by (1.0 / ySize).toFloat

      //loop over points and draw each as a box
      for (i <- rangeX; j <- rangeY) {
        val a = maxA(getX(i), getY(j))
        g2d.setColor(colors(a % colors.length))

        //convert point to a pixel location on screen
        //notice we floor the start location and ceil the end so the rectangles fit snugly
        val x = scala.math.floor(dimension.width * i).toInt
        val y = scala.math.floor(dimension.height * j).toInt
        val w = scala.math.ceil(dimension.width * (1.0 / rangeX.length)).toInt
        val h = scala.math.ceil(dimension.height * (1.0 / rangeY.length)).toInt
        g2d.fillRect(x, y, w, h)
      }
    }
}

