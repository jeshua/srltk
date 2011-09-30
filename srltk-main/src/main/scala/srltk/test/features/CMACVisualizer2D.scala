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
package srltk.test.features

import srltk.tools.features.CMAC
import srltk.vis._
import javax.swing._
import java.awt._
import java.awt.Graphics2D
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import srltk.tools.utils.ColorUtils
object CMACVisualizer2D {

  def main(args: Array[String]): Unit = {
    val vis = new Visualizer2D(700, 700, f)
    vis.setVisible(true)
  }

  def f(g: Graphics, dimension: Dimension): Unit =
    {
      val g2d = g.asInstanceOf[Graphics2D];
      val gridWidth = 4
      val gridHeight = 4
      val cellWidth = (dimension.width - 1) / gridWidth
      val cellHeight = (dimension.height - 1) / gridHeight
      val boxSize = scala.math.min(cellWidth, cellHeight);

      //create a cmac
      val rng = new scala.util.Random()
      val ranges = immutable.List((0.0, dimension.width.toDouble), (0.0, dimension.height.toDouble))
      val bins = immutable.List(5, 5);
      val c = new CMAC(ranges, bins, 1, rng);

      for (
        i <- 0.0f to 1.0f by 0.03f;
        j <- 0.0f to 1.0f by 0.03f
      ) {
        val x = dimension.width * i
        val y = dimension.height * j
        val tiling = c.getTilingIndices(DenseVector(x, y));
        g2d.setColor(ColorUtils.colorProgression(tiling(0)))
        g2d.drawString(tiling(0).toString, x.toInt, y.toInt)
      }
    }
}

