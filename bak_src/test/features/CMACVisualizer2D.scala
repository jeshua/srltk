package srltk.test.features

import srltk.features.CMAC
import srltk.vis._
import javax.swing._
import java.awt._
import java.awt.Graphics2D
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import srltk.utils.Colors
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
      val c = new CMAC(ranges, bins, 1);

      for (
        i <- 0.0f to 1.0f by 0.03f;
        j <- 0.0f to 1.0f by 0.03f
      ) {
        val x = dimension.width * i
        val y = dimension.height * j
        val tiling = c.getTilingIndices(DenseVector(x, y));
        g2d.setColor(Colors.colorProgression(tiling(0)))
        g2d.drawString(tiling(0).toString, x.toInt, y.toInt)
      }
    }
}

