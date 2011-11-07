package srltk.vis
import javax.swing._
import java.awt._
import java.awt.Graphics2D

import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter

class Visualizer2D(width: Int, height: Int, f: (Graphics, Dimension) => Unit) extends JFrame {

  private val dimension = new Dimension(width, height)
  private val panel = new Visualizer2DPanel(width, height, f)

  def updateDisplay = panel.repaint()

  setVisible(true)
  setSize(width, height)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  add(panel)
}

class Visualizer2DPanel(width: Int, height: Int, f: (Graphics, Dimension) => Unit) extends JPanel {
  setSize(width, height)
  setBackground(new Color(0, 0, 0))
  setDoubleBuffered(true)

  val listener = new MouseAdapter() {
    override def mousePressed(e: MouseEvent): Unit = {
      if (e.getClickCount == 2) {
        e.consume()
        repaint()
      }
    }
  }
  this.addMouseListener(listener)

  override def paint(g: Graphics): Unit =
    {
      super.paint(g)

      f(g, this.size)
    }
}

