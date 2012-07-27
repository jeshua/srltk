package srltk.vis

import javax.swing._
import java.awt._
import java.awt.Graphics2D
import srltk.common._
import srltk.domains._

class StateViewer(width: Int, height: Int, renderer: SimStateRenderer) extends JFrame {
  def updateDisplay = panel.updateDisplay _

  private val dimension = new Dimension(width, height)
  private val panel = new StateViewerPanel(dimension, renderer)

  setVisible(true)
  setSize(width, height)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  add(panel)
}

class StateViewerPanel(dimension: Dimension, renderer: SimStateRenderer) extends JPanel {

  private var currState: SimState = null
  setDoubleBuffered(true)
  setSize(dimension)
  show()
  override def paint(g: Graphics): Unit =
    {
      super.paint(g)
      val g2d = g.asInstanceOf[Graphics2D];
      if (currState != null)
        renderer.render(currState, g2d, dimension);
      g.dispose()
    }
  def updateDisplay(s: SimState) = {
    this.currState = s
    repaint()
  }
}
