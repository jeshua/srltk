package srltk.vis

import javax.swing._
import java.awt._
import java.awt.Graphics2D
import srltk.common._
import srltk.domains._

class StateViewer[T <: SimState[T]](width: Int, height: Int, renderer: SimStateRenderer[T]) extends JFrame {
  def updateDisplay = panel.updateDisplay _

  private val dimension = new Dimension(width, height)
  private val panel = new StateViewerPanel(dimension, renderer)

  setVisible(true)
  setSize(width, height)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  add(panel)
}

class StateViewerPanel[T <: SimState[T]](dimension: Dimension, renderer: SimStateRenderer[T]) extends JPanel {

  private var currState : Option[T] = None
  setDoubleBuffered(true)
  setSize(dimension)
  show()
  override def paint(g: Graphics): Unit =
    {
      super.paint(g)
      val g2d = g.asInstanceOf[Graphics2D];
      if (currState != None)
        renderer.render(currState.get, g2d, dimension);
      g.dispose()
    }
  def updateDisplay(s: T) = {
    this.currState = Some(s)
    repaint()
  }
}
