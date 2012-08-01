package srltk.vis

import javax.swing._
import java.awt._
import java.awt.Graphics2D
import srltk.common._
import srltk.domains._
import srltk.driver.SimpleDriver

class StateViewer[T <: SimState[T]](width: Int, height: Int, renderer: SimStateRenderer[T]) extends JFrame {
  
  def updateDisplay(a : Int, s: T) = panel.updateDisplay(a,s)
  def updateDisplay(d : SimpleDriver[T,_]) = panel.updateDisplay(d)
  private val dimension = new Dimension(width, height)
  private val panel = new StateViewerPanel(dimension, renderer)

  setVisible(true)
  setSize(width, height)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  add(panel)
}

class StateViewerPanel[T <: SimState[T]](dimension: Dimension, renderer: SimStateRenderer[T]) extends JPanel {

  private var currState : Option[T] = None
  private var action = 0
  setDoubleBuffered(true)
  setSize(dimension)
  show()
  override def paint(g: Graphics): Unit =
    {
      super.paint(g)
      val g2d = g.asInstanceOf[Graphics2D];
      if (currState != None)
        renderer.render(action, currState.get, g2d, dimension);
      g.dispose()
    }
  def updateDisplay(a : Int, s: T) = {
    this.currState = Some(s)
    this.action = a;
    repaint()
  }
  def updateDisplay(d : SimpleDriver[T,_]) = {
    this.currState = Some(d.d.state)
    this.action = if(d.prev_action == null) 0 else d.prev_action.n;
    repaint()
  }
}
