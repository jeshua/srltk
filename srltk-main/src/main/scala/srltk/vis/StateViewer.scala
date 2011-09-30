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

import javax.swing._
import java.awt._
import java.awt.Graphics2D
import srltk.api.domain._

class StateViewer(width: Int, height: Int, renderer: StateRenderer) extends JFrame {
  def updateDisplay = panel.updateDisplay _

  private val dimension = new Dimension(width, height)
  private val panel = new StateViewerPanel(dimension, renderer)

  setVisible(true)
  setSize(width, height)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
  add(panel)
}

class StateViewerPanel(dimension: Dimension, renderer: StateRenderer) extends JPanel {

  private var currState: State = null
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
  def updateDisplay(s: State) = {
    this.currState = s
    repaint()
  }
}
