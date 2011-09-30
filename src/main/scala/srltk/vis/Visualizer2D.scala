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

