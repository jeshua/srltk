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

import javax.media.j3d.BoundingSphere
import javax.media.j3d.BranchGroup
import javax.media.j3d.DirectionalLight
import javax.media.j3d.Transform3D
import javax.media.j3d.TransformGroup
import javax.vecmath.Color3f
import javax.vecmath.Point3d
import javax.vecmath.Vector3f
import com.sun.j3d.utils.behaviors.mouse.MouseRotate
import com.sun.j3d.utils.behaviors.mouse.MouseWheelZoom
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.universe.SimpleUniverse
import javax.swing.JFrame
import javax.media.j3d.Canvas3D
import java.awt.GraphicsConfiguration
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import srltk.tools.features.CMAC
import javax.media.j3d.Font3D
import javax.media.j3d.FontExtrusion
import java.awt.Font
import javax.media.j3d.Text3D
import javax.media.j3d.Shape3D
import javax.media.j3d.Appearance
import javax.media.j3d.ColoringAttributes
import javax.media.j3d.Material
import java.awt.Color
import javax.media.j3d.AmbientLight
import javax.vecmath.Color4f
import javax.media.j3d.TransparencyAttributes
import com.sun.j3d.utils.geometry.Box

class Visualizer3D(width: Int, height: Int, draw: (TransformGroup) => Unit) extends JFrame {
	
  val drawingPanel = new javax.swing.JPanel();

  //create JFframe Contents  
  setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
  drawingPanel.setLayout(new java.awt.BorderLayout());
  drawingPanel.setPreferredSize(new java.awt.Dimension(500, 500));
  getContentPane().add(drawingPanel, java.awt.BorderLayout.CENTER);
  pack();

  //add convas to panel
  val config = SimpleUniverse.getPreferredConfiguration();
  val canvas = new Canvas3D(config);
  drawingPanel.add(canvas, java.awt.BorderLayout.CENTER);

  val univ = new SimpleUniverse(canvas);
  univ.getViewingPlatform().setNominalViewingTransform();
  univ.getViewer().getView().setMinimumFrameCycleTime(5);

  var scene: BranchGroup = null

  redraw()

  //==================================================

  def redraw(): Unit = {
    if (scene != null) scene.detach()
    scene = createScene()
    scene.setCapability(BranchGroup.ALLOW_DETACH)
    scene.compile()
    univ.addBranchGraph(scene);
  }

  //==================================================
  //double click behavior
  val listener = new MouseAdapter() {
    override def mousePressed(e: MouseEvent): Unit = {
      if (e.getClickCount == 2) {
        e.consume()
        redraw
      }
    }
  }

  canvas.addMouseListener(listener)

  //==================================================

  def createScene(): BranchGroup = {
    val group = new BranchGroup();

    // Y axis made of cones
    val trans = new TransformGroup();
    trans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

    //draw to transform group
    this.draw(trans)

    //scale everything down by 50%
    val transform = new Transform3D()
    transform.setScale(0.5)
    trans.setTransform(transform)

    //add transform group
    group.addChild(trans);

    val lightColor = new Color3f(.5f, .5f, .5f); // white light
    val bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);
    val light = new AmbientLight(lightColor);
    light.setInfluencingBounds(bounds);
    group.addChild(light);

    //allow rotation and zoom with mouse
    val rotateBehavior = new MouseRotate();
    rotateBehavior.setTransformGroup(trans);
    trans.addChild(rotateBehavior);
    rotateBehavior.setSchedulingBounds(bounds);

    val zoomBehavior = new MouseWheelZoom();
    zoomBehavior.setTransformGroup(trans);
    trans.addChild(zoomBehavior);
    zoomBehavior.setSchedulingBounds(bounds);

    group
  }
}
