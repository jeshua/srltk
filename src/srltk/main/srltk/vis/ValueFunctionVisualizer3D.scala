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

import srltk.tools.features.CMAC
import srltk.tools.utils.Bounds2D
import javax.swing._
import java.awt._
import java.awt.Graphics2D
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import scala.collection.JavaConversions
import java.awt.Dimension
import javax.swing.JFrame
import org.jzy3d.chart.Chart
import org.jzy3d.colors.{ Color => Jz3Color }
import org.jzy3d.colors.ColorMapper
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.maths.Range
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.builder.Mapper
import org.jzy3d.plot3d.builder.concrete.OrthonormalGrid
import org.jzy3d.plot3d.primitives.Shape
import org.jzy3d.chart.Chart
import org.jzy3d.colors.Color
import org.jzy3d.colors.ColorMapper
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.maths.Coord3d
import org.jzy3d.maths.Range
import org.jzy3d.maths.TicToc
import org.jzy3d.maths.Utils
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.builder.Mapper
import org.jzy3d.plot3d.builder.concrete.OrthonormalGrid
import org.jzy3d.plot3d.primitives.AbstractDrawable
import org.jzy3d.plot3d.primitives.Point
import org.jzy3d.plot3d.primitives.Polygon
import org.jzy3d.plot3d.primitives.Shape
import org.jzy3d.plot3d.rendering.legends.colorbars.ColorbarLegend
import org.jzy3d.plot3d.rendering.view.Renderer2d
import org.jzy3d.chart.controllers.thread.ChartThreadController
import org.jzy3d.chart.controllers.mouse.ChartMouseController
import org.jzy3d.chart.controllers.mouse.interactives.SphereMouseSelector
import org.jzy3d.contour.MapperContourPictureGenerator
import org.jzy3d.plot3d.rendering.canvas.Quality
import org.jzy3d.plot3d.primitives.axes.ContourAxeBox
import org.jzy3d.contour.DefaultContourColoringPolicy
import org.jzy3d.factories.JzyFactories
import org.jzy3d.plot3d.primitives.axes.AxeFactory
import org.jzy3d.plot3d.primitives.axes.IAxe

class ValueFunctionVisualizer3D(val bounds: Bounds2D,
  var V2D: (Double, Double) => Double)
  extends ValueFunctionVisualizer {
  def getFrame = frame

  def update() {
    remap()
  }
  def reRender() {
    remake()
  }
  def setValueFunction(v: (Double, Double) => Double) = { this.V2D = v }

  val mapper = new Mapper() {
    def f(x: Double, y: Double) = V2D(x, y)
  }
  var surface: Shape = null
  val frame = new JFrame()
  
  
  val rangeX = new Range(bounds.xMin, bounds.xMax)
  
    val rangeY = new Range(bounds.yMin, bounds.yMax)
    val chart = createChart()

    
    def createSurface() {
    	val steps = 60
    	surface = Builder.buildOrthonormal(new OrthonormalGrid(
    			rangeX, steps, rangeY, steps), mapper).asInstanceOf[Shape]
    			                                                    val colorMapper = new ColorMapper(new ColorMapRainbow(), surface
    			                                                    		.getBounds().getZmin(), surface.getBounds().getZmax(),
    			                                                    		new Jz3Color(1, 1, 1, .7f))
    	surface.setColorMapper(colorMapper)		  
    	surface.setWireframeDisplayed(false)
    }

    def createChart(): Chart = {
    		
    		createSurface()
    		

    		/*JzyFactories.axe = new AxeFactory(){
    			override def getInstance() :  IAxe = new ContourAxeBox(box);
    		}*/
    		val chart = new Chart(Quality.Advanced);
    		
    		/*val contour = new MapperContourPictureGenerator(mapper, rangeX, rangeY);
    		val cab = chart.getView().getAxe().asInstanceOf[ContourAxeBox]
    		cab.setContourImg( contour.getFilledContourImage(new DefaultContourColoringPolicy(colorMapper), 400, 400, 10), rangeX, rangeY);*/
    		
    		//==================================================
    		
    		surface.setLegend(new ColorbarLegend(surface, 
    				chart.getView().getAxe().getLayout().getZTickProvider(), 
    				chart.getView().getAxe().getLayout().getZTickRenderer()));
    		surface.setLegendDisplayed(true);
    		
    		
    		//==================================================
    		
    		chart.getScene().getGraph().add(surface)
    		val threadCamera = new ChartThreadController(chart);
    		val mouseCamera = new ChartMouseController();
    		mouseCamera.addSlaveThreadController(threadCamera);
    		chart.addController(mouseCamera);
    		
    		//==================================================
    		frame.setSize(new Dimension(500, 500))
    		frame.add(chart.getCanvas().asInstanceOf[java.awt.Component])
    		frame.setVisible(true)
    		
    		chart
    }

  def remap() {
    val polygons = surface.getDrawables().toArray()
    for (d <- polygons) {
      if (d.isInstanceOf[Polygon]) {
        val p = d.asInstanceOf[Polygon]
        for (i <- 0 until p.size()) {
          val pt = p.get(i)
          val c = pt.xyz
          c.z = mapper.f(c.x, c.y).toFloat
        }
      }
    }
    chart.render()
  }
  
  def remake(){
    
    val temp = surface
    createSurface()
    chart.getScene().getGraph().add(surface)        
    
    chart.getScene().getGraph().remove(temp)
    chart.render()
  }
}
