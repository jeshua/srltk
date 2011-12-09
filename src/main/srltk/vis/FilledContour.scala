package srltk.vis
import org.jzy3d.chart.Chart
import org.jzy3d.colors.Color
import org.jzy3d.colors.ColorMapper
import org.jzy3d.colors.colormaps.ColorMapRainbow
import org.jzy3d.colors.colormaps.ColorMapRedAndGreen
import org.jzy3d.contour.DefaultContourColoringPolicy
import org.jzy3d.contour.MapperContourPictureGenerator
import org.jzy3d.factories.JzyFactories
import org.jzy3d.maths.Range
import org.jzy3d.plot3d.builder.Builder
import org.jzy3d.plot3d.builder.Mapper
import org.jzy3d.plot3d.builder.concrete.OrthonormalGrid
import org.jzy3d.plot3d.primitives.Shape
import org.jzy3d.plot3d.primitives.axes.AxeFactory
import org.jzy3d.plot3d.primitives.axes.ContourAxeBox
import org.jzy3d.plot3d.primitives.axes.IAxe
import org.jzy3d.plot3d.rendering.canvas.Quality
import org.jzy3d.plot3d.rendering.legends.colorbars.ColorbarLegend
import srltk.utils.Bounds2D
import javax.swing.JFrame
import java.awt.Dimension
import org.jzy3d.chart.controllers.thread.ChartThreadController
import org.jzy3d.chart.controllers.mouse.ChartMouseController
import org.jzy3d.maths.Coord3d
import org.jzy3d.plot3d.primitives.Scatter


class FilledContour(
  val bounds: Bounds2D,
  var func: (Double, Double) => Double,
  faceDisplayed : Boolean = true) {
  def this(b : (Double,Double,Double,Double), func : (Double,Double)=>Double) = this(new Bounds2D(b),func)
  def this(b : (Double,Double,Double,Double), func : (Double,Double)=>Double, fd : Boolean) = this(new Bounds2D(b),func,fd)
  
  val frame = new JFrame()
  def getFrame = frame
  def setFunction(f: (Double, Double) => Double) = { this.func = f }
  val mapper = new Mapper() {
    def f(x: Double, y: Double) = func(x, y)
  }
  var surface: Shape = null
  val rangeX = new Range(bounds.xMin, bounds.xMax)
  val rangeY = new Range(bounds.yMin, bounds.yMax)
  
  val chart = createChart()
  def createChart(): Chart = {
  
    val steps = 40
    surface = Builder.buildOrthonormal(new OrthonormalGrid(
      rangeX, steps, rangeY, steps), mapper).asInstanceOf[Shape]
    val colorMapper = new ColorMapper(new ColorMapRedAndGreen(), surface
		  .getBounds().getZmin(), surface.getBounds().getZmax(),
		  new Color(1, 1, 1, .7f))
  
    surface.setColorMapper(colorMapper)
    surface.setWireframeDisplayed(false)
    surface.setFaceDisplayed(faceDisplayed)
                
    
    //==================================================
     // Create a chart with contour axe box, and attach the contour picture
    JzyFactories.axe = new AxeFactory(){
    	override def getInstance() : IAxe = {new ContourAxeBox(box)}                        
    }
    val chart = new Chart()
    //chart.getAxeLayout().setXTickLabelDisplayed(false)    
    //chart.getAxeLayout().setYTickLabelDisplayed(false)    
    chart.getAxeLayout().setZTickLabelDisplayed(false)    
    val cab = chart.getView().getAxe().asInstanceOf[ContourAxeBox]
    val contour = new MapperContourPictureGenerator(mapper, rangeX, rangeY)
    
    cab.setContourImg(contour.getFilledContourImage(new DefaultContourColoringPolicy(colorMapper), 400, 400, 20), rangeX, rangeY)
    //cab.setContourImg(contour.getContourImage(new DefaultContourColoringPolicy(colorMapper), 400, 400, 10), rangeX, rangeY)
    chart.addDrawable(surface);
    chart.setViewPoint(new Coord3d(math.Pi/2,math.Pi/2,0d))

    		
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
}
