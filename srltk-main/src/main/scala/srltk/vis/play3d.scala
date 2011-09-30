package srltk.vis

import javax.media.j3d._
import javax.vecmath._
import com.sun.j3d.utils.geometry._
import com.sun.j3d.utils.image.TextureLoader
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.picking.behaviors._
import com.sun.j3d.utils.picking._
import com.sun.j3d.utils.applet.MainFrame
import scala.collection._
import scalala.tensor.dense.DenseVector
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import srltk.tools.features.CMAC
import srltk.tools.utils.LibFunctions
import javax.swing.JFrame
import javax.swing.JPanel
import java.awt.GraphicsEnvironment
import com.sun.j3d.utils.behaviors.mouse.MouseRotate
import com.sun.j3d.utils.behaviors.mouse.MouseWheelZoom



class Play3D(width: Int, height: Int) extends JFrame {
	
  val mainPanel = new javax.swing.JPanel()
  val drawingPanel1 = new javax.swing.JPanel()
  val drawingPanel2 =  new javax.swing.JPanel()
  

  //create JFframe Contents  
  setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
  drawingPanel1.setLayout(new java.awt.BorderLayout());
  drawingPanel1.setPreferredSize(new java.awt.Dimension(500, 500));
  drawingPanel2.setLayout(new java.awt.BorderLayout());
  drawingPanel2.setPreferredSize(new java.awt.Dimension(500, 500));
    
  mainPanel.add(drawingPanel1)
  mainPanel.add(drawingPanel2)
  
  getContentPane().add(mainPanel, java.awt.BorderLayout.CENTER)
  pack()

  val camera1 = new CameraView()
  val camera2 = new CameraView()
  val universe = new VirtualUniverse()
  val loc = new Locale(universe)
  initialize()
  
  
  val scene = createScene()
  drawingPanel1.add(camera1.canvas, java.awt.BorderLayout.CENTER)
  drawingPanel2.add(camera2.canvas, java.awt.BorderLayout.CENTER)
  
  
  //==================================================
  def initialize() { 
	  val xform1 = new Transform3D()
	  val rot1 = new Transform3D()
	  xform1.set(new Vector3f( 0.0f, 0.0f, 5.0f ))	  
	  //rot1.rotY(scala.math.Pi / 2)
	  //xform1.mul(rot1)
	  camera1.vpTG.setTransform(xform1)
	

	
	  val xform2 = new Transform3D()
	  val vec2 = new Vector3f( 0.0f, 0.5f, 0.5f )
	  xform2.set(vec2)
	  camera2.vpTG.setTransform(xform2)
	  //camera2.view.setProjectionPolicy(View.PARALLEL_PROJECTION);
	 
	  loc.addBranchGraph(camera1.group)
  }
 
  //==================================================

  def createScene(): BranchGroup = {
    val scene = new BranchGroup();
    val trans = new TransformGroup();
    trans.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    trans.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    trans.setCapability(javax.media.j3d.Node.ENABLE_PICK_REPORTING)
    this.draw(trans)
    
    //add transform group
    trans.addChild(camera2.group)
    
    
    //directional light
    val lightColor = new Color3f(.5f, .5f, .5f); // white light
    val bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);
    val lightDirection = new Vector3f (0.0f, -3.0f, 0f);
    val light = new DirectionalLight(lightColor,lightDirection)//new AmbientLight(lightColor);
    light.setInfluencingBounds(bounds);
    trans.addChild(light);
    //ambient light
    val lightColor2 = new Color3f(.4f, .4f, .4f); // white light
    val light2 = new AmbientLight(lightColor2)//new AmbientLight(lightColor);
    light.setInfluencingBounds(bounds);
    scene.addChild(light2);
    
    scene.addChild(trans);
    
    //allow rotation and zoom with mouse
    /*val rotateBehavior = new MouseRotate();
    rotateBehavior.
    rotateBehavior.setTransformGroup(trans);
    trans.addChild(rotateBehavior);
    rotateBehavior.setSchedulingBounds(bounds);*/
    val zoomBehavior = new MouseWheelZoom();
    zoomBehavior.setTransformGroup(trans);
    trans.addChild(zoomBehavior);
    zoomBehavior.setSchedulingBounds(bounds);
    
    scene.addChild(new PickRotateBehavior( scene, camera1.canvas, bounds));
    scene.addChild(new PickTranslateBehavior( scene, camera1.canvas, bounds));
    
    
    
    
    
    
    scene.compile()
    
    loc.addBranchGraph(scene)  
    
    scene
  }
  
   //==================================================
 
  def draw(trans: TransformGroup): Unit = {
		  val rng = new scala.util.Random()

		  //colors
		  val colors = immutable.List(new Color3f(1, 0, 0), new Color3f(0, 1, 0), new Color3f(0, 0, 1),
				  new Color3f(1, 1, 0), new Color3f(0, 1, 1), new Color3f(1, 0, 1),
				  new Color3f(.5f, 0, 0), new Color3f(0, .5f, 0), new Color3f(0, 0, .5f),
				  new Color3f(.5f, .5f, 0), new Color3f(0, .5f, .5f), new Color3f(.5f, 0, .5f),
				  new Color3f(.25f, 0, 0), new Color3f(0, .25f, 0), new Color3f(0, 0, .25f),
				  new Color3f(.25f, .25f, 0), new Color3f(0, .25f, .25f), new Color3f(.25f, 0, .25f))

				  val white = new Color3f(1, 1, 1)

		  val x = 0f
		  val z = 0f


		  //create translation to place object at 3d location
		  
		  
		  //create color
		  val mat = new Material()
		  mat.setAmbientColor(white)
		  mat.setDiffuseColor(white)
		  mat.setEmissiveColor(new Color3f(1f,0f,0f))

		  val appear = new Appearance()
		  appear.setMaterial(mat);            
		  
		  val box = new Box(.8f,.01f,.8f,appear)
		  val tg = new TransformGroup()
		  val transform = new Transform3D()
		  transform.setTranslation(new Vector3f(x, 0, z))
		  tg.setTransform(transform)		  
		  tg.addChild(box)

		  trans.addChild(tg)
  }  
}

//======================================================================


object CameraView {
  val body = new PhysicalBody
  val environment = new PhysicalEnvironment
}
class CameraView {
	val gconfigTempl = new GraphicsConfigTemplate3D();
	val gconfig = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getBestConfiguration(gconfigTempl)
	val canvas = new Canvas3D( gconfig )
	val view = new View()
	val viewPlatform = new ViewPlatform()

	view.setPhysicalBody(CameraView.body)
	view.setPhysicalEnvironment(CameraView.environment)
	view.attachViewPlatform(viewPlatform)
	view.addCanvas3D(canvas)

	val vpTG = new TransformGroup()
	vpTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
	vpTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
	vpTG.addChild(viewPlatform)

	val group = new BranchGroup()
	group.setCapability(BranchGroup.ALLOW_DETACH)
	group.addChild(vpTG)
	
	
}

//======================================================================


object Play3D {
  def main(args : Array[String]){
    LibFunctions.addLibs()
    val v = new Play3D(1000,500)
    v.setVisible(true)
  }
}
