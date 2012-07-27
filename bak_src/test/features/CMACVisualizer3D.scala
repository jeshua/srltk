package srltk.test.features
import javax.media.j3d.BoundingSphere
import javax.media.j3d.BranchGroup
import javax.media.j3d.DirectionalLight
import javax.media.j3d.Transform3D
import javax.media.j3d.TransformGroup
import javax.vecmath.Color3f
import javax.vecmath.Point3d
import javax.vecmath.Vector3f
import com.sun.j3d.utils.behaviors.mouse._
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
import srltk.features.CMAC
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
import srltk.vis._
object CMACVisualizer3D {

  def main(args: Array[String]): Unit = {
    val v = new Visualizer3D(700, 700, draw)
    v.setVisible(true)
  }
  def draw(trans: TransformGroup): Unit = {
    val rng = new scala.util.Random()
    val ranges = immutable.List((-1.0, 1.0), (-1.0, 1.0), (-1.0, 1.0))
    val bins = immutable.List(3, 3, 2);
    val c = new CMAC(ranges, bins, 1);

    //colors
    val colors = immutable.List(new Color3f(1, 0, 0), new Color3f(0, 1, 0), new Color3f(0, 0, 1),
      new Color3f(1, 1, 0), new Color3f(0, 1, 1), new Color3f(1, 0, 1),
      new Color3f(.5f, 0, 0), new Color3f(0, .5f, 0), new Color3f(0, 0, .5f),
      new Color3f(.5f, .5f, 0), new Color3f(0, .5f, .5f), new Color3f(.5f, 0, .5f),
      new Color3f(.25f, 0, 0), new Color3f(0, .25f, 0), new Color3f(0, 0, .25f),
      new Color3f(.25f, .25f, 0), new Color3f(0, .25f, .25f), new Color3f(.25f, 0, .25f))

    val white = new Color3f(1, 1, 1)

    for (
      x <- -1.0f to 1.0f by 0.2f;
      y <- -1.0f to 1.0f by 0.2f;
      z <- -1.0f to 1.0f by 0.2f
    ) {

      val tiling = c.getTilingIndices(DenseVector(x, y, z));
      val text = tiling(0).toString()

      //create translation to place object at 3d location
      val tg = new TransformGroup()
      val transform = new Transform3D()
      val vector = new Vector3f(x, y, z)
      transform.setTranslation(vector)
      transform.setScale(0.1)
      tg.setTransform(transform)

      //create color
      val col = colors(tiling(0) % colors.length)
      val mat = new Material()
      mat.setAmbientColor(white)
      mat.setDiffuseColor(white)
      mat.setEmissiveColor(col)

      val displayText = false
      if (displayText) {
        //create text object
        val textAppear = new Appearance();
        val textColor = new ColoringAttributes();
        textColor.setColor(col)
        textAppear.setColoringAttributes(textColor);
        textAppear.setMaterial(mat);
        //create font
        val font3D = new Font3D(new Font("Helvetica", Font.PLAIN, 1), new FontExtrusion());
        val textGeom = new Text3D(font3D, text);
        textGeom.setAlignment(Text3D.ALIGN_CENTER);
        val textShape = new Shape3D();
        textShape.setGeometry(textGeom);
        textShape.setAppearance(textAppear);
        //add text object to transform group        
        tg.addChild(textShape);
      } else {
        val obj = new Sphere(.6f);

        val appear = new Appearance();
        //val alpha = new TransparencyAttributes();
        //alpha.setTransparencyMode(TransparencyAttributes.SCREEN_DOOR);
        //alpha.setTransparency(.1f)
        //appear.setTransparencyAttributes(alpha);
        appear.setMaterial(mat);
        //mat.setShininess(10f)
        //val obj = new Box(.2f,.2f,.2f,appear);              
        obj.setAppearance(appear);
        tg.addChild(obj)
      }
      //add transform group to parent
      trans.addChild(tg);
    }
  }
}
