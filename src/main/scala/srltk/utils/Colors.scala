package srltk.utils
import java.awt.Color

object Colors {

  private val colors = List(new Color(255, 0, 0), new Color(0, 255, 0), new Color(0, 0, 255),
    new Color(255, 255, 0), new Color(0, 255, 255), new Color(255, 0, 255),
    new Color(150, 0, 0), new Color(0, 150, 0), new Color(0, 0, 150),
    new Color(150, 150, 0), new Color(0, 150, 150), new Color(150, 0, 150),
    new Color(75, 0, 0), new Color(0, 75, 0), new Color(0, 0, 75),
    new Color(75, 75, 0), new Color(0, 75, 75), new Color(75, 0, 75));

  def colorProgression(i: Int) = colors(i % colors.length)

  
  def mix(cols : Seq[Color], alpha : Seq[Float]) : Color = {
    var col = cols(0)
    var a = alpha(0)
    for(i <- 1 until cols.length){
      val n = mix(col,cols(i),a,alpha(i))      
      col = n._1
      a = n._2
    }    
    col
  }
  
  def mix(col1 : Color, col2 : Color, a1 : Float, a2 : Float) : (Color,Float) = {
    val rgb1 = col1.getRGBColorComponents(null)
    val rgb2 = col2.getRGBColorComponents(null)
    val a = a1 + a2*(1f-a1)
    val r = (1/a)*(rgb1(0)*a1 + rgb2(0)*a2 * (1f-a1))
    val g = (1/a)*(rgb1(1)*a1 + rgb2(1)*a2 * (1f-a1))
    val b = (1/a)*(rgb1(2)*a1 + rgb2(2)*a2 * (1f-a1))    
    (new Color(r,g,b),a);
  }
}
