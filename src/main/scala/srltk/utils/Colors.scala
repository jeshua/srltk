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

}
