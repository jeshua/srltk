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
    
    
	  var col1 = cols(0).getComponents(null)
	  col1(3) = alpha(0)    
    for(i <- 1 until cols.length){
       var col2 = cols(i).getComponents(null)
	  col2(3) = alpha(i)   
      col1 = mix(col1,col2)
    }    
	for(i <- 0 to 2) col1(i) = math.min(1,math.max(0,col1(i)));	
	new Color(col1(0),col1(1),col1(2))
	  
  }
  
  def mix(col1 : Array[Float], col2 : Array[Float]) : Array[Float] = {
   
    val a1 = col1(3);
    val a2 = col2(3);
    if(a1 > .99) col1
    else if(a2 > .99) col2
    else{
    val a = a1 + a2*(1f-a1)
    val ret = new Array[Float](4)
    for(i <- 0 to 2)
    	ret(i) = (1/a)*(col1(i)*a1 + col2(i)*a2 * (1f-a1))
    ret(3) = a
    ret
    }
  }
}
