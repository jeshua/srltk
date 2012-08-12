package srltk.utils

class Bounds2D(val xMin: Double, val xMax: Double, val yMin: Double, val yMax: Double) {
  def this(b : (Double,Double,Double,Double)) = this(b._1,b._2,b._3,b._4)
  
  def as1D()= List(Bounds1D(xMin,xMax),Bounds1D(yMin,yMax))
}
