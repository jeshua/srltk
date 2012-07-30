package srltk.utils
class Bounds1D(val min : Double, val max : Double)
object Bounds1D{
  def apply(min : Double, max : Double) = new Bounds1D(min,max)
}