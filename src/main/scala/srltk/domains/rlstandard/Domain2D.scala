package srltk.domains.rlstandard
import srltk.domains._
import srltk.utils.Bounds2D
import srltk.common._

trait Domain2D[T <: SimState[T]] {
  def createState(d1 : Double, d2 : Double) : T
  val bounds : Bounds2D
  val integerState : Boolean = false
} 
