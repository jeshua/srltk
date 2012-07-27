package srltk.domains.rlstandard
import srltk.domains._
import srltk.utils.Bounds2D
import srltk.common._

trait Domain2D {
  def createState(d1 : Double, d2 : Double) : SimState
  val bounds : Bounds2D
  val integerState : Boolean = false
} 
