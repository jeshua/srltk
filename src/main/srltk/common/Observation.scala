package srltk.common
import scala.collection._
import scala.collection.generic._

class Observation(
		val vec : Traversable[Double],
		val reward : Double = 0d,
		val intObs : Int = -1) {}
