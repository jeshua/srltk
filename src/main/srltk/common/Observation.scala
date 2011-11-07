/**
 * Scala Reinforcement Learning Toolkit (SRLTK)
 * @author Jeshua Bratman (jeshuabratman@gmail.com)
 * **/

package srltk.common
import scala.collection._
import scala.collection.generic._

class Observation(
		val vec : Traversable[Double],
		val reward : Double,
		val intObs : Int = -1) {}
