package srltk.algs.linear
import srltk.common._
import scalala.tensor.VectorCol

object common{
	type Vec = VectorCol[Double]
}
class VecObs(val f : common.Vec, r : Double) extends Observation(r) {}


