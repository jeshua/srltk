package srltk.common
import scalala.tensor.dense.DenseVectorCol

object Implicits {
    implicit def toFeats(d: Double*): Feats = new DenseVectorCol(d.toArray);
    implicit def toFeats(v: Vector[Double]): Feats = v.asCol
	implicit def intToAction(n : Int) : IntAction = IntAction(n)
	implicit def actionToInt(a : IntAction) : Int = a.n	
}