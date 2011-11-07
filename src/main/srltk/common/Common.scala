package srltk
import scalala.tensor.VectorCol
import scalala.tensor.dense.DenseVectorCol

package object common {
	type Feats = VectorCol[Double]
    implicit def toFeats(d: Double*): Feats = new DenseVectorCol(d.toArray);
    implicit def toFeats(v: Vector[Double]): Feats = v.asCol
	implicit def intToAction(n : Int) : IntAction = IntAction(n)
	implicit def actionToInt(a : IntAction) : Int = a.n	
	
	//allow scalala vector cols to be traversable
	class TraversableVectorCol[A](val v : VectorCol[Double]) extends Traversable[A] {
    	def foreach[U](f: A => U) = v.foreach _
    }
    def asTraversable(v : VectorCol[Double]) = new TraversableVectorCol[Double](v)
    def asVectorCol(t : Traversable[Double]) = t.asInstanceOf[TraversableVectorCol[Double]].v
}
