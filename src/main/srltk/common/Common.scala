package srltk
import scalala.tensor.VectorCol
import scalala.tensor.dense.DenseVectorCol

package object common {
	type Feats = VectorCol[Double]

	
	//allow scalala vector cols to be traversable
	class TraversableVectorCol[A](val v : VectorCol[Double]) extends Traversable[A] {
    	def foreach[U](f: A => U) = v.foreach _
    }
    def asTraversable(v : VectorCol[Double]) = new TraversableVectorCol[Double](v)
    def asVectorCol(t : Traversable[Double]) = t.asInstanceOf[TraversableVectorCol[Double]].v
}
