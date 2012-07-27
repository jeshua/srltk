package srltk.features
import srltk.common._
import scalala.tensor.VectorCol

trait FeatureExtractor extends Serializable{
  def apply(feats: VectorCol[Double]): VectorCol[Double] 
  def length: Int
  def isSparse = false
  def invert(feats : VectorCol[Double]) : VectorCol[Double] = {
    throw new IllegalArgumentException("Extractor is not invertible.")
  }
}

class IdentityExtractor(outDim : Int) extends FeatureExtractor {
  def apply(input: VectorCol[Double]): VectorCol[Double] = input
  def length: Int = outDim  
}
object IdentityExtractor { def apply(d: Int) = new IdentityExtractor(d) }
