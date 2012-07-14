package srltk.features
import srltk.common._

trait FeatureExtractor extends Serializable{
  def apply(feats: Feats): Feats 
  def length: Int
  def isSparse = false
  def invert(feats : Feats) : Feats = {
    throw new IllegalArgumentException("Extractor is not invertible.")
  }
}

class IdentityExtractor(outDim : Int) extends FeatureExtractor {
  def apply(input: Feats): Feats = input
  def length: Int = outDim  
}
object IdentityExtractor { def apply(d: Int) = new IdentityExtractor(d) }
