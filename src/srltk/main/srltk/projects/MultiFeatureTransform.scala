package srltk.projects
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import srltk.api.agent.FeatureTransform

abstract class MultiFeatureTransform()  {
  def apply(feats : Array[Feats]) : Feats
}

class MultiFeatureTransformWrapper(f : (Array[Feats]) => Feats) extends MultiFeatureTransform  {
  def apply(feats : Array[Feats]) : Feats = f(feats)
}

class StackTransform extends MultiFeatureTransform  {
  def apply(f : Array[Feats]) : Feats = {
		  if(f.length > 0)
			  StackTransform.stack(f,f(0).isInstanceOf[SparseVector[_]])
		  else
			  SparseVector.zeros[Double](0)
  }
}

object StackTransform{
  def stack(f : Array[Feats], sparse : Boolean) : Feats = {
	  val totalSize = f.foldLeft(0)(_ + _.length)
	  //create a vector to hold combined input vectors
	  val stackedVector: scalala.tensor.mutable.VectorCol[Double] =
		  if(!sparse) DenseVector.zeros[Double](totalSize);
		  else SparseVector.zeros[Double](totalSize);
	  var offset = 0
	  for (i <- 0 until f.length) {
		  f(i).foreachNonZeroPair(
				  (k: Int, v: Double) => stackedVector(k + offset) = v);
		  offset += f(i).length
	  }
	  stackedVector
  }
}

/**
 * Applies same transform to every feature input then stacks the results
 */
class MultiTransform(extractor : FeatureTransform) extends MultiFeatureTransform  {
  def apply(f : Array[Feats]) : Feats = {
		  val features = for(feature <- f) yield extractor <= feature
		  StackTransform.stack(features,extractor.isSparse)
  }
}