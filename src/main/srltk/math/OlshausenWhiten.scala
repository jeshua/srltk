package srltk.math
import scalala.tensor.dense._
import MatOps._
import MatConversions._


object OlshausenWhiten {
  def main(args: Array[String]) = {
    val data = DenseMatrix((.23, .384, .389), (.32, .349, .098), (.284, .892, .372), (.0348, .320, .102))

    		print(whiten(data))
      }
  
  
  def apply(X : IndexedSeq[DenseVector[Double]]) : IndexedSeq[DenseVector[Double]] = {
    matToSeq(whiten(seqToMat(X)))
  }
  
  def apply(x : DenseVector[Double]) : DenseVector[Double] = {
    val mat : DenseMatrix[Double] = vecToMat(x)
    matToVec(whiten(mat))
  }
  
  //X: dims x samples
  def whiten(X: DenseMatrix[Double]): DenseMatrix[Double] = {
     var data = X :- repmat(mean(X,1),X.numRows,1);
	  data :/= repmat(std(data,1),X.numRows,1);
	  data :-= mean(data);
	  data :/= std(data);	
	  val N1 = size(data,1)
	  val N2 = size(data,2)
	  val range1 = seq(-N1/2d,N1/2d)
	  val range2 = seq(-N2/2d,N2/2d)
	  val m = meshgrid(range1,range2)
	  val fx = m._1
	  val fy = m._2
	  val rho = sqrt((fx :* fx) + (fy :* fy)).t.toDense
	  val f0 = 0.4 * (N1 + N2)/2d
	  val filt = rho :* exp(-pow(rho :/ f0,4))
	  val ft = FFT.fftComplex(ColtData.toColtComplex(data));
	  val filtShift = ColtData.toComplex(FFT.fftshift(filt))  
	  val whitened = FFT.ifftComplexColt(ColtData.complexComponentMult(ft, filtShift))
	  val realW = ColtData.toReal(whitened)
	  realW / std(realW)
  }
  
  

}