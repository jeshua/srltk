package srltk.tools.linearalgebra
import scalala.tensor.dense._
import scalala.tensor._
import scalala.library._

package object MiscStats {

  
  	// ==================================================
	//Gaussians
	
	/**
	 * Computes n-dimensional gaussian pdf at x
	 * mu     :  n x 1 mean
	 * invCov : n x n inverse of covariance matrix
	 * detCov : determinant of covariance matrix
	 */
	def gaussianPDF(
			x : DenseVectorCol[Double],
			mu : DenseVectorCol[Double], 
			invCov : DenseMatrix[Double],
			detCov : Double) = {
	  val n = mu.length
	  val v = (x-mu)
	  val term1= v.asRow * invCov * v
	  val term2= math.pow(2*math.Pi,-n/2d) * 1d/(math.sqrt(detCov))
	  term2 * math.exp(-0.5 * term1)	  
	}
  
  
	
	// ==================================================
	// DIAG
	def diag(n : Double, length : Int) : DenseMatrix[Double]  = {
	  val ret = DenseMatrix.eye[Double](length,length)
	  ret :* n
	}
  
	def diag(v : DenseVector[Double]) : DenseMatrix[Double] = {
	  val ret = DenseMatrix.zeros[Double](v.length,v.length)
	  for(i <- 0 until v.length) ret(i,i) = v(i)
	  ret
	}
	
	
  
  
	//sums over the rows: result numCols x 1
	def colSums(X : DenseMatrix[Double]) : DenseVectorCol[Double] = {
		
		var sum = DenseVectorCol.zeros[Double](X.numCols)
		for(i <- 0 until X.numRows)
			sum = sum + X(i,::)
		sum
	}
	//returns colSums/#rows
	def colMeans(X : DenseMatrix[Double]) : DenseVectorCol[Double] = {
	  colSums(X) :/ X.numRows
	}
	
	//sums over the cols: result numRows x q
	def rowSums(X : DenseMatrix[Double]) : DenseVectorCol[Double] = {
	
		var sum = DenseVectorCol.zeros[Double](X.numRows)
		for(i <- 0 until X.numCols)			
			sum = sum :+ X(::, i).asCol
		
		sum
	}
	//returns rowSums/#cols
	def rowMeans(X : DenseMatrix[Double]) : DenseVectorCol[Double] = {
	  rowSums(X) :/ X.numCols
	}
	
	
}