package srltk.algs.linearalgebra
import org.netlib.lapack._
import org.netlib.util.intW
import scalala.library.LinearAlgebra._
import scalala.operators._
import scalala.scalar.Scalar
import scalala.generic.collection.CanViewAsVector
import scalala.tensor.domain.TableDomain
import scalala.tensor.dense.{ DenseVector, DenseMatrix }
import scalala.tensor.{DiagonalMatrix, MatrixSingularException, Matrix, Vector }
import scalala.library._
import scalala.library.Library._


object LinearAlgebra {

  private def requireNonEmptyMatrix[V](mat: Matrix[V]) = {
    if (mat.numCols == 0 || mat.numRows == 0)
      throw new MatrixEmptyException
  }

  def toDense(d : scalala.tensor.mutable.Vector[Double])  : DenseVector[Double]  = {
    val ret = DenseVector.zeros[Double](d.length)
    ret := d
  
    ret
  }

  def toDense(d : scalala.tensor.mutable.Matrix[Double])  : DenseMatrix[Double]  = {
    val ret = DenseMatrix.zeros[Double](d.numRows,d.numCols)
    ret := d
    ret
  }

  def lNorm(d : Vector[Double], l : Int = 2) : Double = {
    var sum = 0d
    for(i <- 0 until d.length) sum += math.pow(d(i),l)
    math.sqrt(sum)
  }


  def transpose(d : DenseMatrix[Double]) : DenseMatrix[Double] = {
    toDense(d.t)
  }


 

  //estimate rank
  def estRank(A : DenseMatrix[Double], thresh : Double = 1e-10) : Int = {
    estRankSVD(svd(A)._2,thresh)
  }

  /**
   * Estimate rank using SVD
   * 
   * input: A matrix of interest
   *        D from SVD (singular values)
   *        thresh threshold on singular values values
   * output: estimated rank
   */
  def estRankSVD(D : DenseVector[Double], thresh : Double = 1e-10) : Int = {
    val m = D.length
    val newD = normalize(D,2)
    //    println(newD)
    def r(rank : Int) : Int = {
      if(math.abs(newD(rank)) > thresh && rank < m-1) r(rank+1)
      else rank
    }
    math.max(r(0),1)
  }

  /**
   * Estimate rank using QF Factorization
   * 
   * input: A matrix of interest
   *        R from qr factorization
   *        thresh threshold on R values
   * output: estimated rank
   */
  def estRankQR(R : DenseMatrix[Double], thresh : Double = 1e-10) : Int = {
    val m = math.min(R.numRows,R.numCols)
    val newR = R
    def r(rank : Int) : Int = {
      if(math.abs(newR(rank, rank)) > thresh && rank < m-1) r(rank+1)
      else rank
    }
    math.max(r(0),1)
  }
 


  /**
  * Input: R: m x n matrix returned by QR decomposition
  * 
  * Return R= Ak Bk
  *           0  Ck
  *
  * with Ak : kxk
  *      Bk : k x (n-k)
  *      Ck : (m-k)(n-k)
  * */
  def qrDecomposeR(R : DenseMatrix[Double], k : Int)
  : (DenseMatrix[Double],DenseMatrix[Double],DenseMatrix[Double]) =
    {
      val m = R.numRows
      val n = R.numCols
      //k x k
      val Ak = DenseMatrix.zeros[Double](k,k)
      Ak := R(0 until k, 0 until k)
      //k x n-k
      val Bk = DenseMatrix.zeros[Double](k,n-k)
      Bk := R(0 until k, k until n)
      //m-k x n-k
      val Ck = DenseMatrix.zeros[Double](m-k,n-k)
      Ck := R(k until m, k until n)

      (Ak,Bk,Ck)
    }


}
