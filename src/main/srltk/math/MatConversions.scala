package srltk.math
import scala.collection.mutable.ArraySeq
import scalala.tensor.dense._
import scala.collection.mutable.ArrayBuffer
import scalala.tensor.dense.DenseMatrix
import scala.collection.mutable.HashMap
import scala.util.Sorting._

object MatConversions {

  //convert sequence to observation data matrix of size d x n
  def seqToMat(seq: IndexedSeq[DenseVector[Double]]): DenseMatrix[Double] = {
    val dim = seq(0).length
    val num = seq.length
    val ret = DenseMatrix.zeros[Double](dim, num)
    for (i <- 0 until seq.length) {
      val f: DenseVectorCol[Double] = seq(i).toDense.asCol
      ret(0 until ret.numRows, i) := f
    }
    ret
  }

  //convert matrix of size d x n to n-length sequence of d-dimension features
  def matToSeq(m: DenseMatrix[Double]): IndexedSeq[DenseVector[Double]] = {
    val dim = m.numRows
    val num = m.numCols
    (for (i <- 0 until num) yield m(0 until dim, i).toDense.asCol)
  }

  //convert vector to matrix
  def vecToMat(v: DenseVector[Double]): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](v.length, 1)
    for(i <- 0 until v.length) ret(i,0) = v(i)
    ret
  }

  //convert vector to matrix
  def matToVec(m: DenseMatrix[Double]): DenseVector[Double] = {
    val ret = DenseVector.zeros[Double](m.numRows * m.numCols)
    for (i <- 0 until m.numRows; j <- 0 until m.numCols) ret(i * m.numCols + j) = m(i,j)
    ret
  }
}
