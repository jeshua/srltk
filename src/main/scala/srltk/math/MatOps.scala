package srltk.math
import scalala.tensor.dense._
import scalala.operators.Implicits
import scalala.tensor._
import scalala.library._
import scala.collection.immutable.NumericRange

object MatOps {

  def main(args: Array[String]) = {
    val data = DenseMatrix((.23, .384, .389), (.32, .349, .098), (.284, .892, .372), (.0348, .320, .102))
    println(data)
    println()
    println(repmat(mean(data, 1), 3, 1))
    println()
    println(std(data, 1))
    println(std(data, 2))
  }

  def size(X: DenseMatrix[Double], dim: Int): Int = if (dim == 1) X.numRows else X.numCols
  def size(X: DenseMatrix[Double]): (Int, Int) = (X.numRows, X.numCols)

  /**
   * Duplicates a matrix alone one or both directions
   */
  def repmat(x: DenseMatrix[Double], n: Int, m: Int): DenseMatrix[Double] = {
    val newMat = DenseMatrix.zeros[Double](x.numRows * n, x.numCols * m)
    for (i <- 0 until n; j <- 0 until m; k <- 0 until x.numRows; l <- 0 until x.numCols)
      newMat(i * x.numRows + k, j * x.numCols + l) = x(k, l)
    newMat
  }
  def repmat(x: DenseVectorCol[Double], n: Int, m: Int): DenseMatrix[Double] = {
    val newMat = DenseMatrix.zeros[Double](x.length * n, 1 * m)
    for (i <- 0 until n; j <- 0 until m; k <- 0 until x.length)
      newMat(i * x.length + k, j) = x(k)
    newMat
  }
  def repmat(x: DenseVectorRow[Double], n: Int, m: Int): DenseMatrix[Double] = {
    val newMat = DenseMatrix.zeros[Double](n, x.length * m)
    for (i <- 0 until n; j <- 0 until m; l <- 0 until x.length)
      newMat(i, j * x.length + l) = x(l)
    newMat
  }

  /**
   * Mean
   * dim = 1 returns mean of rows as single row
   * dim = 2 returns mean of cols as single col
   */
  def mean(x: DenseMatrix[Double], dim: Int): DenseMatrix[Double] = {
    if (dim == 1) {
      val ret = DenseMatrix.zeros[Double](1, x.numCols)
      val m = colMeans(x)
      for (i <- 0 until m.length) ret(0, i) = m(i)
      ret
    } else {
      val ret = DenseMatrix.zeros[Double](x.numRows, 1)
      val m = rowMeans(x)
      for (i <- 0 until m.length) ret(i, 0) = m(i)
      ret
    }
  }
  /**
   * Mean of all entries
   */
  def mean(x: DenseMatrix[Double]): Double = {
    sum(x) / (x.numRows * x.numCols)
  }

  /**
   * STD
   * dim = 1 returns mean of rows as single row
   * dim = 2 returns mean of cols as single col
   * (normalizeds by N-1)
   */
  def std(x: DenseMatrix[Double], dim: Int): DenseMatrix[Double] = {
    if (dim == 1) {
      val c = x - repmat(mean(x, 1), size(x, 1), 1)
      sqrt(sum(pow(c, 2), 1) / (size(x, 1) - 1))
    } else {
      val c = x - repmat(mean(x, 2), 1, size(x, 2))
      sqrt(sum(pow(c, 2), 2) / (size(x, 2) - 1))
    }
  }
  /**
   * STD of all entries
   */
  def std(x: DenseMatrix[Double]): Double = {
    val mu = mean(x)
    var sum = 0d
    for (i <- 0 until x.numRows; j <- 0 until x.numCols) sum += (x(i, j) - mu) * (x(i, j) - mu)
    scala.math.sqrt(sum / (x.numRows * x.numCols - 1))
  }

  /**
   * Sum
   * dim = 1 returns sum of rows as single row
   * dim = 2 returns sum of cols as single col
   */
  def sum(x: DenseMatrix[Double], dim: Int): DenseMatrix[Double] = {
    if (dim == 1) {
      val ret = DenseMatrix.zeros[Double](1, x.numCols)
      val m = colSums(x)
      for (i <- 0 until m.length) ret(0, i) = m(i)
      ret
    } else {
      val ret = DenseMatrix.zeros[Double](x.numRows, 1)
      val m = rowSums(x)
      for (i <- 0 until m.length) ret(i, 0) = m(i)
      ret
    }
  }
  /**
   * Sum of all entries
   */
  def sum(x: DenseMatrix[Double]): Double = {
    var sum = 0d
    for (i <- 0 until x.numRows; j <- 0 until x.numCols) sum += x(i, j)
    sum
  }

  /**
   * Create a diagonal matrix with value n
   */
  def diag(n: Double, length: Int): DenseMatrix[Double] = {
    val ret = DenseMatrix.eye[Double](length, length)
    ret :* n
  }

  /**
   * Convert vector into diagonal matrix
   */
  def diag(v: DenseVector[Double]): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](v.length, v.length)
    for (i <- 0 until v.length) ret(i, i) = v(i)
    ret
  }
  //sums over the rows: result numCols x 1
  def colSums(X: DenseMatrix[Double]): DenseVectorCol[Double] = {
    var sum = DenseVectorCol.zeros[Double](X.numCols)
    for (i <- 0 until X.numRows)
      sum :+= X(i, ::)
    sum
  }
  //returns colSums/#rows
  def colMeans(X: DenseMatrix[Double]): DenseVectorCol[Double] = {
    colSums(X) :/ X.numRows
  }

  //sums over the cols: result numRows x 1
  def rowSums(X: DenseMatrix[Double]): DenseVectorCol[Double] = {
    var sum = DenseVectorCol.zeros[Double](X.numRows)
    for (i <- 0 until X.numCols)
      sum = sum :+ X(::, i).asCol
    sum
  }
  //returns rowSums/#cols
  def rowMeans(X: DenseMatrix[Double]): DenseVectorCol[Double] = {
    rowSums(X) :/ X.numCols
  }

  def sqrt(x: DenseMatrix[Double]): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](x.numRows, x.numCols)
    for (i <- 0 until x.numRows; j <- 0 until x.numCols) ret(i, j) = scala.math.sqrt(x(i, j))
    ret
  }
  def pow(x: DenseMatrix[Double], n: Int): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](x.numRows, x.numCols)
    for (i <- 0 until x.numRows; j <- 0 until x.numCols) ret(i, j) = scala.math.pow(x(i, j), n)
    ret
  }
  def exp(x: DenseMatrix[Double]): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](x.numRows, x.numCols)
    for (i <- 0 until x.numRows; j <- 0 until x.numCols) ret(i, j) = scala.math.exp(x(i, j))
    ret
  }

  /**
   * Seq
   */
  def seq(start: Double, end: Double, by: Int = 1): DenseVector[Double] = {
    val len = math.floor((end - start) / by).toInt
    val ret = DenseVector.zeros[Double](len)
    for (i <- 0 until len) ret(i) = i * by + start
    ret
  }

  /**
   * Meshgrid
   */
  def meshgrid(a: DenseVector[Double], b: DenseVector[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val fx = DenseMatrix.zeros[Double](b.length, a.length)
    for (i <- 0 until a.length; j <- 0 until b.length) fx(j, i) = a(i)
    val fy = DenseMatrix.zeros[Double](b.length, a.length)
    for (i <- 0 until a.length; j <- 0 until b.length) fy(j, i) = b(j)
    (fx, fy)
  }

}