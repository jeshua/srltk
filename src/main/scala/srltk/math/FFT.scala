package srltk.math
import scalala.tensor.dense._
import edu.emory.mathcs.jtransforms.fft._
object FFT {
  /**
   * FFT, input and output are real matrices, complex portion discarded
   */
  def fftReal(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val mat = ColtData.toColtComplex(data)   
    val ft = new DoubleFFT_2D(data.numRows, data.numCols)
    ft.complexForward(mat)
    val newData = ColtData.toReal(mat)
    newData
  }
  
  /**
   * FFT, input and output are complex matrices (odd columns are complex)
   */
  def fftComplex(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val mat = ColtData.toColt(data)
    fftComplex(mat)
  }
  def fftComplex(data: Array[Array[Double]]): DenseMatrix[Double] = {
	ColtData.toMat(fftComplexColt(data))
  }
  def fftComplexColt(data: DenseMatrix[Double]): Array[Array[Double]] = {
    fftComplexColt(ColtData.toColt(data))
  }
  def fftComplexColt(data: Array[Array[Double]]): Array[Array[Double]] = {
    val ft = new DoubleFFT_2D(data.length, data(0).length/2)
    ft.complexForward(data)
    data
  }
  
  /**
   * Inverse FFT, input and output are complex matrices (odd columns are complex)
   */
  def ifftComplex(data: DenseMatrix[Double]): DenseMatrix[Double] = {
    val mat = ColtData.toColt(data)
    ifftComplex(mat)
  }
  def ifftComplex(data: Array[Array[Double]]): DenseMatrix[Double] = {
	ColtData.toMat(ifftComplexColt(data))
  }
  def ifftComplexColt(data: DenseMatrix[Double]): Array[Array[Double]] = {
    ifftComplexColt(ColtData.toColt(data))
  }
  def ifftComplexColt(data: Array[Array[Double]]): Array[Array[Double]] = {
    val ft = new DoubleFFT_2D(data.length, data(0).length/2)
    ft.complexInverse(data,true)
    data
  }
  
  
  def main(args: Array[String]) {
    val data = DenseMatrix((.23, .384, .389), (.32, .349, .098), (.284, .892, .372), (.0348, .320, .102))
    println(data)
    println(fftReal(data))
  }
  
  
  def fftshift(x : DenseMatrix[Double]) : DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](x.numRows,x.numCols)
    val m1 = x.numRows/2
    val m2 = x.numCols/2
    //first quadrant
    for(i <- 0 until m1; j <- 0 until m2)					ret(i,j) = x(i+m1,j+m2)
    //second quadrant
    for(i <- 0 until m1; j <- m2 until x.numCols)    		ret(i,j) = x(i+m1,j-m2)
    //third quadrant
    for(i <- m1 until x.numRows; j <- 0 until m2)			ret(i,j) = x(i-m1,j+m2)
    //fourth quadrant
    for(i <- m1 until x.numRows; j <- m2 until x.numCols)	ret(i,j) = x(i-m1,j-m2)
 ret
  }

}