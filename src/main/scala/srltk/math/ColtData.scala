package srltk.math
import scalala.tensor.dense.DenseMatrix

object ColtData {
  //converts a complex colt matrix to a real scalala matrix
  def toReal(data: Array[Array[Double]]): DenseMatrix[Double] = {
    val nr = data.length
    val nc = data(0).length
    val mat = DenseMatrix.zeros[Double](nr, nc / 2)
    for (i <- 0 until nr; j <- (0 until nc).by(2)) {
      mat(i, j / 2) = data(i)(j)
    }
    mat
  }
  
  //converts a colt matrix to a scalala matrix
  def toMat(data: Array[Array[Double]]): DenseMatrix[Double] = {
    val nr = data.length
    val nc = data(0).length
    val mat = DenseMatrix.zeros[Double](nr, nc)
    for (i <- 0 until nr; j <- 0 until nc) {
      mat(i, j) = data(i)(j)
    }
    mat
  }
  //converts a real scalala matrix to a complex scalala matrix
  def toComplex(data: DenseMatrix[Double], complexVal : Double = 0): DenseMatrix[Double] = {
    val ret = DenseMatrix.zeros[Double](data.numRows,data.numCols*2)
    for (i <- 0 until data.numRows) {           
      for (j <- 0 until data.numCols) ret(i,j*2) = data(i, j)
      if(complexVal != 0)
    	  for (j <- 0 until data.numCols) ret(i,j*2+1) = complexVal
    }
    ret
  }  
  
  //converts real matrix to a complex colt matrix
  def toColtComplex(data : DenseMatrix[Double]) : Array[Array[Double]] = {
   val ret = new Array[Array[Double]](data.numRows)
    for (i <- 0 until data.numRows) {
      ret(i) = new Array[Double](2 * data.numCols)
      for (j <- 0 until data.numCols) ret(i)(j*2) = data(i, j)
    }
   ret
  }
  
  
  //converts a matrix to colt matrix
  def toColt(data : DenseMatrix[Double]) : Array[Array[Double]] = {
   val ret = new Array[Array[Double]](data.numRows)
    for (i <- 0 until data.numRows) {
      ret(i) = new Array[Double](data.numCols)
      for (j <- 0 until data.numCols) ret(i)(j) = data(i, j)
    }
   ret
  }  
  
  
  //multiply complex matrices componentwise
  def complexComponentMult(mat1 : DenseMatrix[Double], mat2 : DenseMatrix[Double]) = {
    val ret = DenseMatrix.zeros[Double](mat1.numRows,mat1.numCols)
    for(i <- 0 until mat1.numRows){
    	for(j <- 0 until mat1.numCols/2){
    		
    		ret(i,j*2) = mat1(i,j*2) * mat2(i,j*2)
    		ret(i,j*2+1) = mat1(i,j*2) * mat2(i,j*2+1) + mat1(i,j*2+1) * mat2(i,j*2) - mat1(i,j*2+1) * mat2(i,j*2+1)
    	}
    }
    ret
  }
  
  
}