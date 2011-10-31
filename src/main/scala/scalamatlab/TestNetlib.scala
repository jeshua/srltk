package scalamatlab
import org.netlib.blas._
import scalala.tensor.dense.DenseMatrix

import srltk.tools.utils.Timer._


object TestNetlib {

  def main(args : Array[String]){
	  
	  val rng = new util.Random()
	  val a = DenseMatrix.zeros[Double](400,400)	  
	  a.foreachPair((k :(Int,Int),v : Double) => a(k._1,k._2) = rng.nextDouble)

	  def multBlas(a : DenseMatrix[Double], b : DenseMatrix[Double]) : DenseMatrix[Double] = {
		  val rv = DenseMatrix.zeros[Double](a.numRows, b.numCols)
		  println(BLAS.getInstance().getClass().getName)
		  BLAS.getInstance().dgemm("N", "N",
				  rv.numRows, rv.numCols, a.numCols,
				  1.0, a.data, a.numRows, b.data, a.numCols,
				  0.0, rv.data, a.numRows)
		  rv
	  }
      
      def mult2 = {
		  multBlas(a,a)
      }
      println(time(mult2 _))
  }
  
}