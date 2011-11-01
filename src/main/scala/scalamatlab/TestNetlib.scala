package scalamatlab
import org.netlib.blas._
import scalala.tensor.dense.DenseMatrix

import srltk.tools.utils.Timer._


object TestNetlib {

	def main(args : Array[String]){
		val ml = new ScalalaMatlab(false)
		println(BLAS.getInstance().getClass().getName)
		val sizes = List(10,100,1000,2000)
		for(s <- sizes){
			printf("%dx%d matrix: \n",s,s)
			val rng = new util.Random()
			//val a = DenseMatrix.zeros[Double](s,s)	  
			val a = DenseMatrix.eye[Double](s,s)	  
			a.foreachPair((k :(Int,Int),v : Double) => a(k._1,k._2) = rng.nextDouble)
			var res1 : DenseMatrix[Double] = null
			var res2 : DenseMatrix[Double] = null
			def multMatlab = {
				res1 = ml.mult(a,a)
			}
			
		
			println("\tMatlab: "+timeWallclock(multMatlab _))
			def multBlas(a : DenseMatrix[Double], b : DenseMatrix[Double]) : DenseMatrix[Double] = {
				val rv = DenseMatrix.zeros[Double](a.numRows, b.numCols)
				
				BLAS.getInstance().dgemm("N", "N",
						rv.numRows, rv.numCols, a.numCols,
						1.0, a.data, a.numRows, b.data, a.numCols,
						0.0, rv.data, a.numRows)
						rv
			}
				def mult2  = {
				res2 = multBlas(a,a)
			}
			println("\tBLAS: "+timeWallclock(mult2 _))
			
			
		}
	}

}