package srltk.projects.psrs

import srltk.tools.utils._
import srltk.tools.utils.Timer._
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.sparse.SparseVector
import scalala.tensor.mutable._
import scala.util.Random
import scalala.library.LinearAlgebra._
object TimeMatrix {
	def main(args : Array[String]){

		{
			val size = 100
			val mat1 : DenseMatrix[Double] = DenseMatrix.zeros[Double](size,size)
			val mat2 : DenseMatrix[Double] = DenseMatrix.zeros[Double](size,size)
			val rand = new Random()
			for(i <- 0 until size; j <- 0 until size){
				mat1(i,j) = rand.nextDouble()
				mat2(i,j) = rand.nextDouble()
			}
			println("svd time: "+(time(() => {svd(mat1)})).toString)
			println("multiplication time: "+(time(() => {mat1 * mat2})).toString)
		}
		{
			val vec1 : VectorCol[Double] = SparseVector.zeros[Double](100000) 
			vec1(128) = 1
			val mat1 : Matrix[Double] = DenseMatrix.zeros[Double](100000,100)
			println("sparse multiplication time: "+time(() => {vec1.t * mat1}).toString)
		}
	}  
}