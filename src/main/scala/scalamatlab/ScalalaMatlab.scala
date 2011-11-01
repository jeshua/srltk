package scalamatlab
import scalala.tensor._
import scalala.tensor.dense._
import matlabcontrol._
import matlabcontrol.extensions._
import java.util.Arrays
import srltk.tools.utils.Timer._

class ScalalaMatlab(show : Boolean = false) {  
	val matlab = new Matlab(show)
	def toArray(m : Matrix[Double]) = 
		(for(i <- 0 until m.numRows) yield (for(j <- 0 until m.numCols) yield m(i,j)).toArray).toArray
	def toArray(v : DenseVector[Double]) = 
		(for(i <- 0 until v.length) yield (for(j <- 0 until 1) yield v(i)).toArray).toArray
		
	def toMat(m : Array[Array[Double]]) : DenseMatrix[Double] = {
	  val numRows = m.length
	  val numCols = m(0).length
	  val ret = DenseMatrix.zeros[Double](numRows,numCols)
	  for(i <- 0 until numRows;j <- 0 until numCols)
	    ret(i,j) = m(i)(j)
	  ret
	}
	
	def toVec(m : Array[Array[Double]]) : DenseVector[Double] = {
	  val len = m.length
	  val ret = DenseVector.zeros[Double](len)
	  for(i <- 0 until len)
	    ret(i) = m(i)(0)
	  ret
	}
	
	def mult(m1 : Matrix[Double], m2 : Matrix[Double]) : DenseMatrix[Double] = {		
		matlab.assign("m1",toArray(m1))
		matlab.assign("m2",toArray(m2))
		matlab.eval("res = m1 * m2;")
		toMat(matlab.getArray("res"))
	}
	
	def mult(m1 : Matrix[Double], v2 : DenseVector[Double]) : DenseVector[Double] = {		
		matlab.assign("m1",toArray(m1))
		matlab.assign("v2",toArray(v2.asCol))
		matlab.eval("res = m1 * v2;")
		toVec(matlab.getArray("res"))
	}
	
	def mult(v1 : DenseVector[Double], m2 : Matrix[Double]) : DenseVector[Double] = {
		matlab.assign("v1",toArray(v1.asCol))
		matlab.assign("m2",toArray(m2))		
		matlab.eval("res = (v1' * m2)';")
		toVec(matlab.getArray("res"))
	}
	def mult(v1 : DenseVectorCol[Double], v2 : DenseVectorRow[Double]) : DenseMatrix[Double] = {
		matlab.assign("v1",toArray(v1.asCol))
		matlab.assign("v2",toArray(v2.asCol))		
		matlab.eval("res = v1 * v2';")
		toMat(matlab.getArray("res"))
	}
	
	def assign(name : String, m : Matrix[Double]) = matlab.assign(name,toArray(m))
	def assign(name : String, v : DenseVector[Double]) = matlab.assign(name,toArray(v))
	def assign(name : String, v : Double) = matlab.eval(name+"="+v+";")
	def eval(expr : String) = matlab.eval(expr)
	def getMat(name : String) = toMat(matlab.getArray(name))
	def getVec(name : String) = toVec(matlab.getArray(name))
	def getDouble(name : String) : Double = matlab.getDouble(name)
		
  
}

object ScalalaMatlab {	
  
	def main(a : Array[String]) = {		
		val ml = new ScalalaMatlab(false)
		
		val rng = new util.Random()
		val m = DenseMatrix.zeros[Double](400,200)
		val v = DenseVector.zeros[Double](200)
		m.foreachPair((k :(Int,Int),v : Double) => m(k._1,k._2) = rng.nextDouble)
		v.foreachPair((k : Int, q : Double) => v(k) = rng.nextDouble)
	    //Send the array to MATLAB, transpose it, then retrieve it and convert it to a 2D double array
		def mult2 = {
		  ml.eval("x = 3;")
		  println(ml.getDouble("x"))
		  //println(ml.mult(v,v.t))
		}
		println(time(mult2 _))
		
	
	}
}
