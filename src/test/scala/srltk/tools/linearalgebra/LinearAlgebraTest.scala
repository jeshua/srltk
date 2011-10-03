package srltk.tools.linearalgebra

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import org.scalatest.FeatureSpec
import org.scalatest.Tag
import scala.collection.mutable._
import org.scalatest.matchers.ShouldMatchers._
import org.junit.Assert._
import scalala.tensor.dense.{ DenseVector, DenseMatrix }
import scalala.tensor.{ DiagonalMatrix, MatrixSingularException, Matrix, Vector }
import scalala.library.MatrixEmptyException
import scalala.library.NotConvergedException
import scalala.tensor.dense.DenseVectorCol

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends FeatureSpec {

	/*feature("test transpose") {
	  
		val a = DenseMatrix.zeros[Double](5,5)
		val b = DenseVectorCol.zeros[Double](5)
		val c = b * a.t
		
	}*/
	
	
  
  
  //======================================================================
  feature("QRP") {
    val A = DenseMatrix.zeros[Double](5,3)
    A(0,0) = 0.4388;    A(0,1) = 0.8775;    A(0,2) = 0.2760
    A(1,0) = 0.3815;    A(1,1) = 0.7631;    A(1,2) = 0.6797
    A(2,0) = 0.7655;    A(2,1) = 1.5310;    A(2,2) = 0.6551
    A(3,0) = 0.7952;    A(3,1) = 1.5904;    A(3,2) = 0.1626
    A(4,0) = 0.1869;    A(4,1) = 0.3738;    A(4,2) = 0.1190
   
    val ret = LinearAlgebra.qrp(A)
    val Q = ret._1
    val R = ret._2
    val P = ret._3
    val QR = Q*R
    val AP = A*P
    
    scenario("AP=QR"){
      for(r <- 0 until 5; c <- 0 until 3)
    	assertEquals(AP(r,c),QR(r,c),0.000001)
    }
  }
  
  //======================================================================
  feature("Rank ") {
    val A = DenseMatrix(
    		(1d,1d,0d,0d,0d,1d),
    		(0d,0d,1d,0d,0d,1d),
    		(0d,1d,1d,0d,0d,0d),
    		(0d,0d,0d,1d,0d,0d),  		
    		(0d,0d,0d,0d,0d,0d),
    		(0d,0d,0d,0d,0d,0d)
    		)
    		
    val rank = scalala.library.LinearAlgebra.rank(A)
    val rng = util.Random
    for(r <- 0 until A.numRows; c <- 0 until A.numCols)
      A(r,c) += rng.nextDouble()*.1
    val thresh = 0.2
    val qr = LinearAlgebra.qrp(A)
    val svd = scalala.library.LinearAlgebra.svd(A)
    val qrrank = LinearAlgebra.estRankQR(qr._2,thresh)
    val svdrank = LinearAlgebra.estRankSVD(svd._2,thresh)
    scenario("QR rank right"){
      assert(rank===qrrank)
    }
    scenario("SVD rank right"){
      assert(rank===svdrank)
    }
  }

  //======================================================================

  def kahanMatrix(size : Int, rng : util.Random) : DenseMatrix[Double] = {
    val piv = rng.nextDouble()
    val phi = math.sqrt(piv)
    val zeta = math.sqrt(1-piv)
    val S = DenseMatrix.zeros[Double](size,size)
    S(0,0) = 1
    for(i <- 1 until size)
      S(i,i) = math.pow(zeta,i)
    val K = DenseMatrix.eye[Double](size,size)  
    for(i <- 0 until size-1;
        j <- (i+1) until size)
      K(i,j) = -phi
    
    S*K
  }

  feature("Strong RRQR") {
    val A = DenseMatrix(
    		(1d,1d,0d,0d,0d,1d),
    		(0d,0d,1d,0d,0d,1d),
    		(0d,1d,1d,0d,0d,0d),
    		(0d,0d,0d,1d,0d,0d),  		
    		(0d,0d,0d,0d,0d,0d),
    		(0d,0d,1d,0d,0d,0d),
    		(0d,0d,1d,0d,0d,0d)
    		)
    		
    val realk = scalala.library.LinearAlgebra.rank(A)
    val rng = new util.Random()
    for(r <- 0 until A.numRows; c <- 0 until A.numCols)
      A(r,c) += rng.nextDouble()*0.1
    

/*    val s = new StrongRRQR(A,.0001)
    scenario("Rank of simple matrix is right"){
      assert(s.k === realk)
    }*/

/*    scenario("Rank of identity matrices"){ 
      for(n <- 2 until 20){
        var n = 5
        assert((new StrongRRQR(DenseMatrix.eye[Double](n),0.00001)).k === n)
      }
    }*/

    
    scenario("Rank of simple diagonal matrices is right"){ 
      val n = 220
      val m = 190
      val d = DenseMatrix.zeros[Double](n,n)
      for(i <- 0 until m;j <- 0 until n)
        d(i,j) = rng.nextDouble()*.00001
      //ranking should be 3,18,4,5,9,6
      d(3,3) = .4
      d(18,18) = .2
      d(4,4) = .1
      d(5,5) = .09
      d(9,9) = .08
      d(6,6) = .07
      try
      {
//        val s = new StrongRRQR(d,-1,maxRank=6,alg=4)
//        println(s.pvt.asRow)
//        println(LinearAlgebra.qrp(d)._4.asRow)
        val m = LinearAlgebra.findBasis(d,tol=0.01)
//        println(m)
      } catch{case e => e.printStackTrace()}
    }


/*  scenario("Rank of simple matrix is right"){ 
    val m = 6
    val n = 5
    val d = DenseMatrix.zeros[Double](m,n)
//    for(i <- 0 until n;j <- 0 until n)
//      d(i,j) = rng.nextDouble()*.00001
      //ranking should be 3,18,4,5,9,6
      d(3,3) = 1
      d(4,4) = 1
      println(d)
    try
    {
      val s = new StrongRRQR(d,.0001)
      println(s.k)
      println(s.pvt.asRow)
      println(LinearAlgebra.qrp(d)._4.asRow)
    } catch{case e => e.printStackTrace()}
     
    }
  */

    
    
/*     val K = kahanMatrix(50,rng)
    println(K)
    println("QR tells me: ")
    println(LinearAlgebra.qrp(K)._4.asRow)
    println("StrongRRQR says: ")
    val d = new StrongRRQR(K,-1,5,4)
    for(i <- d.pvt)
      print(i + " ")
    println("K is "+d.k)*/




  }

}
