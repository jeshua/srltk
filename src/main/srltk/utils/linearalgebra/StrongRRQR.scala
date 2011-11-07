package srltk.algs.linearalgebra

import LinearAlgebra._
import org.netlib.lapack._
import org.netlib.util.intW
import scalala.library.LinearAlgebra._
import scalala.operators._
import scalala.scalar.Scalar
import scalala.generic.collection.CanViewAsVector
import scalala.tensor.domain.TableDomain
import scalala.tensor.dense._
import scalala.tensor.{DiagonalMatrix, MatrixSingularException, Matrix, Vector }
import scalala.library._
import scalala.library.Library._

/**
 * Find set of linearly independent columns
 * 
 * input: A: m x n matrix,
 *        f: accuracy bound
 *        tol: tolerance
 *        maxRank: maximum k, or if tolerance is < 0
 */

class StrongRRQR(A : DenseMatrix[Double], tol : Double, maxRank : Int = 500, alg : Int = 5){
  val m = A.numRows
  val n = A.numCols
  val f : Double = 1.0000001d//math.sqrt(n)
//  require(m >= n)

  //initialize
  val initialK = if(tol < 0) maxRank-1 else 0
  var k = initialK
//  val q = qrp(A)
  var R : DenseMatrix[Double] = A


  //pvt is length n vector
  var pvt : DenseVector[Int] = DenseVector.zeros[Int](n)//q._4
  for(i <- 0 until n) pvt(i) = i
  var Ak : DenseMatrix[Double] = null
  var Akinv : DenseMatrix[Double] = null
  var Bk : DenseMatrix[Double] = null
  var Ck : DenseMatrix[Double] = null
  var AkBk : DenseMatrix[Double] = null  
  var omega : DenseVector[Double] = null
  var gamma : DenseVector[Double] = null
  var jMax : Int = 0

  //update R, Ak,Bk,Ck,AkBk,omega,gamma,jMax given R and k
  def update(triangularize : Boolean = true) : Boolean =  {
    if(triangularize)
      R = qrp(R)._2
    //Ak: k x k
    //Bk: k x (n-k)
    //Ck: (m-k) x (n-k)
    val d = qrDecomposeR(R,k)
    Ak = d._1
    Bk = d._2
    Ck = d._3

    try{
      Akinv = pinv(Ak)
      // k x (n-k)
      AkBk = Akinv * Bk
/*      println("R:")
      println(R)
      println("K = "+k)
      println("AK:")
      println(Ak)
      println("BK:")
      println(Bk)
      println("CK:")
      println(Ck)
      println("AKBK")
      println(AkBk)
      println()*/
      //update norms

      this.omega = calcOmega(Akinv)
      this.gamma = calcGamma(Ck)
    } catch {
      case e : scalala.tensor.MatrixSingularException => return false
    }
    true
  }

  def calcOmega(Akinv : DenseMatrix[Double]) : DenseVector[Double] = {
    val omega = DenseVector.zeros[Double](Akinv.numRows)
    for(i <- 0 until omega.length) omega(i) = lNorm(Akinv(i,0 until Akinv.numCols))
    omega
  }

  def calcGamma(Ck : DenseMatrix[Double]) : DenseVector[Double] =  {
    val gamma = DenseVector.zeros[Double](Ck.numCols)
    for(j <- 0 until gamma.length) gamma(j) = lNorm(Ck(0 until Ck.numRows,j))
    gamma
  }
  def calcJMax(gamma : DenseVector[Double]) : Int = {
    var jMax = 0
    for(j <- 0 until n-k)
      if(gamma(j) > jMax) jMax = j
    jMax
  }


  def findIJ() : (Int,Int) = {
    //AkBk: k x (n-k)
    for(i <- 0 until AkBk.numRows; j <- 0 until AkBk.numCols){
      //printf("f is %.4f,\t AkBk(%d,%d)=%.4f,\tgamma(%d)*omega(%d) = %f\n",f,i,j,AkBk(i,j),i,j,gamma(j)*omega(i))
      if(goodSwap(i,j))
        return((i,j))
    }
    null
  }
  
  //swaps i and j in R and pvt
  def swapCols(i : Int, j : Int){
    //swap i and j in 
    val tempCol = R(0 until R.numRows,i)
    R(0 until m, i) := R(0 until R.numRows, j) 
    R(0 until m, j) := tempCol
    //swap i and (j) in permutation
    val temp = pvt(i)
    pvt(i) = pvt(j)
    pvt(j) = temp
  }

  def detRatio(i : Int, j : Int) : Double = {
    //sqrt(math.pow(AkBk(i,j),2) + math.pow((gamma(j) * omega(i)),2))
    sqrt(math.pow(AkBk(i,j),2) + math.pow(gamma(j) * omega(i),2))
  }

  def goodSwap(i : Int, j : Int) : Boolean = {
    math.abs(AkBk(i,j)) > f || gamma(j)*omega(i) > f
   /* println("gamma: ")
    println(gamma.asRow)
    println("omega: ")
    println(omega.asRow)
    printf("det ratio(%d,%d) = %f\n",i,j,detRatio(i,j))*/
    //detRatio(i,j) > f

   }
  def goodSwapExists : Boolean = {
    for(i <- 0 until k; j <- 0 until (n-k))
      if(goodSwap(i,j)) return true
    false
   }

  //==================================================

  def alg5(){
    var fail = false
    val p = qrp(A)._3
    R = A*p
    Ck = R
    gamma = calcGamma(Ck)
    while(k <= maxRank 
          && (k == initialK || gamma(calcJMax(gamma)) >= tol)
          && !fail)
    {
      jMax = calcJMax(gamma)
      k += 1
//      printf("k = %d, jMax=%d, gamma.length=%d, r: (%d x %d)\n",k,jMax,gamma.length,R.numRows,R.numCols)
      swapCols(k,k+jMax-1)
      /*    if(k > initialK)
       printf("(outer) k: %d swapping %d, %d: gamma(%d) = %f?\n",k,k,k+jMax, jMax, gamma(jMax))*/
      fail = !update()

      while(!fail && goodSwapExists){
        val (i,j) = findIJ()
        //      printf("(inner) k: %d swapping %d, %d has det ratio %f, good swap? %b\n",k,i,j+k,detRatio(i,j),goodSwap(i,j))
        swapCols(i,j+k-1)
        fail = !update()
      }
    }
    if(fail)
      System.err.println("Failed due to matrix singular exception.")
  }

  def alg4(){
    val q = qrp(A)
    R = q._2
    pvt = new DenseVectorCol[Int](q._4)
    k = maxRank
    var fail = !update(false)
//    println("Does good swap exist? "+ goodSwapExists)
    while(!fail && goodSwapExists){
      val (i,j) = findIJ()
      swapCols(i,j+k-1)
      fail = !update()
    }
    if(fail)
      System.err.println("Failed due to matrix singular exception.")
  }

  if(alg == 4) alg4() else alg5()



  
  
}
