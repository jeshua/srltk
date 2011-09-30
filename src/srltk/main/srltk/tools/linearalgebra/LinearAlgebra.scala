package srltk.tools.linearalgebra
import org.netlib.lapack._
import org.netlib.util.intW
import scalala.library.LinearAlgebra._
import scalala.operators._
import scalala.scalar.Scalar
import scalala.generic.collection.CanViewAsVector
import scalala.tensor.domain.TableDomain
import scalala.tensor.dense.{ DenseVector, DenseMatrix }
import scalala.tensor.{DiagonalMatrix, MatrixSingularException, Matrix, Vector }
import scalala.library._
import scalala.library.Library._


package object LinearAlgebra {

  private def requireNonEmptyMatrix[V](mat: Matrix[V]) = {
    if (mat.numCols == 0 || mat.numRows == 0)
      throw new MatrixEmptyException
  }

  def toDense(d : scalala.tensor.mutable.Vector[Double])  : DenseVector[Double]  = {
    val ret = DenseVector.zeros[Double](d.length)
    ret := d
  
    ret
  }

  def toDense(d : scalala.tensor.mutable.Matrix[Double])  : DenseMatrix[Double]  = {
    val ret = DenseMatrix.zeros[Double](d.numRows,d.numCols)
    ret := d
    ret
  }

  def lNorm(d : Vector[Double], l : Int = 2) : Double = {
    var sum = 0d
    for(i <- 0 until d.length) sum += math.pow(d(i),l)
    math.sqrt(sum)
  }


  def transpose(d : DenseMatrix[Double]) : DenseMatrix[Double] = {
    toDense(d.t)
  }


  /**
   * Find basis
   * 
   * input: A m x n matrix,
   *        rank : rank or -1 to compute it with qr
   * output: C (m x rank) where each column is linearly independent
   */
  def findBasis(A : DenseMatrix[Double], rank : Int = -1, tol : Double = 0.1) 
  : (DenseMatrix[Double],Vector[Int],Vector[Int]) = {
    
    val k : Int = 
      if(rank < 0){
        val R = qrp(A)._2
        var estr = -1
        var l = 1
        while(l < R.numCols && estr < 0){
          if (math.abs(R(l, l)) < tol)
            estr=l
          l += 1
        }
        println("est. rank is "+l)
        estr
      } else rank
    val ret = DenseMatrix.zeros[Double](k,k)
    val At = transpose(A)
    val pvtc = (new StrongRRQR(A,-1,k,4)).pvt(0 until k)
    val pvtr = (new StrongRRQR(At,-1,k,4)).pvt(0 until k)
    for(c <- 0 until k;
        r <- 0 until k)
      ret(r,c) = A(pvtr(r),pvtc(c))
    (ret,pvtc,pvtr)
  }

  //estimate rank
  def estRank(A : DenseMatrix[Double], thresh : Double = 1e-10) : Int = {
    estRankSVD(svd(A)._2,thresh)
  }

  /**
   * Estimate rank using SVD
   * 
   * input: A matrix of interest
   *        D from SVD (singular values)
   *        thresh threshold on singular values values
   * output: estimated rank
   */
  def estRankSVD(D : DenseVector[Double], thresh : Double = 1e-10) : Int = {
    val m = D.length
    val newD = normalize(D,2)
    //    println(newD)
    def r(rank : Int) : Int = {
      if(math.abs(newD(rank)) > thresh && rank < m-1) r(rank+1)
      else rank
    }
    math.max(r(0),1)
  }

  /**
   * Estimate rank using QF Factorization
   * 
   * input: A matrix of interest
   *        R from qr factorization
   *        thresh threshold on R values
   * output: estimated rank
   */
  def estRankQR(R : DenseMatrix[Double], thresh : Double = 1e-10) : Int = {
    val m = math.min(R.numRows,R.numCols)
    val newR = R
    def r(rank : Int) : Int = {
      if(math.abs(newR(rank, rank)) > thresh && rank < m-1) r(rank+1)
      else rank
    }
    math.max(r(0),1)
  }
  /**
   * QR Factorization
   * 
   * input: A m x n matrix
   * optional skipQ - if true, don't calculate orthogonal matrix Q (instead returns (null,R))
   * output: (Q,R) 
   *   Q: m x m
   *   R: m x n 
   */
  def qr(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.numRows
    val n = A.numCols
    val lapack = LAPACK.getInstance();

    //Get optimal workspace size 
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info);
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, math.min(m,n), null, m, null, work, -1, info);
    val lwork2 = if(info.`val` != 0) n else work(0).toInt;
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](math.max(lwork1, lwork2));
    
    //Perform the QR factorization with dgeqrf
    val maxd = math.max(m,n)
    val mind = math.max(m,n)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    val tau = new Array[Double](math.min(m,n))
    for(r <- 0 until m; c <- 0 until n) outputMat(r,c) = A(r,c)
    lapack.dgeqrf(m, n, outputMat.data, m, workspace, tau, workspace.length, info);
    
    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()
    
    //Get R
    val R = DenseMatrix.zeros[Double](m,n)
    for(c <- 0 until maxd if(c < n);
        r <- 0 until m if(r <= c)) 
      R(r,c) = outputMat(r,c)    

    //unless the skipq flag is set
    if(!skipQ){
      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,m)    
      lapack.dorgqr(m, m, math.min(m,n), outputMat.data, m, tau, workspace, workspace.length, info);
      for(r <- 0 until m;
          c <- 0 until maxd if(c < m)) 
        Q(r,c) = outputMat(r,c)
      
      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()
      (Q,R)
    }
    //skip Q and just return R
      else (null,R)
  }

  /**
  * QR Factorization with pivoting
  * 
  * input: A m x n matrix
  * output: (Q,R,P,pvt) where AP = QR  
  *   Q: m x m
  *   R: m x n 
  *   P: n x n : permutation matrix (P(pvt(i),i) = 1)
  *   pvt : n x 1: pivot indices
  */
  def qrp(A: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Int], DenseVector[Int]) = {
    val m = A.numRows
    val n = A.numCols
    val lapack = LAPACK.getInstance();

    //Get optimal workspace size 
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info);
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, math.min(m,n), null, m, null, work, -1, info);
    val lwork2 = if(info.`val` != 0) n else work(0).toInt;
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](math.max(lwork1, lwork2));
    
    //Perform the QR factorization with dgep3
    val maxd = math.max(m,n)
    val mind = math.max(m,n)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    val pvt = DenseVector.zeros[Int](n)
    val tau = new Array[Double](math.min(m,n))
    for(r <- 0 until m; c <- 0 until n) outputMat(r,c) = A(r,c)
    lapack.dgeqp3(m, n, outputMat.data, m, pvt.data, tau, workspace, workspace.length, info);
    
    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()
    
    //Get R
    val R = DenseMatrix.zeros[Double](m,n)
    for(c <- 0 until maxd if(c < n);
        r <- 0 until m if(r <= c)) 
      R(r,c) = outputMat(r,c)    
    
    //Get Q from the matrix returned by dgep3
    val Q = DenseMatrix.zeros[Double](m,m)    
    lapack.dorgqr(m, m, math.min(m,n), outputMat.data, m, tau, workspace, workspace.length, info);
    for(r <- 0 until m;
        c <- 0 until maxd if(c < m)) 
      Q(r,c) = outputMat(r,c)
    
    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()
    
    //Get P
    for(i <- 0 until pvt.length) pvt(i)-=1
    val P = DenseMatrix.zeros[Int](n,n)
    for(i <- 0 until n)
      P(pvt(i), i) = 1                
    
    (Q,R,P,pvt)
  }



  /**
  * Input: R: m x n matrix returned by QR decomposition
  * 
  * Return R= Ak Bk
  *           0  Ck
  *
  * with Ak : kxk
  *      Bk : k x (n-k)
  *      Ck : (m-k)(n-k)
  * */
  def qrDecomposeR(R : DenseMatrix[Double], k : Int)
  : (DenseMatrix[Double],DenseMatrix[Double],DenseMatrix[Double]) =
    {
      val m = R.numRows
      val n = R.numCols
      //k x k
      val Ak = DenseMatrix.zeros[Double](k,k)
      Ak := R(0 until k, 0 until k)
      //k x n-k
      val Bk = DenseMatrix.zeros[Double](k,n-k)
      Bk := R(0 until k, k until n)
      //m-k x n-k
      val Ck = DenseMatrix.zeros[Double](m-k,n-k)
      Ck := R(k until m, k until n)

      (Ak,Bk,Ck)
    }


}
