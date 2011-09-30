package srltk.projects.psrs
import srltk.tools.linearalgebra.MiscStats._
import scalala.tensor.dense._
import scalala.tensor._
import scalala.library._
import scalala.library.LinearAlgebra.{pinv,det}
import srltk.vis.FilledContour
import org.jzy3d.maths.Coord3d
import org.jzy3d.plot3d.primitives.Scatter
import org.jzy3d.colors.Color

/**
 * dimensions:
 *    d: dimensions of random variable
 *    n: samples
 *
 * GaussianKDE will learn a KDE with the samples in X as centers
 *  covariance chosen with PCA
 *
 * X is a (d x n) matrix
 */
class GaussianKDE(X: DenseMatrix[Double]) {
  val d = X.numRows
  val n = X.numCols

  //first, mean center X
  //get mean
  val means = rowMeans(X) //mutates X
  for (i <- 0 until n) X(::, i) -= means

  //now SVD
  val svd = LinearAlgebra.svd(X)
  val U = svd._1 //d x d
  val S = diag(svd._2) //d x n
  val Vt = svd._3 //n x n

  //sqrt of eigenvalues
  val sqrtS = S.mapValues((x) => math.sqrt(x))
  val T = sqrtS * U

  //whiten data; x is a dx1 vector data sample
  def whiten(x: DenseVectorCol[Double]): DenseVectorCol[Double] = T * (x - means)

  //create whitened centers
  val centers = X
  for(i <- 0 until n) centers(::,i) := T * (centers(::,i))
  
  //probability of x
  //computes normalized sum of gaussians centered at data points
  def f(x: DenseVectorCol[Double], h : Double, shouldWhiten : Boolean = true) : Double = {
    val wx = if(shouldWhiten) whiten(x) else x
    val data = if(shouldWhiten) centers else X
    
    val cov = DenseMatrix.eye[Double](d,d) :* h
    val invCov = pinv(cov)
    val detCov = det(cov)
    var sum = 0d
    for(i <- 0 until n){//loop over centers
    	val mu : DenseVectorCol[Double] = data(::,i)
    	sum += gaussianPDF(x, mu, invCov, detCov)
    }
    sum / n //normalize and return
  }
}


//======================================================================
//test

object GaussianKDE {
  def main(args : Array[String]) = {
    val size = 100
    
    val rng = new util.Random(3)
    val data = DenseMatrix.zeros[Double](2,size)
    for(i <- 0 until size){      
      data(0,i) = rng.nextGaussian()
      data(1,i) = rng.nextGaussian()
    }
    
    //create kde
    val h = .7d
    val kde = new GaussianKDE(data)
    val tempVec = DenseVectorCol.zeros[Double](2)
    def f(x : Double,y : Double) : Double = {
      tempVec(0) = x
      tempVec(1) = y
      val v = kde.f(tempVec, h)
      v * 100
    }
    //==================================================
    //VISUALIZE
    val xmax = data(0,::).max
    val xmin = data(0,::).min
    val ymax = data(1,::).max
    val ymin = data(1,::).min
    val bounds = (xmin,xmax,ymin,ymax)
	val fc = new FilledContour(bounds,f _)
    
    def createScatter(mat : DenseMatrix[Double], width : Int = 3) : Scatter = {
    	val size = mat.numCols
    	val points = new Array[Coord3d](size)
    	for(i <- 0 until size)
    		points(i) = new Coord3d(mat(0,i),mat(1,i),0)
    	val scatter = new Scatter(points)
    	scatter.setColor(new Color(255,255,255))
    	scatter.setWidth(width)  
    	scatter
    }
    fc.chart.getScene().add(createScatter(data))
  }
}
 