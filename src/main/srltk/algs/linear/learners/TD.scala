package srltk.algs.linear.learners

import srltk.common._
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.dense._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._;
import srltk.common._

class TD(
		val obsDim : Int,
    val alpha: Double,
    val lambda: Double, 
    val gamma: Double, 
    initialValue: Double = 0, 
    beta: Double = 0) extends LearnerV{
  require(lambda == 0 || beta == 0)

  val numFeats = obsDim
  val theta = DenseVector.zeros[Double](numFeats)
  val z = DenseVector.zeros[Double](numFeats)
  
  //for gradient td
  val w = if (beta > 0) DenseVector.zeros[Double](numFeats) else null

  //linear value function
  override def getV(o: Feats): Double = {o dot theta}

  //update value function from observed transition
  def learn(o1: Feats, r1 : Double, a1 : Int, o2: Feats, r2 : Double): Unit = {learn(o1,r1,o2,r2)}
  def learn(o1: Feats, r1 : Double, o2: Feats, r2 : Double) :Unit = {
    //calculate the TD error
    val delta = r2 + gamma * getV(o2) - getV(o1)

    //gradient TD
    if (beta > 0) {
      val d: Double = (o1 dot w)
      w := (o1 * (delta - d) * beta)
      theta += (o1 * delta - o2 * gamma * d) * alpha
    } else {
      //Update z and theta
      if (lambda > 0) {
        z := o1 + z * gamma * lambda
        theta += z * alpha * delta
      } else {
        val ad = alpha * delta
        o1.foreachNonZeroPair((k: Int, v: Double) =>
          theta(k) += v * ad)
      }
    }
  }
  
  def reset() = {
	  z :*= 0
  }
}
