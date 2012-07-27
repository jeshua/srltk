package srltk.algs.linear.learners

import srltk.common._
import scalala.tensor.dense.DenseVector
import srltk.algs.linear.common._

class TD(
		val obs_dim : Int,
    val alpha: Double,
    val lambda: Double, 
    val gamma: Double, 
    initial_value: Double = 0, 
    beta: Double = 0){
  require(lambda == 0 || beta == 0)

  val n_features = obs_dim
  val theta = DenseVector.zeros[Double](n_features)
  val z = DenseVector.zeros[Double](n_features)
  
  //for gradient td
  val w = if (beta > 0) DenseVector.zeros[Double](n_features) else null

  //linear value function
   def getV(o: Vec): Double = {o dot theta}

  //update value function from observed transition
  def learn(o1: Vec, r1 : Double, a1 : Int, o2: Vec, r2 : Double): Unit = {learn(o1,r1,o2,r2)}
  def learn(o1: Vec, r1 : Double, o2: Vec, r2 : Double) :Unit = {
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
