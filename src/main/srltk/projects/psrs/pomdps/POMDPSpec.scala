package srltk.projects.psrs.pomdps

import java.io.Serializable
import java.util.ArrayList
import scalala.tensor.dense._

trait POMDPSpec {
  val discount : Double 
  
  val numStates : Int
  val stateNames : List[String]
  
  val numActions : Int
  val actionNames : List[String]
  
  val numObservations : Int
  val observationNames : List[String]
  
  def startStateProb(state : Int) : Double
  def T(state1 : Int, action : Int, state2 : Int) : Double
  def O(state1 : Int, action : Int, state2 : Int, obs : Int) : Double
  def expectedO(state : Int, action : Int, obs : Int) : Double = {
    (for(s <- 0 until numStates) yield T(state,action,s) * O(state,action,s,obs)).reduceLeft(_ + _ )      
  }
  def R(state1 : Int, action : Int, state2 : Int, obs : Int) : Double


  def getOMat(a : Int, o : Int) : DenseMatrix[Double] = {
    val O = DenseMatrix.zeros[Double](numStates,numStates)
    for(s1 <- 0 until numStates)
      for(s2 <- 0 until numStates)
        O(s1,s2) = this.O(s1,a,s2,o)
    O
  }

  //get transition matrix for action a
  def getTMat(a : Int) : DenseMatrix[Double] = {
    val T = DenseMatrix.zeros[Double](numStates,numStates)
    for(s1 <- 0 until numStates)
      for(s2 <- 0 until numStates)
        T(s1,s2) = this.T(s1,a,s2)
    T
  }

  
} 
