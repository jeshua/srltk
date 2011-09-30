package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

class TwoStateMDP extends POMDPSimulator {

  val discount = 0.75
  val stateNames = List("tiger-left", "tiger-right")
  val numStates = stateNames.length
  reset()
  
  val actionNames = List("listen", "open-left", "open-right")
  val numActions = actionNames.length

  val observationNames = List("tiger-left", "tiger-right")
  val numObservations = observationNames.length

  def startStateProb(state : Int) = 1d/numStates
  
  //calculate Pr{state2 | state1+action}
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    a match {
      //identity
      case "listen" => {
        if (state1 == state2) .85d
        else .15d
      }
      //uniform
      case "open-left" => {
        1d / numStates
      }
      case "open-right" => {
        1d / numStates
      }
      case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    if (obs == state2) 1 else 0             
  }
  //calculate reward
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
  0d  
  }
}
