package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

class Chain(val numStates : Int) extends POMDPSimulator {
  val discount = 1d
  val stateNames = Nil
  reset()  
  val actionNames = List("left", "right")
  val numActions = actionNames.length

  val observationNames = Nil
  val numObservations = 2

  def startStateProb(state : Int) = 1d / numStates

  //calculate Pr{state2 | state1+action}
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    val left = if(state1 >= numStates-1) 0 else state1+1
    val right = if(state1 <= 0) numStates-1 else state1-1
    a match {
      case "left" => {
        if(state2==left) 1d
        else 0d
      }
      case "right" => {     
        if(state2==right) 1d
        else 0d
      }
      case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    val a = actionNames(action)
    if(state1 == 0 && obs == 1)
      .9d
    else if(state1 != 0 && obs==0)
      .9d
    else
      .1d
  }
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
