package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

class Paint extends POMDPSimulator {
  val discount = 0.95
  val stateNames = List("nfl-nbl-npa","nfl-nbl-pa","fl-nbl-pa","fl-bl-npa")
  val numStates = stateNames.length
  reset()
  
  val actionNames = List("paint","inspect","ship","reject")
  val numActions = actionNames.length

  val observationNames = List("nbl", "bl")
  val numObservations = observationNames.length

  def startStateProb(state : Int) = if(state==0) .5 else if(state==3) 0.5 else 0
  
  //calculate Pr{state2 | state1+action}
  def T(state1: Int, action: Int, state2: Int) : Double = {
    val a = actionNames(action)
    val s1 = stateNames(state1)
    val s2 = stateNames(state2)
    a match {
      case "paint" => {
        if      (s1 == "nfl-nbl-npa" && s2 == "nfl-nbl-npa")  0.1d
        else if (s1 == "nfl-nbl-npa" && s2 == "nfl-nbl-pa")  0.9d
        else if (s1 == "nfl-nbl-pa" && s2 == "nfl-nbl-pa")  1d
        else if (s1 == "fl-nbl-pa" && s2 == "fl-nbl-pa")  1d
        else if (s1 == "fl-bl-npa" && s2 == "fl-nbl-pa")  0.9d
        else if (s1 == "fl-bl-npa" && s2 == "fl-bl-npa")  0.1d
        else 0d
      }
      case "inspect" => {
        if(state1==state2) 1d else 0d
      }
      case "reject" => {
        startStateProb(state2)
      }
      case "ship" => {
        startStateProb(state2)
      }
      case _ => throw new IllegalArgumentException("Bad action")
    }
  }
  //calculate Pr(o | action -> state)
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    val a = actionNames(action)
    val s = stateNames(state2)
    val o = observationNames(obs)

    val pr :Double = if(a=="inspect") {
      if      (s == "nfl-nbl-npa" && o == "nbl")  0.75d
      else if (s == "nfl-nbl-npa" && o == "bl")  0.25d
      else if (s == "nfl-nbl-pa" && o == "nbl")  .75d
      else if (s == "nfl-nbl-pa" && o == "bl")  .25d
      else if (s == "fl-nbl-pa" && o == "nbl")  .75d
      else if (s == "fl-nbl-pa" && o == "bl")  .25d
      else if (s == "fl-bl-npa" && o == "nbl")  0.25d
      else if (s == "fl-bl-npa" && o == "bl")  0.75d
      else 0d
    } else {
      if(o=="nbl") 1d
      else 0
    } 

    pr
  }
  //calculate reward
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
