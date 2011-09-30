package srltk.projects.psrs.pomdps
import scalala.tensor.sparse.SparseVector
import java.io.{BufferedOutputStream, File, FileOutputStream}
import srltk.projects.psrs._
import scala.collection.mutable.StringBuilder

class TwoStateHMM extends POMDPSimulator {
  val discount = 0.75
  val stateNames = Nil
  val numStates = 3
  reset()  
  val actionNames = Nil
  val numActions = 1

  val observationNames = Nil
  val numObservations = 2

  def startStateProb(state : Int) = 1d/numStates
  //10% change of self loop, 90% change of circling right
  def T(state1: Int, action: Int, state2: Int) : Double = {
    if     (state1 == 0 && state2 == 1) .9d
    else if(state1 == 0 && state2 == 0) .1d
    else if(state1 == 1 && state2 == 2) .9d
    else if(state1 == 1 && state2 == 1) .1d
    else if(state1 == 2 && state2 == 0) .9d
    else if(state1 == 2 && state2 == 2) .1d
    else 0
  }

  //state 0: 90/10, state 1: 50/50, state2: 10/90
  def O(state1 : Int, action: Int, state2: Int, obs : Int)  : Double ={
    if(state2 == 0 && obs == 0) .9d
    else if(state2 == 0 && obs == 1) .1d
    else if(state2 == 1 && obs == 0) 0.5d
    else if(state2 == 1 && obs == 1) 0.5d
    else if(state2 == 2 && obs == 0) .1d
    else if(state2 == 2 && obs == 1) .9d
    else 0d
  }
  //calculate reward
  def R(state1 : Int, action : Int, state2 : Int, observation : Int) : Double = {
    0
  }
}
